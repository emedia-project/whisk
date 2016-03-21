-module(whisk).
-behaviour(gen_server).
-include("../include/whisk.hrl").
-define(SERVER, ?MODULE).
-define(TCP_OPTS, [
                   binary, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, true}
                  ]).
-define(TIMEOUT, 5000).
-define(RECONNECT, false).
-record(server, {host, 
                 port, 
                 socket, 
                 reconnect = ?RECONNECT,
                 timeout = ?TIMEOUT}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/3,
         connect/2,
         connect/3,
         disconnect/1,
         timeout/2,
         version/1,
         stat/1,
         stats/1,
         stats/2,
         set/3,
         set/4,
         get/2,
         delete/2
        ]).

-define(DO(Pid, Fun), 
        if
          is_pid(Pid) ->
            case erlang:process_info(Pid) of
              undefined -> {error, disconnected};
              _ -> Fun
            end;
          true ->
            {error, invalid_client}
        end).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

% @hidden
start_link(Host, Port, Options) when is_list(Host), is_integer(Port), is_list(Options) ->
  gen_server:start_link(?MODULE, [Host, Port, Options], []).

% @equiv connect(Host, Port, [])
-spec connect(string(), integer()) -> {ok, pid()} | {error, term()}.
connect(Host, Port) when is_list(Host), is_integer(Port) ->
  start_link(Host, Port, []).

% @doc
% Connect to a server.
% @end
-spec connect(string(), integer(), [{atom(), term()}]) -> {ok, pid()} | {error, term()}.
connect(Host, Port, Options) when is_list(Host), is_integer(Port), is_list(Options) ->
  start_link(Host, Port, Options).

% @doc
% Disconnect from the server.
% @end
-spec disconnect(pid()) -> ok | {error, term()}.
disconnect(Pid) ->
  ?DO(Pid, gen_server:cast(Pid, stop)).

% @doc
% Set socker timeout
% @end
-spec timeout(pid, integer()) -> ok | {error, term()}.
timeout(Pid, Timeout) -> 
  if
    is_integer(Timeout) ->
      ?DO(Pid, gen_server:call(Pid, {timeout, Timeout}));
    true ->
      {error, invalid_timeout}
  end.

% @doc
% Version of the memcache server.
% @end
-spec version(pid()) -> {ok, binary()} | {error, term()}.
version(Pid) ->
  ?DO(Pid, gen_server:call(Pid, version)).

% @deprecated Please use {@link whisk:stats/1}
-spec stat(pid()) -> {ok, [{binary(), binary()}]} | {error, term()}.
stat(Pid) ->
  stats(Pid).
% @doc
% Collect the stats for the server.
% @end
-spec stats(pid()) -> {ok, [{binary(), binary()}]} | {error, term()}.
stats(Pid) ->
  ?DO(Pid, gen_server:call(Pid, {stat, []})).

% @hidden 
% Experimental
stats(Pid, Extra) ->
  ?DO(Pid, gen_server:call(Pid, {stat, Extra})).

% @equiv set(Key, Value, [{expiry, 0}, {cas, 0}])
-spec set(pid(), term(), term()) -> {ok, integer()} | {error, term()}.
set(Pid, Key, Value) ->
  set(Pid, Key, Value, [{expiry, 0}, {cas, 0}]).
% @doc
% Set the key-value pair
% @end
-spec set(pid(), term(), term(), [{atom(), term()}]) -> {ok, integer()} | {error, term()}.
set(Pid, Key, Value, Options) ->
  if
    is_list(Options) ->
      ?DO(Pid, gen_server:call(Pid, {set, Key, Value, Options}));
    true ->
      {error, invalid_options}
  end.

% @doc
% Get the value associated with the key.
% @end
-spec get(pid(), term()) -> {ok, term()} | {error, term()}.
get(Pid, Key) ->
  ?DO(Pid, gen_server:call(Pid, {get, Key})).

% @doc
% Delete a key/value pair
% @end
-spec delete(pid(), term()) -> ok | {error, term()}.
delete(Pid, Key) ->
  ?DO(Pid, gen_server:call(Pid, {delete, Key})).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

% @hidden
init([Host, Port, Options]) ->
  case gen_tcp:connect(Host, Port, ?TCP_OPTS) of
    {ok, Socket} -> {ok, #server{host = Host,
                                 port = Port,
                                 socket = Socket,
                                 reconnect = buclists:keyfind(reconnect, 1, Options, ?RECONNECT),
                                 timeout = buclists:keyfind(timeout, 1, Options, ?TIMEOUT)}};
    {error, Reason} -> 
      {stop, Reason};
    _ -> {error, connection_faild}
  end.

% @hidden
handle_call({timeout, Timeout}, _From, Server) ->
  {reply, ok, Server#server{timeout = Timeout}};
handle_call(version, _From, Server) ->
  {Response, Server1} = send(Server, whisk_operation:request(?OP_VERSION)),
  {reply, Response, Server1};
handle_call({stat, Key}, _From, Server) ->
  {Response, Server1} = send(multi, ?OP_STAT, Server, whisk_operation:request(?OP_STAT, [{key, bucs:to_binary(Key)}])),
  {reply, Response, Server1};
handle_call({set, Key, Value, Options}, _From, Server) ->
  Expiry = buclists:keyfind(expiry, 1, Options, 0),
  CAS = buclists:keyfind(cas, 1, Options, 0),
  {Flags, Value1} = if
                      is_binary(Value) -> {0, Value};
                      true -> {1, term_to_binary(Value)}
                    end,
  Extra = <<Flags:32, Expiry:32>>,
  {Response, Server1} = send(Server, whisk_operation:request(?OP_SET, [{extra, Extra},
                                                                       {key, bucs:to_binary(Key)},
                                                                       {value, Value1},
                                                                       {cas, CAS}])),
  {reply, Response, Server1};
handle_call({get, Key}, _From, Server) ->
  {Response, Server1} = send(Server, whisk_operation:request(?OP_GET, [{key, bucs:to_binary(Key)}])),
  {reply, Response, Server1};
handle_call({delete, Key}, _From, Server) ->
  {Response, Server1} = send(Server, whisk_operation:request(?OP_DELETE, [{key, bucs:to_binary(Key)}])),
  {reply, Response, Server1};
handle_call(_Request, _From, Server) ->
  {reply, ok, Server}.

% @hidden
handle_cast(stop, Server) ->
  {stop, normal, Server};
handle_cast(_Msg, Server) ->
  {noreply, Server}.

% @hidden
handle_info(_Info, Server) ->
  {noreply, Server}.

% @hidden
terminate(_Reason, _Server) ->
  ok.

% @hidden
code_change(_OldVsn, Server, _Extra) ->
  {ok, Server}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send(Server, Request) ->
  send(simple, 0, Server, Request).

send(Mode, Operation, #server{timeout = Timeout} = State, Request) ->
  case check_connection(State) of
    {ok, State1, Socket1} ->
      case gen_tcp:send(Socket1, Request) of
        ok -> 
          if 
            Mode =:= multi -> {multi_recv(Timeout, Operation, <<>>, []), State1};
            true -> {simple_recv(Timeout, Operation, <<>>), State1}
          end;
        E -> {E, State1}
      end;
    E -> E
  end.

check_connection(#server{socket = Socket, reconnect = Reconn, host = Host, port = Port} = State) ->
  case erlang:port_info(Socket) of 
    undefined -> 
      if
        Reconn -> 
          case gen_tcp:connect(Host, Port, ?TCP_OPTS) of
            {ok, NewSocket} -> {ok, State#server{socket = NewSocket}, NewSocket};
            {error, Reason} -> {{error, Reason}, State}
          end;
        true ->
          {{error, connection_close}, State}
      end;
    _ ->
      {ok, State, Socket}
  end.

simple_recv(Timeout, Operation, Acc) ->
  receive
    {tcp, _, Data} ->
      Data1 = <<Acc/binary, Data/binary>>,
      case whisk_operation:response(Data1) of
        {ok, Response, _} -> Response;
        _ -> simple_recv(Timeout, Operation, Data1)
      end;
    {error, E} ->
      {error, E}
  after Timeout -> 
          {error, timeout}
  end.

multi_recv(Timeout, Operation, Acc, Resp) ->
  receive
    {tcp, _, Data} ->
      Data1 = <<Acc/binary, Data/binary>>,
      case flow_data(Operation, Data1, Resp) of
        {stop, Resp1} -> 
          {ok, Resp1};
        {next, Rest, Resp1} ->
          multi_recv(Timeout, Operation, Rest, Resp1)
      end;
    {error, E} ->
      {error, E}
  after Timeout ->
          {error, timeout}
  end.

flow_data(Operation, Data, Acc) ->
  case whisk_operation:is_end(Operation, Data) of
    true -> {stop, Acc};
    false -> 
      case whisk_operation:response(Data) of
        {ok, {ok, Response}, Rest} -> flow_data(Operation, Rest, [Response|Acc]);
        _ -> {next, Data, Acc}
      end
  end.

