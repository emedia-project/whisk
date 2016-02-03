-module(whisk).
-behaviour(gen_server).
-include("../include/whisk.hrl").
-define(SERVER, ?MODULE).
-define(TCP_OPTS, [
                   binary, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, true}
                  ]).
-define(TIMEOUT, 5000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/2,
         connect/2,
         disconnect/0,
         version/0,
         stat/0,
         stats/0,
         set/2,
         set/3,
         get/1,
         delete/1
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

% @hidden
start_link(Host, Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port], []).

% @doc
% Connect to a server.
% @end
-spec connect(string(), integer()) -> {ok, pid()} | {error, term()}.
connect(Host, Port) ->
  start_link(Host, Port).

% @doc
% Disconnect from the server.
% @end
-spec disconnect() -> ok.
disconnect() ->
  gen_server:cast(?SERVER, stop).

% @doc
% Version of the memcache server.
% @end
-spec version() -> {ok, binary()}.
version() ->
  gen_server:call(?SERVER, version).

% @deprecated
-spec stat() -> {ok, [{binary(), binary()}]}.
stat() ->
  stats().
% @doc
% Collect the stats for the server.
% @end
-spec stats() -> {ok, [{binary(), binary()}]}.
stats() ->
  gen_server:call(?SERVER, stat).

% @equiv set(Key, Value, [{expiry, 0}, {cas, 0}])
-spec set(term(), term()) -> {ok, integer()}.
set(Key, Value) ->
  set(Key, Value, [{expiry, 0}, {cas, 0}]).
% @doc
% Set the key-value pair
% @end
-spec set(term(), term(), [{atom(), term()}]) -> {ok, integer()}.
set(Key, Value, Options) ->
  gen_server:call(?SERVER, {set, Key, Value, Options}).

% @doc
% Get the value associated with the key.
% @end
-spec get(term()) -> {ok, term()} | {error, term()}.
get(Key) ->
  gen_server:call(?SERVER, {get, Key}).

% @doc
% Delete a key/value pair
% @end
-spec delete(term()) -> ok | {error, term()}.
delete(Key) ->
  gen_server:call(?SERVER, {delete, Key}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

% @hidden
init([Host, Port]) ->
  case gen_tcp:connect(Host, Port, ?TCP_OPTS) of
    {ok, Socket} -> {ok, Socket};
    {error, Reason} -> {stop, Reason};
    _ -> {error, connection_faild}
  end.

% @hidden
handle_call(version, _From, Socket) ->
  Response = case send(Socket, whisk_operation:request(?OP_VERSION)) of
    {ok, Data} -> whisk_operation:response(Data);
    E -> E
  end,
  {reply, Response, Socket};
handle_call(stat, _From, Socket) ->
  Response = case send(multi, ?OP_STAT, Socket, whisk_operation:request(?OP_STAT)) of
    {ok, Data} -> whisk_operation:response(Data);
    E -> E
  end,
  {reply, Response, Socket};
handle_call({set, Key, Value, Options}, _From, Socket) ->
  Expiry = buclists:keyfind(expiry, 1, Options, 0),
  CAS = buclists:keyfind(cas, 1, Options, 0),
  {Flags, Value1} = if
                      is_binary(Value) -> {0, Value};
                      true -> {1, term_to_binary(Value)}
                    end,
  Extra = <<Flags:32, Expiry:32>>,
  Response = case send(Socket, whisk_operation:request(?OP_SET, [{extra, Extra},
                                                                 {key, bucs:to_binary(Key)},
                                                                 {value, Value1},
                                                                 {cas, CAS}])) of
               {ok, Data} -> whisk_operation:response(Data);
               E -> E
             end,
  {reply, Response, Socket};
handle_call({get, Key}, _From, Socket) ->
  Response = case send(Socket, whisk_operation:request(?OP_GET, [{key, bucs:to_binary(Key)}])) of
               {ok, Data} -> whisk_operation:response(Data);
               E -> E
             end,
  {reply, Response, Socket};
handle_call({delete, Key}, _From, Socket) ->
  Response = case send(Socket, whisk_operation:request(?OP_DELETE, [{key, bucs:to_binary(Key)}])) of
               {ok, Data} -> whisk_operation:response(Data);
               E -> E
             end,
  {reply, Response, Socket};
handle_call(_Request, _From, Socket) ->
  {reply, ok, Socket}.

% @hidden
handle_cast(stop, Socket) ->
  {stop, normal, Socket};
handle_cast(_Msg, Socket) ->
  {noreply, Socket}.

% @hidden
handle_info(_Info, Socket) ->
  {noreply, Socket}.

% @hidden
terminate(_Reason, _Socket) ->
  ok.

% @hidden
code_change(_OldVsn, Socket, _Extra) ->
  {ok, Socket}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send(Socket, Request) ->
  send(simple, 0, Socket, Request).

send(Mode, Operation, Socket, Request) ->
  case gen_tcp:send(Socket, Request) of
    ok -> 
      if 
        Mode =:= multi -> multi_recv(Operation);
        true -> simple_recv()
      end;
    E -> E
  end.

simple_recv() ->
  receive
    {tcp, _, Data} ->
      {ok, Data};
    {error, E} ->
      {error, E}
  after ?TIMEOUT -> 
          {error, timeout}
  end.

multi_recv(Operation) ->
  multi_recv(Operation, []).

multi_recv(Operation, Data) ->
  receive
    {tcp, _, NewData} ->
      case whisk_operation:is_end(Operation, NewData) of
        true -> {ok, Data};
        false -> multi_recv(Operation, [NewData|Data])
      end;
    {error, E} ->
      {error, E}
  after ?TIMEOUT ->
          {error, timeout}
  end.
