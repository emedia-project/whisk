% @hidden
-module(whisk_operation).
-include("../include/whisk.hrl").

-export([
         request/1,
         request/2,
         is_end/2,
         response/1
        ]).

request(Operation) ->
  request(Operation, []).

request(Operation, Datas) ->
  Extra = eutils:to_binary(elists:keyfind(extra, 1, Datas, <<>>)),
  Key = eutils:to_binary(elists:keyfind(key, 1, Datas, <<>>)),
  Value = eutils:to_binary(elists:keyfind(value, 1, Datas, <<>>)),
  Opaque = elists:keyfind(opaque, 1, Datas, 0),
  CAS = elists:keyfind(cas, 1, Datas, 0),
  <<?REQUEST, 
    Operation, 
    (size(Key)):16, 
    (size(Extra)), 
    0, 
    0:16, 
    (size(Extra)+size(Key)+size(Value)):32,
    Opaque:32,
    CAS:64,
    Extra/binary,
    Key/binary,
    Value/binary>>.

is_end(Operation, <<?RESPONSE,
                    Operation,
                    0:16,
                    0,
                    0,
                    ?STATUS_NO_ERROR:16,
                    0:32,
                    0:32,
                    0:64>>) ->
  true;
is_end(_, _) ->
  false.

% Version
response(<<?RESPONSE,
           ?OP_VERSION,
           0:16,
           0,
           0,
           ?STATUS_NO_ERROR:16,
           Size:32,
           0:32,
           0:64,
           Version/binary>>) when size(Version) =:= Size ->
  {ok, Version};
% Stat
response(<<?RESPONSE,
           ?OP_STAT,
           KeyLength:16,
           0,
           0,
           ?STATUS_NO_ERROR:16,
           Size:32,
           0:32,
           0:64,
           Key:KeyLength/binary,
           Value/binary>>) when size(Key) + size(Value) =:= Size ->
  {ok, {Key, Value}};
% Set
response(<<?RESPONSE,
           ?OP_SET,
           0:16,
           0,
           0,
           ?STATUS_NO_ERROR:16,
           0:32,
           0:32,
           CAS:64>>) ->
  {ok, CAS};
% Get
response(<<?RESPONSE,
           ?OP_GET,
           0:16,
           ExtraLength,
           0,
           ?STATUS_NO_ERROR:16,
           BodyLength:32,
           0:32,
           _:64,
           Flags:32,
           Body/binary>>) when ExtraLength =:= 4, size(Body) =:= BodyLength - ExtraLength ->
  if
    Flags =:= 1 -> {ok, binary_to_term(Body)};
    true -> {ok, Body}
  end;
% Delete
response(<<?RESPONSE,
           ?OP_DELETE,
           0:16,
           0,
           0,
           ?STATUS_NO_ERROR:16,
           0:32,
           0:32,
           0:64>>) ->
  ok;
% Error
response(<<?RESPONSE,
           _,
           0:16,
           0,
           0,
           Status:16,
           0:32,
           0:32,
           _:64>>) when Status =/= ?STATUS_NO_ERROR ->
  {error, Status};
response(Data) when is_list(Data) ->
  lists:foldl(fun
                (D, {ok, Acc}) ->
                  case response(D) of
                    {ok, Response} -> {ok, [Response|Acc]};
                    {error, E} -> {error, E}
                  end;
                (_, Acc) -> Acc
              end, {ok, []}, Data);
response(_) ->
  {error, invalid_response}.

