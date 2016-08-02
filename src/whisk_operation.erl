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
  Extra = bucs:to_binary(buclists:keyfind(extra, 1, Datas, <<>>)),
  Key = bucs:to_binary(buclists:keyfind(key, 1, Datas, <<>>)),
  Value = bucs:to_binary(buclists:keyfind(value, 1, Datas, <<>>)),
  Opaque = buclists:keyfind(opaque, 1, Datas, 0),
  CAS = buclists:keyfind(cas, 1, Datas, 0),
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
           Version:Size/binary,
           Rest/binary>>) ->
  {ok, {ok, Version}, Rest};
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
           Value/binary>>) when size(Key) + size(Value) >= Size ->
  ValueLength = Size - KeyLength,
  <<Value1:ValueLength/binary, Rest/binary>> = Value,
  {ok, {ok, {Key, Value1}}, Rest};
% Set
response(<<?RESPONSE,
           ?OP_SET,
           0:16,
           0,
           0,
           ?STATUS_NO_ERROR:16,
           0:32,
           0:32,
           CAS:64,
           Rest/binary>>) ->
  {ok, {ok, CAS}, Rest};
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
           Body/binary>>) when ExtraLength =:= 4, size(Body) >= BodyLength - ExtraLength ->
  BodyLength1 = BodyLength - ExtraLength,
  <<Body1:BodyLength1/binary, Rest/binary>> = Body,
  if
    Flags =:= 1 -> {ok, {ok, binary_to_term(Body1)}, Rest};
    true -> {ok, {ok, Body1}, Rest}
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
           0:64,
           Rest/binary>>) ->
  {ok, ok, Rest};
% Error
response(<<?RESPONSE,
           _,
           0:16,
           0,
           0,
           Status:16,
           0:32,
           0:32,
           _:64,
           Rest/binary>>) when Status =/= ?STATUS_NO_ERROR ->
  {ok, {error, Status}, Rest};
response(_) ->
  continue.

