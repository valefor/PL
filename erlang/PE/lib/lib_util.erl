-module(lib_util).
-export([debug/2]).

debug(Format,Msg) ->
    io:format("DEBUG:"),
    io:format(Format,Msg).

