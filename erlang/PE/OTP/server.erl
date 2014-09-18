-module(server).
-export([start/1,rpc/2,swap_code/1]).
-import(lib_util,[debug/2]).

start([Name,Mod]) ->
    register(Name,spawn(fun() -> loop(Name,Mod,Mod:init()) end )).

swap_code([Name,Mod]) -> rpc(Name,{swap_code,Mod}).

rpc(Name,Request) ->
    debug("Invoke RPC:~p~n",[Request]),
    Name ! {self(),Request},
    receive
        {Name,crash}        -> exit(rpc);
        {Name,swap_ack}     -> io:format("Name server has been swaped dynamically~n");
        {Name,ok,Response}  -> io:format("Response:~p~n",[Response])
    end.

%% Here maybe a bug in try ... of block
%%              try Mod:handle(Request,OldState) of
%%                  {Response,NewState} ->
%%                      //Can't reach HERE

%% a loop is a server
%% Mark:CB = CallBack
loop(Name,Mod,OldState) ->
    receive
        {From,{swap_code,NewCBMod}} ->
            From ! {Name,swap_ack},
            loop(Name,NewCBMod,OldState);
        {From,Request} ->
            try 
                {Response,NewState} = Mod:handle(Request,OldState),
                From ! {Name,ok,Response},
                loop(Name,Mod,NewState)
            catch
                _:Why ->
                    log_the_error(Name,Request,Why),
                    From ! {Name,crash},
                    loop(Name,Mod,OldState)
            end
    end.

log_the_error(Name,Request,Why) ->
    io:format("Server ~p request ~p~n caused exception ~p~n",[Name,Request,Why]).

generate_ex(1) -> {"ok","hello"};
generate_ex(2) -> throw(a);
generate_ex(3) -> exit(a);
generate_ex(4) -> {'EXIT',a};
generate_ex(5) -> erlang:error(a).

