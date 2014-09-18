-module(my_bank).
-behaviour(gen_server).
-import(lib_util,[debug/2]).
-export([start/0]).

%% gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-compile(export_all).

start() -> gen_server:start_link({local,?MODULE},?MODULE,[],[]).
stop() -> gen_server:call(?MODULE,stop).

%io:format("Type of ~p:~p~n",[Amount,is_atom(Amount)]),
new_account([Who])        -> gen_server:call(?MODULE,{new,Who}).
deposit([Who,Amount])     -> gen_server:call(?MODULE,{add,Who,Amount}).
with_draw([Who,Amount])   -> gen_server:call(?MODULE,{remove,Who,Amount}).

init([]) -> {ok,ets:new(?MODULE,[])}.

handle_call({new,Who},_From,Tab) ->
    Reply = case ets:lookup(Tab,Who) of
        []  -> ets:insert(Tab,{Who,0}),
            {welcome,Who};
        [_] -> {Who,you_already_are_a_customer}
    end,
    debug("Reply:~p~n",[Reply]),
    {reply,Reply,Tab};
handle_call({add,Who,X},_From,Tab) ->
    if is_integer(X) == false ->
            Amount = list_to_integer(atom_to_list(X));
        true ->
            Amount = X
    end,
    Reply = case ets:lookup(Tab,Who) of
        []  -> not_a_customer;
        [{Who,Balance}] ->
                NewBalance = Balance + Amount,
                ets:insert(Tab,{Who,NewBalance}),
                {thanks,Who,your_balacne_is,NewBalance}
        end,
    debug("Reply:~p~n",[Reply]),
    {reply,Reply,Tab};
handle_call({remove,Who,X},_From,Tab) ->
    if is_integer(X) == false ->
            Amount = list_to_integer(atom_to_list(X));
        true ->
            Amount = X
    end,
    Reply = case ets:lookup(Tab,Who) of
        []  -> not_a_customer;
        [{Who,Balance}] when Amount =< Balance ->
            NewBalance = Balance - Amount,
            ets:insert(Tab,{Who,NewBalance}),
            {thanks,Who,your_balance_is,NewBalance};
        [{Who,Balance}] ->
            {sorry,Who,you_only_have,Balance,in_the_bank}
    end,
    debug("Reply:~p~n",[Reply]),
    {reply,Reply,Tab};
handle_call(stop,_From,Tab) ->
    {stop,normal,stopped,Tab}.

handle_cast(_Msg,State) -> {noreply,State}.
handle_info(_Info,State) -> {noreply,State}.
terminate(_Reason,_State) -> ok.
code_change(_OldVsn,State,_Extra) -> {ok,State}.

