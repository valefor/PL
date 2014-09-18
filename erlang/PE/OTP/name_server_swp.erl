-module(name_server_swp).
-export([init/0,add/1,whereis/1,handle/2,all_names/0,delete/1]).
-import(server,[rpc/2]).
-import(lib_util,[debug/2]).

all_names()         -> rpc(name_server,allNames).
delete([Name])      -> rpc(name_server,{delete,Name}).
add([Name,Place])   -> rpc(name_server,{add,Name,Place}).
whereis([Name])     -> rpc(name_server,{whereis,Name}).

init() -> dict:new().

handle(allNames,Dict)           -> {dict:fetch_keys(Dict),Dict};
handle({delete,Name},Dict)      -> {ok,dict:erase(Name,Dict)};
handle({add,Name,Place},Dict)   -> {ok,dict:store(Name,Place,Dict)};
handle({whereis,Name},Dict)     -> {dict:find(Name,Dict),Dict}.

