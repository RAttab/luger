-module(luger_app).


%%-----------------------------------------------------------------
%% application callback
%%-----------------------------------------------------------------

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    luger_sup:start_link().

stop(_State) ->
    ok.
