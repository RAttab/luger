-module(luger_sup).

-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%-----------------------------------------------------------------
%% supervisor callbacks
%%-----------------------------------------------------------------

init([]) ->
    {ok, AppName} = application:get_env(app_name),
    Args = case application:get_env(type) of
               undefined ->
                   [stderr];
               {ok, stderr} ->
                   [stderr];
               {ok, syslog_udp} ->
                   {ok, Host} = application:get_env(syslog_udp_host),
                   {ok, Port} = application:get_env(syslog_udp_port),
                   [syslog_udp, Host, Port]
           end,

    Children = [{luger, {luger, start_link, [AppName] ++ Args}, permanent, 1000, worker, [luger]}],
    {ok, {{one_for_one, 0, 1}, Children}}.
