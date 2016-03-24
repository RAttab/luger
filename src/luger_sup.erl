-module(luger_sup).


%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------

-export([start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%-----------------------------------------------------------------
%% supervisor callbacks
%%-----------------------------------------------------------------

-behaviour(supervisor).
-export([init/1]).

init([]) ->
    Args = case application:get_env(type) of
               undefined ->
                   stderr;
               {ok, syslog_stderr} ->
                   stderr;
               {ok, syslog_udp} ->
                   {ok, Host} = application:get_env(syslog_udp_host),
                   {ok, Port} = application:get_env(syslog_udp_port),
                   {syslog_udp, Host, Port}
           end,

    Children = [{luger, {luger, start_link, Args}, permanent, 1000, worker, [luger]}],
    {ok, {one_for_one, 0, 1}, Children}.
