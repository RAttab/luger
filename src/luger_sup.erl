-module(luger_sup).
-include("luger.hrl").

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

stderr_args() ->
    MinPriority = case application:get_env(stderr_min_priority) of
                      undefined -> ?WARNING;
                      {ok, emergency} -> ?EMERGENCY;
                      {ok, alert} -> ?ALERT;
                      {ok, critical} -> ?CRITICAL;
                      {ok, error} -> ?ERROR;
                      {ok, warning} -> ?WARNING;
                      {ok, notice} -> ?NOTICE;
                      {ok, info} -> ?INFO;
                      {ok, debug} -> ?DEBUG
                      end,
    [stderr, MinPriority].

syslog_udp_args() ->
    {ok, Host} = application:get_env(syslog_udp_host),
    {ok, Port} = application:get_env(syslog_udp_port),
    [syslog_udp, Host, Port].

init([]) ->
    Args = case application:get_env(type) of
               undefined -> stderr_args();
               {ok, stderr} -> stderr_args();
               {ok, syslog_udp} -> syslog_udp_args()
           end,

    {ok, AppName} = application:get_env(app_name),
    Args0 = [AppName] ++ Args,

    Children = [{luger, {luger, start_link, Args0}, permanent, 1000, worker, [luger]}],
    {ok, {{one_for_one, 0, 1}, Children}}.
