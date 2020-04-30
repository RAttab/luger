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

args() ->
    {ok, AppName} = application:get_env(luger, app_name),
    HostName = luger_utils:hostname(),
    MaxLen = application:get_env(luger, max_msg_len, 2048),
    #config{app = luger_utils:appname(AppName),
            host = HostName,
            max_msg_len = MaxLen }.

stderr_args() ->
    MinPriority = case application:get_env(luger, stderr_min_priority) of
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
    #stderr_config{min_priority = MinPriority}.

syslog_udp_args() ->
    Host = case application:get_env(luger, syslog_udp_host) of
               {ok, H} when is_tuple(H) -> H
           end,
    Port = case application:get_env(luger, syslog_udp_port) of
               {ok, P} when is_integer(P) -> P
           end,
    Facility = case application:get_env(luger, syslog_udp_facility) of
                   {ok, F} when is_integer(F) andalso F >= 0 andalso F =< 23 -> F
               end,
    #syslog_udp_config{host = Host, port = Port, facility = Facility}.

init([]) ->
    Args = args(),
    SinkArgs = case application:get_env(luger, type) of
               undefined -> stderr_args();
               {ok, null} -> #null_config {};
               {ok, stderr} -> stderr_args();
               {ok, syslog_udp} -> syslog_udp_args()
           end,

    SingleLine = application:get_env(luger, single_line, false),
    ThrottleThreshold =  application:get_env(luger, throttle_threshold, 5),
    Children = [{luger, {luger, start_link, [Args, SinkArgs, SingleLine, ThrottleThreshold]}, permanent, 1000, worker, [luger]}],
    {ok, {{one_for_one, 0, 1}, Children}}.
