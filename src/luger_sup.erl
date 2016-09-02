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
    Statsd = case application:get_env(luger, statsd) of
                 {ok, Value} when is_boolean(Value) -> Value;
                 undefined -> false
             end,
    #config{app = luger_utils:appname(AppName),
            host = HostName,
            statsd = Statsd }.

stderr_args() ->
    MsgCap = application:get_env(luger, stderr_msg_cap, undefined),
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
    #stderr_config{min_priority = MinPriority, msg_cap = MsgCap}.

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
               {ok, stderr} -> stderr_args();
               {ok, syslog_udp} -> syslog_udp_args()
           end,

    Children = [{luger, {luger, start_link, [Args, SinkArgs]}, permanent, 1000, worker, [luger]}],
    {ok, {{one_for_one, 0, 1}, Children}}.
