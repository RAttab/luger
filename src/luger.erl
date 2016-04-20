-module(luger).
-include("luger.hrl").

-export([start_link/2,
         emergency/2, emergency/3,
         alert/2, alert/3,
         critical/2, critical/3,
         error/2, error/3,
         warning/2, warning/3,
         notice/2, notice/3,
         info/2, info/3,
         debug/2, debug/3
        ]).

-behaviour(supervisor_bridge).
-export([init/1, terminate/2]).

-define(DEFAULT_PRIORITY, ?BASE_PRIORITY + 4).
-define(PROC_NAME, luger).
-define(TABLE, luger).


%%-----------------------------------------------------------------
%% public interface
%%-----------------------------------------------------------------

-spec start_link(tuple(), tuple()) -> {ok, pid()} | {error, any()}.
start_link(Config, SinkConfig) ->
    supervisor_bridge:start_link(?MODULE, [Config, SinkConfig]).

-spec emergency(string(), string()) -> ok | {error, luger_not_running}.
emergency(Channel, Message) ->
    log(?EMERGENCY, Channel, Message).

-spec emergency(string(), string(), [any()]) -> ok | {error, luger_not_running}.
emergency(Channel, Format, Args) ->
    emergency(Channel, io_lib:format(Format, Args)).

-spec alert(string(), string()) -> ok | {error, luger_not_running}.
alert(Channel, Message) ->
    log(?ALERT, Channel, Message).

-spec alert(string(), string(), [any()]) -> ok | {error, luger_not_running}.
alert(Channel, Format, Args) ->
    alert(Channel, io_lib:format(Format, Args)).

-spec critical(string(), string()) -> ok | {error, luger_not_running}.
critical(Channel, Message) ->
    log(?CRITICAL, Channel, Message).

-spec critical(string(), string(), [any()]) -> ok | {error, luger_not_running}.
critical(Channel, Format, Args) ->
    critical(Channel, io_lib:format(Format, Args)).

-spec error(string(), string()) -> ok | {error, luger_not_running}.
error(Channel, Message) ->
    log(?ERROR, Channel, Message).

-spec error(string(), string(), [any()]) -> ok | {error, luger_not_running}.
error(Channel, Format, Args) ->
    luger:error(Channel, io_lib:format(Format, Args)).

-spec warning(string(), string()) -> ok | {error, luger_not_running}.
warning(Channel, Message) ->
    log(?WARNING, Channel, Message).

-spec warning(string(), string(), [any()]) -> ok | {error, luger_not_running}.
warning(Channel, Format, Args) ->
    warning(Channel, io_lib:format(Format, Args)).

-spec notice(string(), string()) -> ok | {error, luger_not_running}.
notice(Channel, Message) ->
    log(?NOTICE, Channel, Message).

-spec notice(string(), string(), [any()]) -> ok | {error, luger_not_running}.
notice(Channel, Format, Args) ->
    notice(Channel, io_lib:format(Format, Args)).

-spec info(string(), string()) -> ok | {error, luger_not_running}.
info(Channel, Message) ->
    log(?INFO, Channel, Message).

-spec info(string(), string(), [any()]) -> ok.
info(Channel, Format, Args) ->
    info(Channel, io_lib:format(Format, Args)).

-spec debug(string(), string()) -> ok.
debug(Channel, Message) ->
    log(?DEBUG, Channel, Message).

-spec debug(string(), string(), [any()]) -> ok.
debug(Channel, Format, Args) ->
    debug(Channel, io_lib:format(Format, Args)).


%%-----------------------------------------------------------------
%% supervisor_bridge callbacks
%%-----------------------------------------------------------------

-type socket() :: term().
-record(state, {
          app                             :: string(),
          host                            :: string(),
          statsd                          :: boolean(),
          type                            :: stderr | syslog_udp,
          stderr_min_priority = ?WARNING  :: integer(),
          syslog_udp_socket = undefined   :: undefined | socket(),
          syslog_udp_host   = undefined   :: undefined | string(),
          syslog_udp_port   = undefined   :: undefined | integer(),
          syslog_udp_facility = undefined :: undefined | integer()
         }).

init([#config{app = AppName, host = HostName, statsd = Statsd}, SinkConfig]) ->
    register(?PROC_NAME, self()),
    ok = error_logger:add_report_handler(luger_error_logger),

    State = init_sink(#state{app = AppName, host = HostName, statsd = Statsd}, SinkConfig),

    ets:new(?TABLE, [named_table, public, set, {keypos, 1}, {read_concurrency, true}]),
    true = ets:insert_new(?TABLE, {state, State}),

    {ok, self(), undefined}.

terminate(_Reason, _State) ->
    error_logger:delete_report_handler(luger_error_logger),
    ok.


%%-----------------------------------------------------------------
%% implementation
%%-----------------------------------------------------------------

init_sink(State = #state{}, #stderr_config{ min_priority = MinPriority }) ->
    State#state{type = stderr,
                stderr_min_priority = MinPriority};
init_sink(State = #state{}, #syslog_udp_config{ host = Host,
                                                port = Port,
                                                facility = Facility }) ->
    {ok, Socket} = inet_udp:open(0),
    State#state{type = syslog_udp,
                syslog_udp_socket = Socket,
                syslog_udp_host = Host,
                syslog_udp_port = Port,
                syslog_udp_facility = Facility}.

log(Priority, Channel, Message) ->
    case whereis(?PROC_NAME) of
        undefined -> {error, luger_not_running};
        _ -> do_log(Priority, Channel, Message)
    end.

do_log(Priority, Channel, Message) ->
    [{state, State}] = ets:lookup(?TABLE, state),

    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    Data = [io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B ",
                          [Year, Month, Day, Hour, Min, Sec]),
            State#state.host, $\s,
            State#state.app, $\s,
            io_lib:format("~p ", [self()]),
            luger_utils:channel(Channel), $\s,
            $\s, % structured message
            luger_utils:message(Message)],
    log_to(Priority, State#state.type, Data, State),

    record_metric(Priority, State#state.statsd),
    ok.

stderr_priority(?EMERGENCY) -> <<"<emergency>">>;
stderr_priority(?ALERT) -> <<"<alert>">>;
stderr_priority(?CRITICAL) -> <<"<critical>">>;
stderr_priority(?ERROR) -> <<"<error>">>;
stderr_priority(?WARNING) -> <<"<warning>">>;
stderr_priority(?NOTICE) -> <<"<notice>">>;
stderr_priority(?INFO) -> <<"<info>">>;
stderr_priority(?DEBUG) -> <<"<debug>">>.

log_to(Priority, stderr, Data, #state{stderr_min_priority = MinPriority}) ->
    case Priority of
        _ when Priority =< MinPriority ->
            io:put_chars(standard_error, [stderr_priority(Priority), " ", Data, $\n]);
        _ -> ok
    end;

log_to(Priority, syslog_udp, Data0, State = #state{syslog_udp_socket = Socket,
                                                   syslog_udp_host = Host,
                                                   syslog_udp_port = Port,
                                                   syslog_udp_facility = Facility}) ->
    Data1 = ["<", integer_to_list(Facility * 8 + Priority), ">1 ", Data0],
    case inet_udp:send(Socket, Host, Port, Data1) of
        ok -> ok;
        {error, Reason} ->
            io:format("ERROR: unable to log to syslog over udp with ~p: ~s~n",
                      [{Socket, Host, Port}, Reason]),
            log_to(Priority, stderr, Data0, State)
    end.

record_metric(Priority, true) ->
    Key = [<<"luger.">>, luger_utils:priority_to_list(Priority)],
    statsderl:increment(Key, 1, 1.0);
record_metric(_Priority, false) ->
    ok.
