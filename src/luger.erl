-module(luger).
-include("luger.hrl").

-export([start_link/3,
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
-define(THROTTLE_THRESHOLD, 5).


%%-----------------------------------------------------------------
%% public interface
%%-----------------------------------------------------------------

-spec start_link(tuple(), tuple(), boolean()) -> {ok, pid()} | {error, any()}.
start_link(Config, SinkConfig, SingleLine) ->
    supervisor_bridge:start_link(?MODULE, [Config, SinkConfig, SingleLine]).

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
          single_line                     :: boolean(),
          type                            :: null | stderr | syslog_udp,
          stderr_min_priority = ?WARNING  :: integer(),
          syslog_udp_socket = undefined   :: undefined | socket(),
          syslog_udp_host   = undefined   :: undefined | string(),
          syslog_udp_port   = undefined   :: undefined | integer(),
          syslog_udp_facility = undefined :: undefined | integer()
         }).

init([#config{app = AppName, host = HostName, statsd = Statsd}, SinkConfig, SingleLine]) ->
    register(?PROC_NAME, self()),
    ok = error_logger:add_report_handler(luger_error_logger),

    State = init_sink(#state{app = AppName, host = HostName, statsd = Statsd}, SinkConfig),

    ets:new(?TABLE, [named_table, public, set, {keypos, 1}, {read_concurrency, true}]),
    true = ets:insert_new(?TABLE, {state, State#state{ single_line = SingleLine}}),

    {ok, self(), undefined}.

terminate(_Reason, _State) ->
    error_logger:delete_report_handler(luger_error_logger),
    ok.


%%-----------------------------------------------------------------
%% implementation
%%-----------------------------------------------------------------

init_sink(State = #state{}, #null_config{}) ->
    State#state{type = null};
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
    log(Priority, Channel, Message, whereis(?PROC_NAME)).

log(_Priority, _Channel, _Message, undefined) ->
    {error, luger_not_running};
log(Priority, Channel, Message, _) ->
    [{state, State}] = ets:lookup(?TABLE, state),

    Now = erlang:monotonic_time(seconds),
    Throttle = throttle_channel(Channel, Now, ets:lookup(?TABLE, Channel)),

    log(State, Priority, Channel, Message, Throttle).


log(_State, _Priority, _Channel, _Message, throttled) ->
    throttled;
log(State, Priority, Channel, Message, _) ->
    record_metrics(State, Priority, Channel),
    do_log(State, Priority, Channel, Message).


throttle_channel(Channel, Now, [{Channel, _, LastUpdate}])
  when Now > LastUpdate ->
    ets:insert(?TABLE, {Channel, 1, Now});
throttle_channel(Channel, _Now, [{Channel, Freq, _}])
  when Freq < ?THROTTLE_THRESHOLD ->
    ets:update_counter(?TABLE, Channel, {2, 1});
throttle_channel(_Channel, _Now, [{_, _, _}]) ->
    throttled;
throttle_channel(Channel, Now, _) ->
    ets:insert(?TABLE, {Channel, 1, Now}).


record_metrics(#state{statsd = true}, Priority, Channel) ->
    Key = [<<"luger.">>, luger_utils:priority_to_list(Priority), $., Channel],
    statsderl:increment(Key, 1, 1.0);
record_metrics(#state{statsd = false}, _Priority, _Channel) ->
    ok.


do_log(State, Priority, Channel, Message) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    Data = [io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B ",
                          [Year, Month, Day, Hour, Min, Sec]),
            State#state.host, $\s,
            State#state.app, $\s,
            io_lib:format("~p ", [self()]),
            luger_utils:channel(Channel), $\s,
            $\s, % structured message
            luger_utils:message(Message, State#state.single_line)],
    log_to(Priority, State#state.type, Data, State).

stderr_priority(?EMERGENCY) -> <<"<emergency>">>;
stderr_priority(?ALERT) -> <<"<alert>">>;
stderr_priority(?CRITICAL) -> <<"<critical>">>;
stderr_priority(?ERROR) -> <<"<error>">>;
stderr_priority(?WARNING) -> <<"<warning>">>;
stderr_priority(?NOTICE) -> <<"<notice>">>;
stderr_priority(?INFO) -> <<"<info>">>;
stderr_priority(?DEBUG) -> <<"<debug>">>.

log_to(_Priority, null, _Data, _State) ->
    ok;
log_to(Priority, stderr, Data, #state{stderr_min_priority = MinPriority}) ->
    case Priority of
        _ when Priority =< MinPriority ->
            luger_utils:send_stderr([stderr_priority(Priority), " ", Data, $\n]);
        _ -> ok
    end;

log_to(Priority, syslog_udp, Data0, State = #state{syslog_udp_socket = Socket,
                                                   syslog_udp_host = Host,
                                                   syslog_udp_port = Port,
                                                   syslog_udp_facility = Facility}) ->
    Data1 = ["<", integer_to_list(Facility * 8 + Priority), ">1 ", Data0],
    case luger_utils:send_syslog(Socket, Host, Port, Data1) of
        ok -> ok;
        {error, Reason} ->
            io:format("ERROR: unable to log to syslog over udp with ~p: ~s~n",
                      [{Socket, Host, Port}, Reason]),
            log_to(Priority, stderr, Data0, State)
    end.
