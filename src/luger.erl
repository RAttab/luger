-module(luger).
-include("luger.hrl").

-export([start_link/3, start_link/4,
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

-define(BASE_PRIORITY, 1 * 8).
-define(DEFAULT_PRIORITY, ?BASE_PRIORITY + 4).


%%-----------------------------------------------------------------
%% public interface
%%-----------------------------------------------------------------

-spec start_link(string(), stderr, integer()) -> {ok, pid()} | {error, any()}.
start_link(AppName, stderr, MinPriority) ->
    supervisor_bridge:start_link(?MODULE, {AppName, stderr, MinPriority}).

-spec start_link(string(), syslog_udp, string(), integer()) -> {ok, pid()} | {error, any()}.
start_link(AppName, syslog_udp, Host, Port) ->
    supervisor_bridge:start_link(?MODULE, {AppName, syslog_udp, Host, Port}).


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
          type                            :: stderr | syslog_udp,
          syslog_udp_socket = undefined   :: undefined | socket(),
          syslog_udp_host   = undefined   :: undefined | string(),
          syslog_udp_port   = undefined   :: undefined | integer(),
          stderr_min_priority = undefined :: undefined | integer()
         }).


init({AppName, stderr, MinPriority}) ->
    ets:new(luger, [named_table, public, set, {keypos, 1}, {read_concurrency, true}]),
    true = ets:insert_new(luger, {config, #state{type = stderr,
                                                 app = appname(AppName),
                                                 host = hostname(),
                                                 stderr_min_priority = MinPriority
                                                }}),
    ok = error_logger:add_report_handler(luger_error_logger),
    {ok, self(), undefined};

init({AppName, syslog_udp, Host, Port}) ->
    {ok, Socket} = inet_udp:open(0),
    ets:new(luger, [named_table, public, set, {keypos, 1}, {read_concurrency, true}]),
    true = ets:insert_new(luger, {config, #state{type = syslog_udp,
                                                 app = appname(AppName),
                                                 host = hostname(),
                                                 syslog_udp_socket = Socket,
                                                 syslog_udp_host = Host,
                                                 syslog_udp_port = Port
                                                }}),
    ok = error_logger:add_report_handler(luger_error_logger),
    {ok, self(), undefined}.

terminate(_Reason, _State) ->
    error_logger:delete_report_handler(luger_error_logger),
    ok.


%%-----------------------------------------------------------------
%% implementation
%%-----------------------------------------------------------------

trunc(N, S) when is_binary(S) ->
    case S of
        <<Sub:N/bytes, _/binary>> -> Sub;
        _ -> S
    end;
trunc(N, S) when is_list(S) ->
    trunc(N, iolist_to_binary(S)).

appname(Name) ->
    trunc(48, Name).

hostname() ->
    {ok, Host} = inet:gethostname(),
    trunc(255, Host).

log(Priority, Channel, Message) ->
    case ets:lookup(luger, config) of
        [{config, State}] ->
            log(Priority, Channel, Message, State);
        _ ->
            {error, luger_not_running}
    end.

log(Priority, Channel, Message, State) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    Data = [io_lib:format("<~B> ~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B ",
                          [Priority, Year, Month, Day, Hour, Min, Sec]),
            State#state.app, $\s,
            State#state.host, $\s,
            io_lib:format("~p ", [self()]),
            trunc(32, Channel), $\s,
            Message],
    log_to(Priority, State#state.type, Data, State),
    ok.

log_to(Priority, stderr, Data, #state{stderr_min_priority = MinPriority}) when Priority =< MinPriority ->
    io:put_chars(standard_error, [Data, $\n]);
log_to(_Priority, stderr, _Data, _State) ->
    ok;
log_to(Priority, syslog_udp, Data, State = #state{syslog_udp_socket = Socket,
                                        syslog_udp_host = Host,
                                        syslog_udp_port = Port}) ->
    case inet_udp:send(Socket, Host, Port, Data) of
        ok -> ok;
        {error, Reason} ->
            io:format("ERROR: unable to log to syslog over udp with ~p: ~s~n",
                      [{Socket, Host, Port}, Reason]),
            log_to(Priority, stderr, Data, State)
    end.
