-module(luger).

-export([start_link/1, start_link/3,
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


%%-----------------------------------------------------------------
%% public interface
%%-----------------------------------------------------------------

-spec start_link(stderr) -> {ok, pid()} | {error, any()}.
start_link(stderr) ->
    supervisor_bridge:start_link(?MODULE, stderr).

-spec start_link(syslog_udp, string(), integer()) -> {ok, pid()} | {error, any()}.
start_link(syslog_udp, Host, Port) ->
    supervisor_bridge:start_link(?MODULE, {syslog_udp, Host, Port}).


-spec emergency(string(), string()) -> ok.
emergency(Channel, Message) ->
    log(?BASE_PRIORITY + 0, Channel, Message).

-spec emergency(string(), string(), [any()]) -> ok.
emergency(Channel, Format, Args) ->
    emergency(Channel, io_lib:format(Format, Args)).

-spec alert(string(), string()) -> ok.
alert(Channel, Message) ->
    log(?BASE_PRIORITY + 1, Channel, Message).

-spec alert(string(), string(), [any()]) -> ok.
alert(Channel, Format, Args) ->
    alert(Channel, io_lib:format(Format, Args)).

-spec critical(string(), string()) -> ok.
critical(Channel, Message) ->
    log(?BASE_PRIORITY + 2, Channel, Message).

-spec critical(string(), string(), [any()]) -> ok.
critical(Channel, Format, Args) ->
    critical(Channel, io_lib:format(Format, Args)).

-spec error(string(), string()) -> ok.
error(Channel, Message) ->
    log(?BASE_PRIORITY + 3, Channel, Message).

-spec error(string(), string(), [any()]) -> ok.
error(Channel, Format, Args) ->
    luger:error(Channel, io_lib:format(Format, Args)).

-spec warning(string(), string()) -> ok.
warning(Channel, Message) ->
    log(?BASE_PRIORITY + 4, Channel, Message).

-spec warning(string(), string(), [any()]) -> ok.
warning(Channel, Format, Args) ->
    warning(Channel, io_lib:format(Format, Args)).

-spec notice(string(), string()) -> ok.
notice(Channel, Message) ->
    log(?BASE_PRIORITY + 5, Channel, Message).

-spec notice(string(), string(), [any()]) -> ok.
notice(Channel, Format, Args) ->
    notice(Channel, io_lib:format(Format, Args)).

-spec info(string(), string()) -> ok.
info(Channel, Message) ->
    log(?BASE_PRIORITY + 6, Channel, Message).

-spec info(string(), string(), [any()]) -> ok.
info(Channel, Format, Args) ->
    info(Channel, io_lib:format(Format, Args)).

-spec debug(string(), string()) -> ok.
debug(Channel, Message) ->
    log(?BASE_PRIORITY + 7, Channel, Message).

-spec debug(string(), string(), [any()]) -> ok.
debug(Channel, Format, Args) ->
    debug(Channel, io_lib:format(Format, Args)).



%%-----------------------------------------------------------------
%% supervisor_bridge callbacks
%%-----------------------------------------------------------------

-type socket() :: term().
-record(state, {
          host                          :: string(),
          type                          :: stderr | syslog_udp,
          syslog_udp_socket = undefined :: undefined | socket(),
          syslog_udp_host   = undefined :: undefined | string(),
          syslog_udp_port   = undefined :: undefined | integer()
         }).


init(stderr) ->
    ets:new(luger, [named_table, public, set, {keypos, 1}, {read_concurrency, true}]),
    true = ets:insert_new(luger, {config, #state{type = stderr,
                                                 host = hostname()
                                                }}),
    {ok, self(), undefined};

init({syslog_udp, Host, Port}) ->
    {ok, Socket} = inet_udp:open(0),
    ets:new(luger, [named_table, public, set, {keypos, 1}, {read_concurrency, true}]),
    true = ets:insert_new(luger, {config, #state{type = syslog_udp,
                                                 host = hostname(),
                                                 syslog_udp_socket = Socket,
                                                 syslog_udp_host = Host,
                                                 syslog_udp_port = Port
                                                }}),
    {ok, self(), undefined}.

terminate(_Reason, _State) ->
    ok.


%%-----------------------------------------------------------------
%% implementation
%%-----------------------------------------------------------------

hostname() ->
    {ok, Host} = inet:gethostname(),
    Host.

trunc(N, S) when is_binary(S) ->
    case S of
        <<Sub:N/bytes, _>> -> Sub;
        _ -> S
    end;
trunc(N, S) when is_list(S), length(S) > N ->
    {Head, _} = lists:split(N, S),
    Head;
trunc(_, S) when is_list(S) ->
    S.

log(Priority, Channel, Message) ->
    [{config, State}] = ets:lookup(luger, config),
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    Data = [io_lib:format("<~B> ~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B ",
                          [Priority, Year, Month, Day, Hour, Min, Sec]),
            trunc(255, State#state.host), $\s,
            io_lib:format("~p ", [self()]),
            trunc(32, Channel), $\s,
            Message],
    log_to(State#state.type, Data, State),
    ok.

log_to(stderr, Data, _) ->
    io:put_chars(standard_error, [Data, $\n]);
log_to(syslog_udp, Data, State = #state{syslog_udp_socket = Socket,
                                        syslog_udp_host = Host,
                                        syslog_udp_port = Port}) ->
    case inet_udp:send(Socket, Host, Port, Data) of
        ok -> ok;
        {error, Reason} ->
            io:format("ERROR: unable to log to syslog over udp with ~p: ~s~n",
                      [{Socket, Host, Port}, Reason]),
            log_to(stderr, Data, State)
    end.
