-module(luger_utils).
-include("luger.hrl").

-export([appname/1,
         hostname/0,
         channel/1,
         message/3,
         priority_to_list/1,
         send_stderr/1, send_stderr/2,
         send_syslog/4,
         get_time/0
        ]).

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

channel(Name) ->
    trunc(32, Name).

single_line(Msg, false) ->
    Msg;
single_line(Msg, true) ->
    lists:join(" ", binary:split(iolist_to_binary(Msg), [<<"\n">>, <<" ">>], [global, trim_all])).

message(Msg, SingleLine, infinity) ->
    single_line(Msg, SingleLine);
message(Msg, SingleLine, MaxLen) ->
    trunc(MaxLen, single_line(Msg, SingleLine)).

priority_to_list(?EMERGENCY) -> "emergency";
priority_to_list(?ALERT) -> "alert";
priority_to_list(?CRITICAL) -> "critical";
priority_to_list(?ERROR) -> "error";
priority_to_list(?WARNING) -> "warning";
priority_to_list(?NOTICE) -> "notice";
priority_to_list(?INFO) -> "info";
priority_to_list(?DEBUG) -> "debug".


%% Exist so we can trap the call via meck without disrupting other
%% parts of the system. Need to come up with something better.

send_stderr(IoDevice, Line) ->
    io:format(IoDevice, "~s", [Line]).

send_stderr(Line) ->
    send_stderr(standard_error, Line).


send_syslog(Socket, Host, Port, Line) ->
    inet_udp:send(Socket, Host, Port, Line).


%% Used in order to mock time in test suite

get_time() ->
    erlang:monotonic_time(seconds).
