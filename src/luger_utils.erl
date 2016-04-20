-module(luger_utils).
-include("luger.hrl").

-export([appname/1,
         hostname/0,
         channel/1,
         message/1,
         priority_to_list/1]).

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

channel(Name) ->
    trunc(32, Name).

message(Msg) ->
    trunc(2048, Msg).

priority_to_list(?EMERGENCY) -> "emergency";
priority_to_list(?ALERT) -> "alert";
priority_to_list(?CRITICAL) -> "critical";
priority_to_list(?ERROR) -> "error";
priority_to_list(?WARNING) -> "warning";
priority_to_list(?NOTICE) -> "notice";
priority_to_list(?INFO) -> "info";
priority_to_list(?DEBUG) -> "debug".
