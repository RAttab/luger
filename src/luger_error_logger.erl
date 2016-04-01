-module(luger_error_logger).

-behaviour(gen_event).
-export([init/1,
         code_change/3,
         terminate/2,
         handle_event/2,
         handle_call/2,
         handle_info/2]).


%%-----------------------------------------------------------------
%% gen_event callbacks
%%-----------------------------------------------------------------

init(_Args) ->
    {ok, undefined}.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

terminate(_Args, _State) ->
    ok.

handle_event({error, _, Data}, S) ->
    log_msg(error, Data),
    {ok, S};
handle_event({error_report, _, Data}, S) ->
    log_report(error, Data),
    {ok, S};
handle_event({warning_msg, _, Data}, S) ->
    log_msg(warning, Data),
    {ok, S};
handle_event({warning_report, _, Data}, S) ->
    log_report(warning, Data),
    {ok, S};
handle_event({info_msg, _, Data}, S) ->
    log_msg(info, Data),
    {ok, S};
handle_event({info_report, _, Data}, S) ->
    log_report(info, Data),
    {ok, S};
handle_event(_Event, S) ->
    {ok, S}.

handle_call(_Event, S) ->
    {ok, S}.

handle_info(_Event, S) ->
    {ok, S}.


%%-----------------------------------------------------------------
%% impl
%%-----------------------------------------------------------------

log_msg(Fn, {Pid, Format, Args}) ->
    luger:Fn("sasl", io_lib:format("~p " ++ Format, [Pid | Args])).

log_report(Fn, Data) ->
    luger:Fn("sasl", io_lib:format("~p", [Data])).
