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
    report(error, Data),
    {ok, S};
handle_event({error_report, _, Data}, S) ->
    report(error, Data),
    {ok, S};
handle_event({warning_msg, _, Data}, S) ->
    report(warning, Data),
    {ok, S};
handle_event({warning_report, _, Data}, S) ->
    report(warning, Data),
    {ok, S};
handle_event({info_msg, _, Data}, S) ->
    report(info, Data),
    {ok, S};
handle_event({info_report, _, Data}, S) ->
    report(info, Data),
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

report(Fn, Data) ->
    %% I know I'm going to hell for having written this but I really
    %% want strings to be displayed as strings (ie. can't use ~w)
    %% without having to enable support for multi-line messages
    %% (ie. can't use raw ~p) in syslog.
    Tokens = string:tokens(lists:flatten(io_lib:format("~p", [Data])), [$\n]),
    luger:Fn("sasl", lists:map(fun (Arg) -> string:strip(Arg) end, Tokens)).
