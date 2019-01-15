-module(luger_test_server).

-behaviour(gen_server).

-export([start_link/0,
         log/2,
         get_time/0,
         inc_time/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

log(Source, Msg) ->
    gen_server:call(?SERVER, {log, Source, Msg}).

get_time() ->
    gen_server:call(?SERVER, get_time).

inc_time(Ms) ->
    gen_server:call(?SERVER, {inc_time, Ms}).

init([]) ->
    {ok, {erlang:monotonic_time(millisecond), #{}}}.

handle_call(get_time, _From, State = {Time, _}) ->
    {reply, Time, State};

handle_call({inc_time, Ms}, _From, {Time, Map}) ->
    {reply, ok, {Time + Ms, Map}};

handle_call({log, Source, Msg}, _From, {Time, Map}) ->
    Map2 = case Map of
        #{Source := {Msgs, Read}} ->
            Map#{Source => {[Msg | Msgs], Read}};
        _ ->
            Map#{Source => {[Msg], []}}
    end,
    {reply, ok, {Time, Map2}};

handle_call({get, Source}, _From, State = {Time, Map}) ->
    case Map of
        #{Source := {[], _}} ->
            {reply, undefined, State};
        #{Source := {Msgs, Read}} ->
            Out = lists:reverse(Msgs),
            % Keep already-read messages around to disambiguate the
            % 'already-read' case from the 'haven't-received-yet' case
            Map2 = Map#{Source => {[], Read ++ Out}},
            {reply, Out, {Time, Map2}};
        _ ->
            {reply, undefined, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(close, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
