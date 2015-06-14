-module(bitcoin_transactions_service).
-behaviour(gen_server).

-export([ start_link/1 ]).
-export([ handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2, code_change/3 ]).

start_link(Ctx) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Ctx, []).

init(Ctx) ->
    {ok, Ctx}.

handle_call(_Command, _From, State) ->
    {reply, empty, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.
