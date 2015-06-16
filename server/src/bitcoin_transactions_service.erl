-module(bitcoin_transactions_service).
-behaviour(gen_server).

-export([ start_link/1 ]).
-export([ handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2, code_change/3 ]).

start_link(Ctx) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Ctx, []).

init(Ctx) ->
    << A:32, B:32, C:32 >> = crypto:rand_bytes(12),
    random:seed({A,B,C}),

    MinLng = application:get_env(bitcoin_transactions_service, min_lng, -12),
    MaxLng = application:get_env(bitcoin_transactions_service, max_lng, 30),

    MinLat = application:get_env(bitcoin_transactions_service, min_lat, 42),
    MaxLat = application:get_env(bitcoin_transactions_service, max_lat, 65),

    Timer = erlang:send_after(1, self(), new_transaction),

    {ok, { Ctx, Timer, {MinLat, MaxLat, MinLng, MaxLng} }}.

handle_call(_Command, _From, State) ->
    {reply, empty, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(new_transaction, {Ctx, OldTimer, {MinLat, MaxLat, MinLng, MaxLng} = Bounds}) ->
    erlang:cancel_timer(OldTimer),

    Lat = random:uniform() * (MaxLat - MinLat) + MinLat,
    Lng = random:uniform() * (MaxLng - MinLng) + MinLng,

    Payload = list_to_binary(io_lib:format("~f~f", [ Lat, Lng ])),
    Hash = string:to_lower(binary_to_list(base16:encode(crypto:hash(sha, Payload)))),

    bitcoin_transactions_service_publisher:new_transaction(Hash, Lat, Lng),

    Time = random:uniform(1500) + 1000,
    Timer = erlang:send_after(Time, self(), new_transaction),

    {noreply, {Ctx, Timer, Bounds}};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.
