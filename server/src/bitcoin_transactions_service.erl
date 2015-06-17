-module(bitcoin_transactions_service).
-behaviour(gen_server).

-include("../include/bitcoin_transactions_service.hrl").

-export([ start_link/0, start/0, stop/0, change_coordinates/1 ]).
-export([ handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2, code_change/3 ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:call(?MODULE, start).

stop() ->
    gen_server:call(?MODULE, stop).

change_coordinates(Coordinates) ->
    gen_server:call(?MODULE, {change_coordinates, Coordinates}).

init(_Args) ->
    << A:32, B:32, C:32 >> = crypto:rand_bytes(12),
    random:seed({A,B,C}),

    MinLng = application:get_env(bitcoin_transactions_service, min_lng, -25),
    MaxLng = application:get_env(bitcoin_transactions_service, max_lng, 40),

    MinLat = application:get_env(bitcoin_transactions_service, min_lat, 31),
    MaxLat = application:get_env(bitcoin_transactions_service, max_lat, 71),

    Bounds = #bounds{
        nw = #latlng{ lat = MaxLat, lng = MaxLng },
        se = #latlng{ lat = MinLat, lng = MinLng }
    },

    {ok, #state{status = stopped, bounds = Bounds}}.

handle_call(start, _From, #state{status = stopped} = State) ->
    Time = random:uniform(1500) + 1000,
    Timer = erlang:send_after(Time, self(), new_transaction),
    NewState = State#state{timer = Timer, status = started},
    {reply, started, NewState};

handle_call(start, _From, #state{status = started} = State) ->
    {reply, already_started, State};

handle_call(stop, _From, #state{status = started, timer = Timer} = State) ->
    erlang:cancel_timer(Timer),
    NewState = State#state{timer = undefined, status = stopped},
    {reply, stopped, NewState};

handle_call(stop, _From, #state{status = stopped} = State) ->
    {reply, already_stopped, State};

handle_call({change_coordinates, Coordinates}, _From, State) ->
    {NW, SE} = applyCoordinates(Coordinates, #latlng{lat = 0.0, lng = 0.0}, #latlng{lat = 0.0, lng = 0.0}),

    NewBounds = #bounds{nw = NW, se = SE},
    NewState = State#state{bounds = NewBounds},

    {reply, coordinates_changed, NewState};

handle_call(_Command, _From, State) ->
    {reply, empty, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(new_transaction, #state{timer = OldTimer, bounds = Bounds} = State) ->
    erlang:cancel_timer(OldTimer),

    NW = Bounds#bounds.nw,
    SE = Bounds#bounds.se,

    Lat = random:uniform() * (NW#latlng.lat - SE#latlng.lat) + SE#latlng.lat,
    Lng = random:uniform() * (NW#latlng.lng - SE#latlng.lng) + SE#latlng.lng,

    Payload = list_to_binary(io_lib:format("~f~f", [ Lat, Lng ])),
    Hash = string:to_lower(binary_to_list(base16:encode(crypto:hash(sha, Payload)))),

    bitcoin_transactions_service_publisher:new_transaction(Hash, Lat, Lng),

    Time = random:uniform(1500) + 1000,
    Timer = erlang:send_after(Time, self(), new_transaction),

    NewState = State#state{timer = Timer, bounds = Bounds},

    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.

applyCoordinates([], NW, SE) ->
    {NW, SE};

applyCoordinates([ {Name, Value} | Rest ], NW, SE) ->
    {NewNW, NewSE} = case Name of
        <<"nw-corner-lat">> -> {#latlng{lat = Value, lng = NW#latlng.lng}, SE};
        <<"nw-corner-lng">> -> {#latlng{lat = NW#latlng.lat, lng = Value }, SE};
        <<"se-corner-lat">> -> {NW, #latlng{lat = Value, lng = SE#latlng.lng}};
        <<"se-corner-lng">> -> {NW, #latlng{lat = SE#latlng.lat, lng = Value}}
    end,

    applyCoordinates(Rest, NewNW, NewSE).
