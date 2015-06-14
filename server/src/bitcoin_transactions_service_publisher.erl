-module(bitcoin_transactions_service_publisher).

-export([ start_link/1, new_transaction/3 ]).

start_link(Ctx) ->
    {ok, Pid} = gen_event:start_link({local, ?MODULE}),

    ok = gen_event:add_handler(?MODULE, bitcoin_transactions_service_publisher_zmq, Ctx),

    {ok, Pid}.

new_transaction(Hash, Lat, Lng) when is_list(Hash), is_float(Lat), is_float(Lng) ->
    Payload = [ {hash, list_to_bitstring(Hash)}, {lat, Lat}, {lng, Lng}, {type, <<"transaction">>} ],
    gen_event:notify(?MODULE, {transaction, Payload}).
