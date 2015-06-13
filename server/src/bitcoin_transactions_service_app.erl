-module(bitcoin_transactions_service_app).
-behavior(application).

-export([ start/2, stop/1 ]).

start(_Type, _Args) ->
    {ok, Ctx} = czmq:start_link(),
    bitcoin_transactions_service_sup:start_link(Ctx).

stop(_State) ->
    ok.
