-module(bitcoin_transactions_service_publisher).
%% -behaviour(gen_event).

-export([ start_link/1 ]).

start_link(_Ctx) ->
    ok.
