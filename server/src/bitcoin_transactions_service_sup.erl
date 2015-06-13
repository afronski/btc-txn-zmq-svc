-module(bitcoin_transactions_service_sup).
-behaviour(supervisor).

-export([ init/1 ]).
-export([ start_link/1 ]).

start_link(Ctx) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Ctx).

init(Ctx) ->
    RestartStrategy = rest_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 5000,

    Flags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Controller = {bitcoin_transactions_service_controller,
                  {bitcoin_transactions_service_controller, start_link, [ Ctx ]},
                  Restart, Shutdown, Type,
                  [ bitcoin_transactions_service_controller ]},

    Publisher = {bitcoin_transactions_service_publisher,
                 {bitcoin_transactions_service_publisher, start_link, [ Ctx ]},
                 Restart, Shutdown, Type,
                 [ bitcoin_transactions_service_publisher ]},

    Core = {bitcoin_transactions_service,
            {bitcoin_transactions_service, start_link, [ Ctx ]},
            Restart, Shutdown, Type,
            [ bitcoin_transactions_service ]},

    {ok, {Flags, [ Core, Publisher, Controller ]}}.
