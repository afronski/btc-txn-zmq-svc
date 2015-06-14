-module(bitcoin_transactions_service_publisher_zmq).
-behaviour(gen_event).

-export([ init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3 ]).

init(Ctx) ->
    Pub = czmq:zsocket_new(Ctx, pub),
    Port = application:get_env(bitcoin_transactions_service, publisher_port, 9002),

    {ok, Port} = czmq:zsocket_bind(Pub, "tcp://0.0.0.0:" ++ integer_to_list(Port)),

    {ok, Pub}.

handle_call(_Request, State) ->
    {reply, empty, State}.

handle_event({transaction, Payload}, Pub) ->
    JSON = jsx:encode(Payload),
    ok = czmq:zstr_send(Pub, JSON),

    {ok, Pub}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Args, Pub) ->
    czmq:zsocket_destroy(Pub),
    ok.
