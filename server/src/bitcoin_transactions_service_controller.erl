-module(bitcoin_transactions_service_controller).
-behaviour(gen_server).

-export([ start_link/1 ]).
-export([ handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2, code_change/3 ]).

start_link(Ctx) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Ctx, []).

handle_messages(_Socket, error)    -> ok;
handle_messages(Rep, { ok, Message }) ->
    io:format("~p~n", [ Message ]),

    czmq:zstr_send(Rep, "ACK"),
    ok.

loop(Rep) ->
    ok = handle_messages(Rep, czmq:zstr_recv_nowait(Rep)),
    loop(Rep).

init(Ctx) ->
    Rep = czmq:zsocket_new(Ctx, rep),
    Port = application:get_env(bitcoin_transactions_service, controller_port, 10020),

    {ok, Port} = czmq:zsocket_bind(Rep, "tcp://0.0.0.0:" ++ integer_to_list(Port)),

    Pid = spawn_link(fun() -> loop(Rep) end),
    register(bitcoin_transactions_service_controller_socket_loop, Pid),

    {ok, Rep}.

handle_call(_Command, _From, State) ->
    {reply, empty, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Args, Rep) ->
    cmzq:zsocket_destroy(Rep),
    ok.
