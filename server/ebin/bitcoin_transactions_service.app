{ application, bitcoin_transactions_service,
 [ { description, "Bitcoin transactions service written in Erlang and ZeroMQ." },
   { vsn, "1.0" },
   { modules, [] },
   { registered, [] },
   { applications, [ kernel, stdlib, sasl ] },
   { env, [] },
   { mod, { bitcoin_transactions_service_app, [] } }
 ]
}.
