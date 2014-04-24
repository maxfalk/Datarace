{application, datarace,
 [{description, "Datarace server"},
  {vsn, "0.0.0"},
  {mod, {datarace, []}},
  {registered, []},
  {modules, [datarace, master_sup, client_server, packconv, listener, account, database, usercom]},
  {applications, [stdlib, kernel, crypto, emysql]}]}.
