{application, search,
 [{description, "User search server"},
  {vsn, "0.0.0"},
  {mod, {search, []}},
  {registered, []},
  {modules, [search_serv, search_sup, search]},
  {applications, [stdlib, kernel, crypto, emysql, datarace]}]}.
