{application, sellaprime,
 [{description, "The best way to sell fake prime numbers"},
  {vsn, "1.0"},
  {modules, [sellaprime_app, super_supervisor, area_server,
             prime_server, my_alarm_handler]},
  {registered, [area_server, prime_server, super_supervisor]},
  {applications, [kernel,stdlib]},
  {mod, {sellaprime_app,[]}},
  {start_phases, []}]}.
