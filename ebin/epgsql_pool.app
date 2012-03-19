{application,epgsql_pool,
             [{description,"PostgreSQL Connection Pool"},
              {vsn,"0.1"},
              {registered,[]},
              {applications,[kernel,stdlib,epgsql]},
              {mod,{epgsql_pool_app,[]}},
              {env,[{pools,[]}]},
              {modules,[epgsql_pool,epgsql_pool_app,epgsql_pool_sup,
                        pgsql_pool]}]}.
