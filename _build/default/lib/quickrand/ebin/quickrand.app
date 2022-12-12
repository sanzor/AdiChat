{application,quickrand,
             [{description,"Quick Random Number Generation"},
              {vsn,"2.0.4"},
              {modules,[quickrand,quickrand_cache,quickrand_cache_normal,
                        quickrand_hash,quickrand_normal,random_wh06_int,
                        random_wh82]},
              {registered,[]},
              {applications,[crypto,stdlib,kernel]},
              {env,[{cache_size,65536}]}]}.
