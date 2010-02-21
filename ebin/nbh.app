{application, nbh,
    [{description, "Neighbour nodes search"},
     {author, "Jakub Kulhan <jakub.kulhan@gmail.com>"},
     {modules, [nbh, nbhsup, nbhsrv]},
     {applications, [kernel, stdlib, crypto]},
     {mod, {nbh, []}},
     {env, [{addr, {232, 0, 1, 66}},
            {port, 6666},
            {ttl, 1}]}
    ]}.
