{application, nlb,
    [{description, "The erlang network load balance"},
	{vsn, "1.0"},
	{modules, [nlb_app, nlb_sup]},
	{registered, [nlb_server, nlb_sup]},
	{applications, [kernel, stdlib]},
	{mod, {nlb_app, []}} ,
	{mod, {sellaprime_app,[]}},
	{start_phases, []}
    ]}.

