-module(nlb_sup).
-behaviour(supervisor).

-export([
    start/0,
    start_link/1,
    init/1
    ]).

start() ->
	%s:start(),
	timer:apply_interval(10000,backend_service,check_backend_status,[]),
    spawn(fun() ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg=[])
    end)
	
	.

start_link(Args) ->
	%s:start(),
	timer:apply_interval(10000,backend_service,check_backend_status,[]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args)
	.

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 30,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,

    Nlb_server = {'nlb_server', {nlb_server, start_link, []},
        Restart, Shutdown, worker, dynamic},
 	Backend_service = {'backend_service', {backend_service, start_link, []},
         Restart, Shutdown, worker, dynamic},
    
    {ok, {SupFlags, [Backend_service,Nlb_server]}}.
%{ok, {SupFlags, [Nlb_server,Backend_service]}}.

