-module(nlb_app).
-behaviour(application).

-export([
    start/0,
    start/2,
    stop/1
    ]).

start() ->
    application:start(nlb).

start(_Type, StartArgs) ->
    nlb_sup:start_link(StartArgs).

stop(_State) ->
    ok.
