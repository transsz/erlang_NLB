%% Author: Jacky
%% Created: 2012-8-26
%% Description: TODO: the network load balance connection management
%% For: manage income socket and out socket
-module(connection_mgr).
-import(lists, [reverse/1]).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0,loop_origin/2,start_socket/1,loop/1]).

-define(TCPPORT, 11111).
-define(TARGET_TCPPORT, 3306).
-define(TARGET_IP_ADD, "10.2.2.3").

%-define(TARGET_TCPPORT, 80).
%-define(TARGET_IP_ADD, "127.0.0.1").

%%
%% API Functions
%%
start() ->
	{ok, Listen} = gen_tcp:listen(?TCPPORT, [{reuseaddr, true}, {packet, 0},
				       {active, true}]),
	%{ok, Listen} = gen_tcp:listen(?TCPPORT, [binary, {packet, 0}]),
	
	%spawn(fun()->start_socket(Listen) end)
	start_socket(Listen)
    .

start_socket(Listen)->
	%{ok, Listen} = gen_tcp:listen(11111, [binary, {packet, 4},  %% (6)
	%				 {reuseaddr, true},
	%				 {active, true}]),
	case gen_tcp:accept(Listen) of
		
    {ok, Socket} ->
	 ok;
	_ ->
	  {ok, Listen_new} = gen_tcp:listen(?TCPPORT, [{reuseaddr, true}, {packet, 0},
				       {active, once}]),
	  {ok, Socket} =gen_tcp:accept(Listen_new)
	 end ,
	  %% (7)
    %gen_tcp:close(Listen),  %% (8)
%% 	io:format("gen_tcp accept\n"),
%% 	{ok,Socket_target} = gen_tcp:connect(?TARGET_IP_ADD,?TARGET_TCPPORT,[binary, {packet, 0}]), %% (1)
%% 	io:format("Socket_target=~p\n",[Socket_target]),
%% 	io:format("Socket=~p\n",[Socket]),
%% 	io:format("gen_tcp accept target server\n"),
%% 	Target_socket = spawn(fun()->  target_data(Socket_target, [])  end),
    spawn(?MODULE,start_socket,[Listen]),
%% 	Target_socket ! {orgin_pid,self()},
	erlang:hibernate(?MODULE, loop, [Socket])
	%loop(Socket)
	%loop_origin(Target_socket,Socket)
	%loop_origin([],Socket)
	.
 
loop(Socket) ->
    receive
	{tcp, Socket, Bin} ->
	    %io:format("Server received Socket=~p binary = ~p~n",[Socket,Bin]),
	   
	    loop(Socket);
	{tcp, _S, Bin} ->
		%io:format("Server received s=~p binary = ~p~n",[S,Bin]),
	    %gen_tcp:send(Socket, Bin),  %% (11)
		
	    loop(Socket);
	{tcp_closed, _Socket} ->
		gen_tcp:close(Socket),
		%Target_socket ! {close_by_origin},
	    io:format("Server socket closed~n");
	{tcp_error, S, Reason} ->
	    io:format("nlb socket error(~p, ~p)~n", [S, Reason]),
	    exit(Reason);
	{send_msg,Msg} ->
		gen_tcp:send(Socket, Msg),  %% (11)
	    loop(Socket);
	{close_by_target} ->
		gen_tcp:close(Socket),
		io:format("close_by_target~n");
	Other ->
		io:format("nlb origin socket get unkown message  ~p\n", [Other]),
		loop(Socket)
    end.

%%
%% Local Functions
%%
loop_origin(Target_socket,Socket) ->
    receive
	{tcp, Socket, Bin} ->
	    %io:format("Server received Socket=~p binary = ~p~n",[Socket,Bin]),
	    %gen_tcp:send(Socket, Bin),  %% (11)
		case Target_socket of
			[] ->
				do_nothing;
			_->
			Target_socket ! {send_data,Bin}
		end,
	    loop_origin(Target_socket,Socket);
	{tcp, _S, Bin} ->
		%io:format("Server received s=~p binary = ~p~n",[S,Bin]),
	    gen_tcp:send(Socket, Bin),  %% (11)
		
	    loop_origin(Target_socket,Socket);
	{tcp_closed, _Socket} ->
		gen_tcp:close(Socket),
		Target_socket ! {close_by_origin},
	    io:format("Server socket closed~n");
	{tcp_error, S, Reason} ->
	    io:format("nlb socket error(~p, ~p)~n", [S, Reason]),
	    exit(Reason);
	{send_msg,Msg} ->
		gen_tcp:send(Socket, Msg),  %% (11)
	    loop_origin(Target_socket,Socket);
	{close_by_target} ->
		gen_tcp:close(Socket),
		io:format("close_by_target~n");
	Other ->
		io:format("nlb origin socket get unkown message  ~p\n", [Other]),
		loop_origin(Target_socket,Socket)
    end.

target_data(Socket, Orign_socket) ->
	receive
	{orgin_pid,Orign_socket1} ->
		%io:format("Orign_socket=~p\n",[Orign_socket1]),
		target_data	(Socket,Orign_socket1)	;
	{send_data,Bin} ->
		%io:format("send_data Bin=~p\n",[Bin]),
		 gen_tcp:send(Socket,Bin),
		target_data(Socket,Orign_socket);
	{tcp, Socket, Bin} ->
	    %io:format("Target server received binary = ~p~n",[Bin]),
		Orign_socket ! {send_msg,Bin},
	    target_data(Socket,Orign_socket);
	{tcp_closed, Socket} ->
	    %io:format("target_data:Server socket closed~n"),
		Orign_socket ! {close_by_target};
	{close_by_origin}->
		gen_tcp:close(Socket);
		
	{tcp_error, S, Reason} ->
	    io:format("nlb socket error(~p, ~p)~n", [S, Reason]),
	    exit(Reason);
	Other ->
		io:format("nlb get unkown message from target server  ~p\n", [Other]),
		target_data(Socket,Orign_socket)
	end
	.


