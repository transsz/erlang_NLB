%% Author: Jacky
%% Created: 2012-8-26
%% Description: TODO: Add description to socket_test
-module(s).
-import(lists, [reverse/1]).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0,loop_origin/2,start_back_socket/2,connect/2]).

-define(TCPPORT, 12345).
-define(TARGET_TCPPORT, 3306).
-define(TARGET_IP_ADD, "10.2.2.135").

%-define(TARGET_TCPPORT, 80).
%-define(TARGET_IP_ADD, "127.0.0.1").

%%
%% API Functions
%%
start() ->
%% 	{ok, Ip} = inet:getaddr("0.0.0.0",inet),
%% 	{ok, Listen} = gen_tcp:listen(?TCPPORT, [{ip, Ip},{reuseaddr, true}, {packet, 0},
%% 				       {active, true}]),
%% 	%{ok, Listen} = gen_tcp:listen(?TCPPORT, [binary, {packet, 0}]),

	start_from_conf()
	.

start_from_conf()->
	{ok,Conf_list}=file:consult("nlb.conf"),
	start_server_one_by_one(Conf_list)
.
%configure list file like 
%{server,"0.0.0.0",port,"11111",
%        [{backend,"192.168.1.111",port,8080,maxconn,10000},
%         {backend,"192.168.1.1",port,8080,maxconn,10000}]},
%We need to parse the file and start the server
start_server_one_by_one([])->
	ok
	;
start_server_one_by_one([X|Y])->
	{server,Server,port,Port,Arr_back_server}=X,
	%io:format("server=~p,port=~p\nArr_back_server=~p\n",[Server,Port,Arr_back_server]),
	Listen_server = io_lib:format("~p~p", [Server,Port]),
	backend_service:regist(Listen_server, Arr_back_server),
	%back_end_list(Arr_back_server),
	start_nlb(Server,Port),
	start_server_one_by_one(Y)
	.


%%%start the nlb server by config
start_nlb(Server,Port)->
	%io:format("Server=~p\n",[Server]),
	{ok, Ip} = inet:getaddr(Server,inet),
	%io:format("ip=~p\n",[Ip]),
	%{active, false},{active, true},{active, once}
	{ok, Listen} = gen_tcp:listen(Port, [{ip, Ip},{reuseaddr, true}, {packet, 0},
				       {active, once}]),
	Listen_server = io_lib:format("~p~p", [Server,Port]),
	spawn(fun()->start_back_socket(Listen,Listen_server) end)
	.

start_back_socket(Listen,Listen_server)->
	%accept the new socket
	case gen_tcp:accept(Listen) of
    {ok, Socket} ->
	 ok;
	_ ->
	  {ok, Listen_new} = gen_tcp:listen(?TCPPORT, [{reuseaddr, true}, {packet, 0},
				       {active, once}]),
	  {ok, Socket} =gen_tcp:accept(Listen_new)
	 end ,
    %gen_tcp:close(Listen),  %% (8)
	%io:format("gen_tcp accept\n"),
	spawn(?MODULE,start_back_socket,[Listen,Listen_server]),
	X=backend_service:get_userful_conn_str(Listen_server),
	 case X of
		 {backend,Back_server,port,Back_port,maxconn,_} ->
		{ok,Socket_target}=	 gen_tcp:connect(Back_server,Back_port,[binary, {packet, 0}]),
		Target_socket = spawn(fun()->  target_data(Socket_target, [])  end),
		%io:format("%%io:format~p",[Target_socket]), 
	    nlb_server:new_conn(self(), Target_socket),
		loop_origin(Target_socket,Socket);
	 Fail->
		 %io:format("backend_service:get_backend_connection =~p\n",[Fail]),
		 gen_tcp:close(Socket)		
	end
	%loop_origin([],Socket)
	.
 
 
%%
%% Local Functions
%%
loop_origin(Target_socket,Socket) ->
    receive
	{tcp, Socket, Bin} ->
	    %%io:format("Server received Socket=~p binary = ~p~n",[Socket,Bin]),
	    %gen_tcp:send(Socket, Bin),  %% (11)
		case Target_socket of
			[] ->
				do_nothing;
			_->
			%Target_socket ! {send_data,Bin}
			nlb_server:send_target_msg(self(), Bin)
		end,
	    loop_origin(Target_socket,Socket);
	{tcp, _S, Bin} ->
		%%io:format("Server received s=~p binary = ~p~n",[S,Bin]),
	    gen_tcp:send(Socket, Bin),  %% (11)
		
	    loop_origin(Target_socket,Socket);
	{tcp_closed, _Socket} ->
		gen_tcp:close(Socket),
		%Target_socket ! {close_by_origin},
		%nlb_server:send_target_msg(self(), {close_by_origin}),
	    %io:format("Server socket closed~n"),
		ok;
	{tcp_error, S, Reason} ->
	    %io:format("nlb socket error(~p, ~p)~n", [S, Reason]),
	    exit(Reason);
	{send_msg,Msg} ->
		gen_tcp:send(Socket, Msg),  %% (11)
	    loop_origin(Target_socket,Socket);
	{close_by_target} ->
		gen_tcp:close(Socket);
		%io:format("close_by_target~n")
	Other ->
		io:format("nlb origin socket get unkown message  ~p\n", [Other]),
		loop_origin(Target_socket,Socket)
    end.

target_data(Socket, Orign_socket) ->
	receive
	%{orgin_pid,Orign_socket1} ->
		%%io:format("Orign_socket=~p\n",[Orign_socket1]),
	%	target_data	(Socket,Orign_socket1)	;
	{send_data,Bin} ->
		%%io:format("send_data Bin=~p\n",[Bin]),
		 gen_tcp:send(Socket,Bin),
		target_data(Socket,Orign_socket);
	{tcp, Socket, Bin} ->
	    %io:format("Target server received binary = ~p~n",[Bin]),
		nlb_server:send_back_msg(self(), Bin),
		%Orign_socket ! {send_msg,Bin},
	    target_data(Socket,Orign_socket);
	{tcp_closed, Socket} ->
	    %io:format("target_data:Server socket closed~n"),
	%	Orign_socket ! {close_by_target}
	%	nlb_server:send_back_msg(self(), {close_by_target});
		ok;
	{close_by_origin}->
		%%io:format("close_by_origin~n"),
		gen_tcp:close(Socket);
		
	{tcp_error, S, Reason} ->
	    %io:format("nlb socket error(~p, ~p)~n", [S, Reason]),
	    exit(Reason);
	Other ->
		%io:format("nlb get unkown message from target server  ~p\n", [Other]),
		target_data(Socket,Orign_socket)
	end
	.

connect(Back_server,Back_port)->
	gen_tcp:connect(Back_server,Back_port,[binary, {packet, 0}])
	.
