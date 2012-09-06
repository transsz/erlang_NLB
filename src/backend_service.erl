%%%-------------------------------------------------------------------
%%% File    : backend_service provide the backend server connection
%%%           management,and check the health status of backend server
%%% Author  : Jacky.Chen <transsz@126.com>
%%% Description : 
%%% the connection otp service
%%% Created :  Sep 31 2012
%%%-------------------------------------------------------------------
-module(backend_service).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([test/0,regist/2,send_back_msg/2,check_backend_status/0]).

%%the server status
-export([get_connected_status/0,get_userful_conn_str/1]).
%%% listen_server:the front_server data set; connect_fail_server the health check fail server
-record(state, {listen_server,connect_fail_server}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the NLB server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], [])
	%timer:apply_interval(1000,?MODULE,check_backend_status,[])
	
	.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
	Listen_server = ets:new(listen_server, [set]),
	Target_tbl = ets:new(connect_fail_server, [set]),
	process_flag(trap_exit, true),
    {ok, #state{listen_server=Listen_server,connect_fail_server=Target_tbl}}.
%%--------------------------------------------------------------------
%% regist(Listen_server,Backend_list)  
%% the origin tcp Listen_server and target tcp Backend_list
%% Listen_server = Server_Port                         
%%                          
%% Description: Initiates the server
%%--------------------------------------------------------------------
regist(Listen_server,Backend_list)->
	gen_server:call(?MODULE, {listen_server,Listen_server,backend_list,Backend_list})
	.


%%--------------------------------------------------------------------
%%Set the current connection counter
%%--------------------------------------------------------------------
%% set_connection_count(Listen_server)->
%% 	gen_server:call(?MODULE, {set_connection_count,Listen_server})
%% 	.

%%--------------------------------------------------------------------
%%Send the message to the origin connection
%%--------------------------------------------------------------------
check_backend_status()->
	%gen_server:call(?MODULE,{check_backend_status})
	gen_server:cast(?MODULE,{check_backend_status})
	.
%%--------------------------------------------------------------------
%%Send the message to the origin connection
%%--------------------------------------------------------------------
send_back_msg(Pid,Msg)->
	gen_server:call(?MODULE, {target,Pid,Msg})
	.
%%--------------------------------------------------------------------
%%Get the backend node list connected status
%%--------------------------------------------------------------------
get_connected_status()->
	gen_server:call(?MODULE, {get_connected_status})
	.
get_userful_conn_str(Listen_server)->
	gen_server:call(?MODULE, {get_userful_conn_str,Listen_server})
	.
test()->
	gen_server:call(?MODULE, [])	
	.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

%%{origin,Origin,target,Target}
handle_call({listen_server,Listen_server,backend_list,Backend_list}, _From, State) ->
 	%io:format("{origin,~p,target,~p}\n",[Listen_server,Backend_list]),
	ets:insert(State#state.listen_server, {Listen_server,Backend_list}),
    Reply = ok,
    {reply, Reply, State};


handle_call({get_userful_conn_str,Listen_server}, _From, State) ->
	%io:format("{get_backend_connection,~p}\n",[Listen_server]),
	[{_,Arr_target}]=ets:lookup(State#state.listen_server, Listen_server),
	[X|Y] = Arr_target,
	New_back_list = erlang:append(Y,[X]),
	ets:insert(State#state.listen_server, {Listen_server,New_back_list}),
	{reply, X, State}
	;
%%{back,Pid,Msg}
handle_call({get_connected_status}, _From, State) ->
	
    Reply = {get_connected_status},
    {reply, Reply, State};
handle_call({check_backend_status}, _From, State) ->
	%io:format("State#state.listen_server="),
%% 	case ets:lookup(State#state.target_tbl, Pid) of
%% 		[{Pid,Origin_pid}] ->
%% 			Origin_pid!{send_msg,Msg};
%% 		_->
%% 			not_match
%% 	end,
%% 	 
    Reply = ok,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->

    Reply = ok,
    {reply, Reply, State}
.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({check_backend_status}, State) ->
	%io:format(" handle_cast({check_backend_status} State#state.listen_server \n"),
	check_backendlist_status_fail_server(State,ets:tab2list(State#state.connect_fail_server)),	
	check_backendlist_status(State,ets:tab2list(State#state.listen_server)),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
	case Info of
        {'EXIT', _Pid, _Why} ->
			ok;
		_->
			nothing
	end,
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% get_node_conn(Listen_server,State,[])->
%% 	ets:delete(State#state.listen_server, Listen_server),
%% 	connecion_fail
%% 	;
%% get_node_conn(Listen_server,State,[X|Y])->
%% 	{backend,Back_server,port,Back_port,maxconn,_Max_conn}= X,
%% 	case gen_tcp:connect(Back_server,Back_port,[binary, {packet, 0}]) of
%% 		{ok,Socket_target} ->
%% 			New_back_list = erlang:append(Y,[X]),
%% 			%io:format("New_back_list=~p",[New_back_list]),
%% 			ets:insert(State#state.listen_server,{Listen_server, New_back_list}),
%% 			Socket_target;
%% 		Connect_fail->
%% 			%io:format("Connect_fail=~p\n",[Connect_fail]),
%% 			Fail_list =ets:lookup(State#state.connect_fail_server, Listen_server),
%% 			New_fail_list = erlang:append(Fail_list,X),
%% 			%io:format("New_fail_list=~p",[New_fail_list]),
%% 			ets:insert(State#state.connect_fail_server, {Listen_server,New_fail_list}),
%% 			get_node_conn(Listen_server,State,Y)
%% 	end
%% 	.
check_backend_list(Listen_server,State,[],List)->
	%io:format("check_backend_listcheck_backend_listcheck_backend_list\ncheck_backend_list end list=~p\n",[List]),
	ets:insert(State#state.listen_server, {Listen_server,List}),
	ok;
check_backend_list(Listen_server,State,[X|Y],List)->
	case X of
	{backend,Back_server,port,Back_port,maxconn,_Max_conn} ->
%% 	case gen_tcp:connect(Back_server,Back_port,[binary, {packet, 0}]) of
	case gen_tcp:connect(Back_server,Back_port,[binary, {packet, 0}]) of		
		{ok,Socket_target} ->
			%io:format("Socket_target done is ~p\n",[X]),
			gen_tcp:close(Socket_target),
			check_backend_list(Listen_server,State,Y,List);
		Connect_fail->
			New_active_list = lists:delete(X, List),
%% 			io:format(" New_active_list=~p,Back_server=~p Back_port=~p  Connect_fail=~p\n",
%% 					  [New_active_list,Back_server,Back_port,Connect_fail]),
			Fail_list =ets:lookup(State#state.connect_fail_server, Listen_server),
			case Fail_list of
				[]->
					ets:insert(State#state.connect_fail_server, {Listen_server,[X]});
				[{_,Fail_list_orgin}] ->
					%io:format("Fail_list_orgin=~p\n",[Fail_list_orgin]),
					New_fail_list = erlang:append(Fail_list_orgin,[X]),
					%io:format("New_fail_list=~p\n",[New_fail_list]),
					ets:insert(State#state.connect_fail_server, {Listen_server,New_fail_list})
			end,
			check_backend_list(Listen_server,State,Y,New_active_list)
	end;
	[]->
		check_backend_list(Listen_server,State,Y,List)
	end
.

check_backendlist_status(_State,[])->
	ok;
check_backendlist_status(State,[X|Y])->
	{Listen_server,Arr}=X,
	%io:format("***************\ncheck_backendlist_status arr=~p\n*****************\n",[Arr]),
	check_backend_list(Listen_server,State,Arr,Arr),
	check_backendlist_status(State,Y)
	.

check_backendlist_status_fail_server(_State,[])->
	%io:format("check_backendlist_status_fail_server done!\n"),
	ok;
check_backendlist_status_fail_server(State,[X|Y])->
	{Listen_server,Arr}=X,
	%io:format("+++++++++++++\n Listen_server=~p arr=~p\n++++++++++++++\n",[Listen_server,Arr]),
	case ets:lookup(State#state.listen_server, Listen_server) of
		[]->
			Arr_active = [];
		[{Listen_server,Arr_active}]->
			ok
	end,
	%io:format("###################\nArr_active=~p\n###################\n",[Arr_active]),
	check_backend_status_fail_server_list(Listen_server,State,Arr,Arr,Arr_active),
	check_backendlist_status_fail_server(State,Y)	
	.

check_backend_status_fail_server_list(Listen_server,State,[],Arr_fail,Arr_active)->
	%io:format("----------------\nArr_active=~p\n-------------------\n",[Arr_active]),
	 ets:insert(State#state.listen_server, {Listen_server,Arr_active}),
	case Arr_fail of
				[]->
					%io:format("check_backend_status_fail_server_list ******************\nArr_fail = ~p\n***************\n",[Arr_fail]),
					ets:delete(State#state.connect_fail_server, Listen_server);
				_ ->
					%io:format("check_backend_status_fail_server_list ******************\nArr_fail = ~p\n***************\n",[Arr_fail]),
					ets:insert(State#state.connect_fail_server, {Listen_server,Arr_fail})
	end,
	ok
	;
check_backend_status_fail_server_list(Listen_server,State,[X|Y],Arr_fail,Arr_active)->
	case X of
	{backend,Back_server,port,Back_port,maxconn,_Max_conn} ->
		case gen_tcp:connect(Back_server,Back_port,[binary, {packet, 0}]) of	
			%%the server is up again
		{ok,Socket_target} ->
			gen_tcp:close(Socket_target),
			case Arr_active of
				[]->
					Arr_ok_new=[X];
				_->
					Arr_ok_new= erlang:append(Arr_active,[X])
			end,
			Arr_fail_new=lists:delete(X,Arr_fail),
			check_backend_status_fail_server_list(Listen_server,State,Y,Arr_fail_new,Arr_ok_new)
			;
		_Connect_fail->
			check_backend_status_fail_server_list(Listen_server,State,Y,Arr_fail,Arr_active)
		end
	end


	.