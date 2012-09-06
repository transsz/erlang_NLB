%%%-------------------------------------------------------------------
%%% File    : nlb_server
%%% Author  : Jacky.Chen <transsz@126.com>
%%% Description : 
%%% the connection otp service
%%% Created :  Sep 28 2012
%%%-------------------------------------------------------------------
-module(nlb_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3,test/0,new_conn/2,send_target_msg/2,send_back_msg/2]).

-record(state, {origin_tbl,target_tbl}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the NLB server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
	s:start(),
	Origin_tbl = ets:new(origin_tcp_pid, [set]),
	Target_tbl = ets:new(target_tcp_pid, [set]),
	process_flag(trap_exit, true),
    {ok, #state{origin_tbl=Origin_tbl,target_tbl=Target_tbl}}
	.
%%--------------------------------------------------------------------
%% new_conn(Origin,Target)  
%% the origin tcp connection and target tcp connection
%%                          
%%                          
%% Description: Initiates the server
%%--------------------------------------------------------------------
new_conn(Origin,Target)->
	gen_server:call(?MODULE, {origin,Origin,target,Target})
	.
%%--------------------------------------------------------------------
%%Send the message to the tareget connection
%%--------------------------------------------------------------------
send_target_msg(Pid,Msg)->
	gen_server:call(?MODULE, {origin,Pid,Msg})
	.
%%--------------------------------------------------------------------
%%Send the message to the origin connection
%%--------------------------------------------------------------------
send_back_msg(Pid,Msg)->
	gen_server:call(?MODULE, {target,Pid,Msg})
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
handle_call({origin,Origin,target,Target}, _From, State) ->
	%io:format("{origin,~p,target,~p}\n",[Origin,Target]),
	link(Origin),
	link(Target),
	ets:insert(State#state.origin_tbl, {Origin,Target}),
	ets:insert(State#state.target_tbl, {Target,Origin}),
	%io:format("ets State#state.origin_tbl info:~p \n",[ets:lookup(State#state.origin_tbl, Origin)]),
    Reply = ok,
    {reply, Reply, State};

%%{back,Pid,Msg}
handle_call({origin,Pid,Msg}, _From, State) ->
	Recodr = ets:lookup(State#state.origin_tbl, Pid),
	%io:format("ets State#state.origin_tbl info:~p \n",[ets:first(State#state.origin_tbl)]),
	
	case Recodr of
		[{Pid,Target_pid}] ->
			%io:format("Send route message...~p\n",[Target_pid]),
			Target_pid! {send_data,Msg};
		_->
			%io:format("{origin_tbl,~p,~p} not_match record=~p\n",[Pid,Msg,Recodr]),
			not_match
	end,
	 
    Reply = ok,
    {reply, Reply, State};
%%{back,Pid,Msg}
handle_call({target,Pid,Msg}, _From, State) ->
	case ets:lookup(State#state.target_tbl, Pid) of
		[{Pid,Origin_pid}] ->
			Origin_pid!{send_msg,Msg};
		_->
			not_match
	end,
	 
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
        {'EXIT', Pid, _Why} ->
			%io:format("Delete record \n ~p\n~p \n",[ets:lookup(State#state.origin_tbl, Pid),ets:lookup(State#state.target_tbl, Pid)]),
			case ets:lookup(State#state.origin_tbl, Pid) of
				[{_,TPid}]->
					ets:delete(State#state.origin_tbl, Pid),
					ets:delete(State#state.target_tbl, TPid),
					%%close the target connection process
					TPid ! {close_by_origin};
					%ets:delete(State#state.target_tbl, TPid);
				_->
					not_match
			end,
			case ets:lookup(State#state.target_tbl, Pid) of
				[{OPid,_TPid}]->
					%%close the orgin source connection process
					OPid! {close_by_target},
					ets:delete(State#state.origin_tbl, OPid),
					ets:delete(State#state.target_tbl, Pid);
				_->
					not_match
			end;
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
