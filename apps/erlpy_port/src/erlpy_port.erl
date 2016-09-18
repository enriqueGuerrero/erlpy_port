%%%-------------------------------------------------------------------
%% @author Enrique Guerrero <enrique.guerrero@hovahealth.com> [ekik]
%% @doc This module is used for TCP conections management.
%% @end
%%%-------------------------------------------------------------------
-module(erlpy_port).
-behaviour(gen_server).

%% API
-export([start_link/0,start/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).

-define(TCP_OPTIONS, [binary, {packet, raw}, {active, false}, {reuseaddr, true}]).
-define(ACCEPTOR, erlpy_port_acceptor).
-define(CHILDS,acceptor_childs).

-record(state,{lsocket,port,privdir}).

-include("../include/util.hrl").

%%%==============================================
%%% API
%%% =============================================

%%-----------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%-----------------------------------------------
start(Port, PrivDir)->
    State=#state{port=Port,privdir=PrivDir},
    gen_server:start_link({local,?MODULE},?MODULE,State,[]).

%% ---------------------------------------------
%% @doc 
%% Starts the server
%%
%% @end
%% ---------------------------------------------
start_link()->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).


%%%==============================================
%%% gen_server callbacks
%%%=============================================

%%-----------------------------------------------
%% @private
%% @doc
%% Initiates the server and socket
%%
%% @end
%%---------------------------------------------
init(State=#state{port=Port})->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok,LSocket}->
            NewState= State#state{lsocket = LSocket},
	    acceptor_childs=ets:new(?CHILDS,[set, named_table]),
	    gen_server:cast(?MODULE, create_child),
            {ok, NewState};
        {error,Reason}->
            {stop,Reason}
    end.

%%----------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%----------------------------------------------------------------------
handle_call(_Request,_From,State)->
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%----------------------------------------------------------------------
handle_cast(list_pids,State)->
    ListPids = ets:lookup(?CHILDS, list_pids),
    error_logger:info_msg("LIST PIDS ~p~n",[?GET_BY_DEFAULT(list_pids,ListPids,[])]),
    {noreply, State};
handle_cast({save_listener_child, Pid},State)->
    NewListPids = case ets:lookup(?CHILDS, pids) of
	[]->
	    [Pid];
        ListPids->
	    ?GET_BY_DEFAULT(list_pids,ListPids,[]) ++ [Pid]
    end,
    true=ets:insert(?CHILDS,{list_pids,NewListPids}),
    true=ets:insert(?CHILDS, {Pid,[ {pid, Pid}, {time, os:timestamp()} ]}),
    {noreply, State};
handle_cast(create_child, State=#state{lsocket=LSocket,privdir=PrivDir})->
    {ok, Pid} = ?ACCEPTOR:start_link({self(),PrivDir,LSocket}),
    erlang:monitor(process, Pid),
    gen_server:cast(?MODULE,{save_listener_child,Pid}),
    {noreply,State};
handle_cast(_Msg,State)->
    {noreply,State}.

%%---------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
%%---------------------------------------------------------------------
handle_info({'DOWN',_Ref,process,Pid,Reason},State)->
    error_logger:info_msg("PROCESS ~p DOWN, REASON ~p~n",[Pid, Reason]),
    true = ets:delete(?CHILDS, Pid),
    ListPids = ets:lookup(?CHILDS,list_pids),
    NewListPids = [ Element || Element <-?GET_BY_DEFAULT(list_pids,ListPids,[]), Element=/=Pid ],
    true=ets:insert(?CHILDS,{list_pids,NewListPids}),
    {noreply, State};
handle_info({'EXIT', _Pid, _Reason}, State)->
    {noreply,State};
handle_info(Msg,State)->
    error_logger:info_msg("IN ~p UNKNOWN INFO MESSAGE ~p~n",[?MODULE, Msg]),
    {noreply,State}.

%%---------------------------------------------------------------------
%% @private 
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module: init/1 and do any
%% necessary cleaning up. When ir returns, the gen_Server terminates
%% with Reason. The return value is ignored.
%% 
%% @end
%%---------------------------------------------------------------------
terminate({socket_terminated, _Reason}, _State) ->
    ok;
terminate(_Reason,_State)->
    ok.

%%---------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%---------------------------------------------------------------------
code_change(_OldVsn,State,_Extra)->
    {ok,State}.
