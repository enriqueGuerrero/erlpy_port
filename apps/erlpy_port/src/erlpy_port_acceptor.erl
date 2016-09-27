%%%-------------------------------------------------------------------
%% @author Enrique Guerrero <enrique.guerrero@hovahealth.com> [ekik]
%% @doc This module is a child that listening to TCP connection.
%% @end
%%%-------------------------------------------------------------------
-module(erlpy_port_acceptor).
-behaviour(gen_server).

%% API
-export([start/1,start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).

-define(TCP_OPTIONS, [binary, {packet, raw}, {active, false}, {reuseaddr, true}]).
-record(state,{parent, privdir,lsocket,socket, port, time, rsocket}).

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

start([])->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).


start_link({Parent,PrivDir,LSocket}) ->
    State=#state{parent=Parent,privdir=PrivDir,lsocket=LSocket},
    gen_server:start_link(?MODULE, State, []).
%%%==============================================
%%% gen_server callbacks
%%%=============================================

%%-----------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @end
%%---------------------------------------------
init(State)->
    process_flag(trap_exit,true),
    gen_server:cast(self(),listener),
    {ok,State}.

%%----------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%----------------------------------------------------------------------
handle_call(_Msg, _From, State)->
    error_logger:info_msg("Esssss"),
    {reply, ok, State}.



%%----------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%----------------------------------------------------------------------
handle_cast({execute_port,Json}, State=#state{port=Port, socket=Socket, rsocket=RSocket})->
    JsonEncode = binary_to_list(jsx:encode(Json)) ++ "\n",
    port_command(Port, JsonEncode),
    PListResponse = case collect_response(Port,[],[] ) of
	{response, Response} -> 
	    Response ++ [{status,success}];
	timeout -> 
	    error_logger:info_msg("PYTHON TIMEOUT PID ~p SOCKET ~p~n",[self(),Socket ]),
	    [{status,failed}]
    end,
    ok=gen_tcp:send(RSocket, jsx:encode(PListResponse)),
    port_close(Port),
    {noreply, State};
handle_cast({dispatcher,undefined,_Json}, State)->
    gen_server:cast(self(),stop),
    {noreply,State};
handle_cast({dispatcher,File,Json},State=#state{privdir=PrivDir})->
    PathFile = filename:join([PrivDir, File]),
    Port = open_port({spawn, "python " ++ PathFile}, [stream, {line, get_maxline()}]),
    error_logger:info_msg("PID ~p OPEN PORT ~p FILE ~p~n",[self(),Port,File]),
    {ok, Serve_directory} = ?GET_ENV(erlpy_port, serve_directory),
    PathServeDirectory = filename:join([PrivDir, Serve_directory]),
    gen_server:cast(self(),{execute_port,Json ++ [{serve_directory,list_to_binary(PathServeDirectory)}]}),
    NewState = State#state{port=Port},
    {noreply,NewState};
handle_cast(listener,State=#state{lsocket=LSocket, parent=Parent})->
    {ok, Socket} = gen_tcp:accept(LSocket),
    error_logger:info_msg("PID ~p SOCKET ACCEPTED ~p~n",[self(), Socket]),
    NewState = State#state{socket=Socket, time=os:timestamp()},
    gen_server:cast(Parent,create_child),
    ok=inet:setopts(Socket,[{active,once}]),
    {ok, SocketTimeout} = ?GET_ENV(erlpy_port,socket_timeout),
    {noreply, NewState, SocketTimeout};
handle_cast(stop, State=#state{socket=Socket})->
    ok=gen_tcp:close(Socket),
    error_logger:info_msg("PID ~p SOCKET ~p CLOSED~n",[self(),Socket]),
    {stop,normal, State};
handle_cast(_Msg,State)->
    {noreply,State}.


%%---------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
%%---------------------------------------------------------------------
handle_info({tcp_closed,Reason},State=#state{socket=Sock})->
    error_logger:info_msg("tcp_closed PID ~p SOCKET ~p REASON ~p~n",[self(),Sock,Reason]),
    gen_server:cast(self(),stop),
    {noreply,State};
handle_info({tcp_error,_Socket,Reason},State=#state{socket=Sock})->
    error_logger:info_msg("tcp_error PID ~p SOCKET ~p REASON ~p~n",[self(),Sock,Reason]),
    gen_server:cast(self(),stop),
    {noreply,State};
handle_info({tcp,Socket,Data}, State=#state{socket=Sock, privdir=_PrivDir})->
    Json = jsx:decode(Data, [{labels,atom}, return_maps]),
    Id = list_to_atom(binary_to_list(?GET_BY_DEFAULT(id,Json, <<"unset">>))),
    File = case ?GET_ENV(erlpy_port,py_files) of
	undefined->
	    undefined;
	{ok, Files}->
	    ?GET_BY_DEFAULT(Id, Files, undefined)
    end,
    gen_server:cast(self(),{dispatcher,File, Json}),
    {ok, SocketTimeout} = ?GET_ENV(erlpy_port,socket_timeout),
    NewState = State#state{rsocket=Socket},
    ok=inet:setopts(Sock,[{active,once}]),
    {noreply,NewState, SocketTimeout};
handle_info(timeout,State=#state{socket=Socket})->
    error_logger:info_msg("PID ~p SOCKET ~p TIMEOUT~n",[self(),Socket]),
    gen_server:cast(self(),stop),
    {noreply,State};
handle_info({'EXIT',Pid, Reason},State)->
    error_logger:info_msg("PROCESS ~p DOWN, REASON ~p~n",[Pid, Reason]),
    gen_server:cast(self(),stop),
    {noreply,State};
handle_info(Msg, State)->
    error_logger:info_msg("IN ~p UNKNOWN INFO MESSAGE ~p~n",[?MODULE, Msg]),
    {noreply, State}.   

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

%%====================================================================
%% Internal functions
%%==================================================================== 

%%---------------------------------------------------------------------
%% @private
%% @doc
%% Get maxline value defined in erlpy_port.schema. This value is the
%% buffer size associated with the port.
%%
%%  @end
%%--------------------------------------------------------------------
get_maxline() ->
    {ok, Value} = ?GET_ENV(erlpy_port, maxline),
    Value.

%%---------------------------------------------------------------------
%% @private
%% @doc
%% Get python_timeout value defined in erlpy_port.schema. This value is the
%% time waiting for python code. Avoid to wait infinity time to response.
%%
%%  @end
%%-------------------------------------------------------------------- 
get_python_timeout() ->
    {ok, Value} = ?GET_ENV(erlpy_port, python_timeout),
    Value.

%%---------------------------------------------------------------------
%% @private
%% @doc
%% Various types of messages are collected into a single response that 
%% can be sent back to the client.
%%
%%  @end
%%--------------------------------------------------------------------
collect_response(Port, RespAcc, LineAcc) ->
    receive
        {Port, {data, {eol, "FINISHED"}}} ->
	    [List | _] = lists:append(lists:reverse(RespAcc)),
	    Json = jsx:decode(list_to_binary(List), [{labels,atom}, return_maps]),
            {response, Json};
        {Port, {data, {eol, Result}}} ->
            Line = lists:reverse([Result | LineAcc]),
            collect_response(Port, [Line | RespAcc], []);
        {Port, {data, {noeol, Result}}} ->
            collect_response(Port, RespAcc, [Result | LineAcc])
    %% Prevent the gen_server from hanging indefinitely in case the
    %% spawned process is taking too long processing the request.
    after get_python_timeout() -> 
            timeout
    end.
