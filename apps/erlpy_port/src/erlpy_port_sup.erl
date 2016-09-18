%%%-------------------------------------------------------------------
%% @author Enrique Guerrero <enrique.guerrero@hovahealth.com> [ekik]
%% @doc This module is responsible for starting, stopping, and monitoring its child processes. Supervisor is that it is to keep its child processes alive by restarting them when necessary.
%% @end
%%%-------------------------------------------------------------------
 
-module(erlpy_port_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I,Type,Args), {I,{I,start,Args},permanent, 2000,Type,[I]}).

-include("../include/util.hrl").

%%====================================================================
%% API functions
%%====================================================================

start_link(ExtFiles) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, ExtFiles).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(PrivDir) ->
    {ok, Port} = ?GET_ENV(erlpy_port, port),
    Args = [Port, PrivDir],
    Childs=[?CHILD(erlpy_port,worker,Args)],
    {ok,{ {one_for_one,1000,3600 },Childs}}.

%%====================================================================
%% Internal functions
%%====================================================================
