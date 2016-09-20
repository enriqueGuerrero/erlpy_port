%%%-------------------------------------------------------------------
%% @author Enrique Guerrero <enrique.guerrero@hovahealth.com> [ekik]
%% @doc This module contains functions for controlling erlpy_port application, that can be started and stopped as a unit.
%% @end
%%%-------------------------------------------------------------------

-module(erlpy_port_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    PrivDir = code:priv_dir(erlpy_port),
    {ok, CowboyState} = application:get_env(erlpy_port, cowboy_http_server),
    case CowboyState of
	on->
	    {ok, HttpPort} = application:get_env(erlpy_port, http_port),
	    {ok, ServeDirectory} = application:get_env(erlpy_port,serve_directory),
	    Dispatch = cowboy_router:compile([
					      {'_', 
					       [
						{"/assets/[...]", cowboy_static, {priv_dir, erlpy_port, ServeDirectory}}
					       ]
					      }
					     ]),
	    cowboy:start_http(http, 100, [{port, HttpPort}],
			      [{env, [{dispatch, Dispatch}]}]
			     );
	_->
	    []
    end,
    erlpy_port_sup:start_link(PrivDir).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
