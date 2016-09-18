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
    erlpy_port_sup:start_link(PrivDir).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
