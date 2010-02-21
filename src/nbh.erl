%% @doc Neighbour nodes search control module
%% @author Jakub Kulhan <jakub.kulhan@gmail.com>

-module(nbh).
-behaviour(application).
-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([discover/0]).

%% @doc Starts crypto server, nbh supervisor and nbh server
%% @spec () -> ok
start() ->
    crypto:start(),
    application:start(?MODULE).

%% @doc Application behaviour implementataion
%% @spec (atom(), [term()]) -> ok
start(_Type, _Args) ->
    {ok, Addr} = application:get_env(?MODULE, addr),
    {ok, Port} = application:get_env(?MODULE, port),
    {ok, TTL} = application:get_env(?MODULE, ttl),
    nbhsup:start_link(Addr, Port, TTL).

%% @doc Stops nbh supervisor (and thus also nbh server)
%% @spec () -> ok
stop() ->
    application:stop(?MODULE).

%% @doc Application behaviour implementataion
%% @spec ([term()]) -> ok
stop(_State) ->
    ok.

%% @doc Discovers all nodes on LAN with same cookie
%% @spec () -> ok
discover() ->
    nbhsrv:discover().
