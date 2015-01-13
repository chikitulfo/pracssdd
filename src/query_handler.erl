-module(query_handler).
-behaviour(gen_server).

-include("zip_info.hrl").
%% API
-export([start/0, stop/0]).

%%%%%%%%%%%%%
%% TESTING %%
-compile(export_all).
%%%%%%%%%%%%%

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?QUERYHANDLER).

%%%===================================================================
%%% API
%%%===================================================================

% Arranca el módulo
start() ->
  gen_server:start_link({local,?SERVER},?MODULE, [], []).


% Llamada para obtener un csv de un zipcode
get_csv(Zipcode) ->
  gen_server:call(?SERVER,{getCsv,Zipcode}).

stop() ->
  gen_server:call(?SERVER, salir).

%% Gen_server

init([]) ->
  ets:new(querytable, [ordered_set, named_table, {keypos, 1}]),
  {ok, {}}.

%Parada
handle_call(salir, _From, State) ->
  {stop, normal, ok, State}.

%No hace nada, sólo implementa behaviour
handle_cast(_Request, State) ->
  {noreply, State}.

%No hace nada, sólo implementa behaviour
handle_info(Info, State) ->
  io:format("Unexpected message: ~p~n",Info),
  {noreply, State}.

terminate(_Reason, {Tabla}) ->
  ets:delete(Tabla).

%Sin uso
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
