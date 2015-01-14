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
-define(TABLA, querytable).

%%%===================================================================
%%% API
%%%===================================================================

% Arranca el módulo
start() ->
  gen_server:start_link({local,?SERVER},?MODULE, [], []).

get_result(QueryId) ->
  gen_server:call(?SERVER,{result,QueryId}).

solve_query(Field,Value) ->
  gen_server:call(?SERVER,{solve,Field,Value}).

stop() ->
  gen_server:call(?SERVER, salir).

%% Gen_server

init([]) ->
  ets:new(?TABLA, [ordered_set, named_table, protected, {keypos, 1}]),
  {ok, {}}.

%Resolver query
handle_call({solve,Field,Value}, _From, State) ->
  {reply, handle_query(Field,Value), State};
%Proporcionar resultados
handle_call({result,QueryId}, _From, State) ->
  {reply, result(QueryId), State};
%Parada
handle_call(salir, _From, State) ->
  {stop, normal, ok, State}.

%No hace nada, sólo implementa behaviour
handle_cast(_Request, State) ->
  {noreply, State}.

%Recibido resultado de un worker
handle_info({query_solver,solved,{QueryId,Field,Value,Resul}}, State) ->
  ets:insert(?TABLA,{QueryId,Field,Value,Resul}),
  {noreply, State};
% Default response
handle_info(Info, State) ->
  io:format("Unexpected message: ~p~n",Info),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%Sin uso
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Funciones privadas

%% Función principal para gestionar una query y reenviarla a query_solver
handle_query(Field,Value) ->
  % Se comprueba que el campo existe en el record zip
  AtomField = field_to_atom(Field),
  case lists:any( fun(X) -> X == AtomField end, record_info(fields,zip)) of
    true ->
      % Calculamos el número donde se recogerá la petición. Se utiliza un hash
      % para favorecer la reutilización.
      QueryNum = erlang:phash2(atom_to_list(AtomField)++Value),
      case ets:lookup(?TABLA,QueryNum) of
        [] -> %No se encuentra en la tabla, hay que  insertarlo
          %Se introduce en la tabla de resultados una tupla que indica que aún no está resuelto
          ets:insert(?TABLA,{QueryNum,AtomField,Value,unsolved}),
          query_solver:resuelve(QueryNum,AtomField,Value);
          %ets:insert(?TABLA,{QueryNum,AtomField,Value,Resul});
        [_Tupla] -> ok %Se encuentra en la tabla, nada que hacer
      end,
      {ok, QueryNum};
    false ->
      {error, badfield}
  end.

result(QueryId) ->
  case ets:lookup(?TABLA,QueryId) of
    [{QueryId, _Field, _Value, ResultList}] when is_list(ResultList) ->
      {ok, ResultList};
    [{QueryId, _Field, _Value, unsolved}] ->
      {error, notready};
    [] ->
      {error, invalidID}
  end.

% Pasa el campo de string a atom
field_to_atom(Field) ->
  list_to_atom(string:to_lower(re:replace(Field, " ", "", [{return,list}]))).


