-module(query_solver).
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

-define(SERVER, ?QUERYSOLVER).
-define(TABLA, querytable).

%%%===================================================================
%%% API
%%%===================================================================

% Arranca el módulo
start() ->
  gen_server:start_link({local,?SERVER},?MODULE, [], []).

% Llamada para realizar el cálculo de qué zips obtienen ese resultado.
solve_query(Field, Value, QueryId) ->
  gen_server:cast(?SERVER,{solve_query,{Field,Value,QueryId}}).

stop() ->
  gen_server:call(?SERVER, salir).

%% Gen_server

init([]) ->
  ets:new(?TABLA, [ordered_set, named_table, {keypos, 1}]),
  {ok, {}}.

%Parada
handle_call(salir, _From, State) ->
  {stop, normal, ok, State}.

%Recibida solicitud de resolver
handle_cast({solve_query,{Field,Value,QueryId}}, State) ->
  solve(Field,Value,QueryId),
  {noreply, State}.

%No hace nada, sólo implementa behaviour
handle_info(Info, State) ->
  io:format("Unexpected message: ~p~n",Info),
  {noreply, State}.

terminate(_Reason, _State) ->
  ets:delete(?TABLA).

%Sin uso
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Funciones privadas

solve(Field,Value,QueryId) ->
  %Se introduce en la tabla de resultados una tupla que indica que aún no está resuelto
  ets:insert(?TABLA,{QueryId,Field,Value,unsolved}),
  %Operación costosa, simulamos con una espera
  Resul = findzips(Field,Value),
  %timer:sleep(5000),
  ets:insert(?TABLA,{QueryId,Field,Value,Resul}).



% Encuentra la lista de zipcodes de una búsqueda
findzips(Field, Value) ->
  Zip = #zip{postalcode = '$1',_='_'},
  lists:append( ets:match(zipinfo, set_field(Field, Value, Zip))).

% Función para modificar el Valor de un Campo en un Record
set_field(Field, Value, Record) ->
  setelement(field_num(Field), Record, Value).

% Conocer la posición que corresponde a un campo en la tupla subyacente
field_num(Field) ->
  Fields = record_info(fields, zip),
  DifField = fun (FieldName) -> Field /= FieldName end,
  case length(lists:takewhile(DifField, Fields)) of
    Length when Length =:= length(Fields) ->
      {error, not_found};
    Length ->
      Length + 2
  end.