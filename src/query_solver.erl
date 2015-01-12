-module(query_solver).
-behaviour(gen_server).

-include("zip_info.hrl").
%% API
-export([start/0, stop/0]).

%%%%%%%%%%%%%
%% TESTING %%
-export([findzips/2]).
%%%%%%%%%%%%%

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?QUERYSOLVER).
-define(ZIPTABLE, zipinfo).

-record(state, {}).

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

% Encuentra el zipcode asociado a una
findzips(Field, Value) ->
  Zip = #zip{postalcode = '$1',_='_'},
  ets:match(zipinfo, set_field(Field, Value, Zip)).

% Función para modificar el Valor de un Campo en un Record
set_field(Field, Value, Record) ->
  setelement(field_num(Field), Record, Value).

% Conocer la posición que corresponde a Field en la tupla subyacente
field_num(Field) ->
  Fields = record_info(fields, zip),
  DifField = fun (FieldName) -> Field /= FieldName end,
  case length(lists:takewhile(DifField, Fields)) of
    Length when Length =:= length(Fields) ->
      {error, not_found};
    Length ->
      Length + 2
  end.