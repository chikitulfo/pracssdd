-module(query_solver).

-include("zip_info.hrl").
%% API
-export([resuelve/3]).

%%%%%%%%%%%%%
%% TESTING %%
%-compile(export_all).
%%%%%%%%%%%%%

-define(TABLA, zipinfo).

%% API

resuelve(QueryId,Field,Value) ->
  From = self(),
  spawn_link(fun() -> resolver(From,{QueryId,Field,Value}) end).
  %spawn_link(?MODULE, resolver, [self(),{QueryId,Field,Value}]).

%% Funciones privadas

resolver(From,{QueryId,Field,Value}) ->
  Resul = findzips(Field,Value),
  %Simulamos la operación costosa con 7s de espera
  timer:sleep(7000),
  From ! {query_solver,solved,{QueryId,Field,Value,Resul}},
  ok.

% Encuentra la lista de zipcodes de una búsqueda
findzips(Field, Value) ->
  Zip = #zip{postalcode = '$1',_='_'},
  lists:append( ets:match(?TABLA, set_field(Field, Value, Zip))).

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