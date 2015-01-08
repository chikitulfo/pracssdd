-module(zipinfo).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(zip,{zipcode,
                 place_name,
                 state,
                 state_code,
                 county,
                 latitude,
                 longitude}).
-define(CsvHeader, "Postal Code,Place Name,State,State Abbreviation,County,Latitude,Longitude,\n").
% Módulo encargado de dar forma y enviar el csv que contiene la información del
% zipcode consultado.
% Postal Code,Place Name,State,State Abbreviation,County,Latitude,Longitude,

init(Args) ->
  erlang:error(not_implemented).

handle_call(Request, From, State) ->
  erlang:error(not_implemented).

handle_cast(Request, State) ->
  erlang:error(not_implemented).

handle_info(Info, State) ->
  erlang:error(not_implemented).

terminate(Reason, State) ->
  erlang:error(not_implemented).

code_change(OldVsn, State, Extra) ->
  erlang:error(not_implemented).


% Lee los datos del archivo y los introduce en la tabla.
leer_datos(Tabla) ->
  case file:open("us_postal_codes.csv", [read,read_ahead]) of
    {ok, Fd} ->
      %Leer la primera linea y comprobar header
      {ok, Header} = file:read_line(Fd),
      case string:equal( Header, ?CsvHeader) of
        false -> throw({wrongfileformat});
        true ->
          try leer_lineas(Fd,Tabla) %Leer el archivo
          after file:close(Fd)
          end
      end;

    {error, Error} -> throw(Error)
  end.


% Función auxiliar de leer_datos, encargada de leer cada línea e introducirla en la tabla.
leer_lineas(Fd,Tabla) ->
  case file:read_line(Fd) of
    eof  -> {leido};
    {ok, Linea} ->
      Tokens = string:tokens(Linea, ","),
      case re:run(hd(Tokens), "^[0-9]{5}$") of %Asegurarnos de que el campo zip son 5 dígitos
        {match, _} ->
          ZipRecord = list_to_tuple([zip|lists:sublist(Tokens,7)]),
          ets:insert(Tabla, ZipRecord),
          leer_lineas(Fd, Tabla);
        {nomatch} -> throw({wrongfileformat})
      end;
    {error, Error} -> throw(Error)
  end.

% Dado un zipcode y la tabla ets, produce un string de archivo csv con la línea
% de header y una segunda línea con la información del zipcode solicitado.
getCsv(Tabla, Zipcode) ->
  [ZipRecord] = ets:lookup(Tabla, Zipcode),
  CsvEntry = string:join( tl( tuple_to_list(ZipRecord)),",") ++ ",\n",
  ?CsvHeader++CsvEntry.


% ets:new(zipinfo, [ordered_set, named_table, {keypos, #zip.zipcode}]).

% Encontrar elementos e imprimir su zipcode
% ets:match(zipinfo, #zip{zipcode='$1',place_name="Portsmouth",state_code="NH",_='_'})

% Devolver un elemento en particular, con su zipcode.
%ets:lookup(zipinfo, "00212")
