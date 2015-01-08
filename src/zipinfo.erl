-module(zipinfo).
-export([leer_datos/1]).

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
%file:open("us_postal_codes.csv", [read,read_ahead]).


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


%Función auxiliar de leer_datos, encargada de leer cada línea e introducirla en la tabla.
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

% ets:new(zipinfo, [ordered_set, named_table, {keypos, #zip.zipcode}]).

% Encontrar elementos e imprimir su zipcode
% ets:match(zipinfo, #zip{zipcode='$1',place_name="Portsmouth",state_code="NH",_='_'})

% Devolver un elemento en particular, con su zipcode.
%ets:lookup(zipinfo, "00212")