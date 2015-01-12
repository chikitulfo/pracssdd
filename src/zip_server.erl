-module(zip_server).
-behaviour(gen_server).

% Interfaz de uso
-export([start/0, get_csv/1,stop/0]).

% Callbacks de gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Módulo encargado de dar forma y enviar el csv que contiene la información del
% zipcode consultado.
-record(zip,{zipcode, place_name, state, state_code, county, latitude, longitude}).
-define(CsvHeader, "Postal Code,Place Name,State,State Abbreviation,County,Latitude,Longitude,\n").
-define(CsvFile, "data/us_postal_codes.csv").

% Arranca el módulo
start() ->
  gen_server:start_link({local,?MODULE},?MODULE, [], []).


% Llamada para obtener un csv de un zipcode
get_csv(Zipcode) ->
  gen_server:call(?MODULE,{getCsv,Zipcode}).

stop() ->
  gen_server:call(?MODULE, salir).

%% Gen_server

init([]) ->
  ets:new(zipinfo, [ordered_set, named_table, {keypos, #zip.zipcode}]),
  try leer_datos(zipinfo)
  catch
    throw:wrongfileformat -> {stop, "Formato incorrecto en el archivo de entrada "++?CsvFile++"."}
  end,
  {ok, {zipinfo}}.

%Gestionar llamada de obtener csv
handle_call({getCsv, Zipcode}, _From, {Tabla}) ->
  {reply, build_csv(Tabla,Zipcode), {Tabla}};
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


%% Funciones Privadas

% Lee los datos del archivo y los introduce en la tabla.
leer_datos(Tabla) ->
  case file:open(?CsvFile, [read,read_ahead]) of
    {ok, Fd} ->
      %Leer la primera linea y comprobar header
      {ok, Header} = file:read_line(Fd),
      case string:equal( Header, ?CsvHeader) of
        false -> throw(wrongfileformat);
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
        {nomatch} -> throw(wrongfileformat)
      end;
    {error, Error} -> throw(Error)
  end.

% Dado un zipcode y la tabla ets, produce una tupla {ok, string} de archivo csv con la línea
% de header y una segunda línea con la información del zipcode solicitado.
% En caso de no encontrar el zipcode introducido, devuelve {error, notfound}
build_csv(Tabla, Zipcode) ->
  case ets:lookup(Tabla, Zipcode) of
    [ZipRecord] ->
      CsvEntry = string:join( tl( tuple_to_list(ZipRecord)),",") ++ ",\n",
      {ok, ?CsvHeader++CsvEntry};
    [] -> {error, notfound}
  end.

