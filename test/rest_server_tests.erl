-module(rest_server_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SERVER,"http://127.0.0.1:8888").

% No tiene tests, solo arranca el cliente http
% y espera para asegurarse de que yaws está iniciado.
setup_test() ->
  inets:start(),
  wait_start().

% Tests que hacen peticiones erroneas
badreq_test_() ->
  {"El servidor arranca y responde adecuadamente a peticiones donde no hay"
    " recursos",
    badreq()}.

% Tests al subsistema /zip/
zip_test_() ->
  {"Responde a peticiones de zip, tanto adecuadas como erroneas",
    zipreq()}.

% Tests al subsistema /query/
queries_test_() ->
  {"Peticiones de query, sin comprobar resultados",
    queryreq()}.

% Test que muestra el sistema en funcionamiento
demostracion_test_()->
  ?debugFmt("~nPasamos ahora a una demostracion de funcionamiento completo.~n"
    "Se hace una peticion al servicio para codigos postales cuyo nombre~n"
    "sea Alhambra:",[]),
  timer:sleep(200),
  %Hacer peticion, obtener su result y comprobar resultados
  Query = "/query?field=Place%20Name&value=Alhambra",
  {ok,{{_, StatusCodeQ, StatusMsgQ},_Headers, ContenidoQ}} = httpreq(Query),
  ?debugFmt("~nSolicitud: ~p~n"
      "Respuesta: ~p ~s~n~s",[Query, StatusCodeQ, StatusMsgQ, ContenidoQ]),

  %Obtenemos la URL de resultado.
  Result = grep(ContenidoQ,"(?<=<a href=)/result/[0-9]+(?=>)"),
  ?debugFmt("~nPreguntamos por el resultado ~p",[Result]),
  {ok,{{_,StatusCodeR,StatusMsgR},_,_}} = httpreq(Result),
  ?debugFmt("~nSolicitud: ~p~n"
  "Respuesta: ~p ~s~n",[Result,StatusCodeR,StatusMsgR]),
  %Esperamos a la respuesta
  ?debugFmt("~nEsperamos 7s a que la respuesta este lista.~n",[]),
  timer:sleep(7500),
  {ok,{{_,StatusCodeR2,StatusMsgR2},_,ContenidoR2}} = httpreq(Result),
  ?debugFmt("~nSolicitud: ~p~n"
  "Respuesta: ~p ~s~n~s~n",[Result,StatusCodeR2,StatusMsgR2, ContenidoR2]),
  %Obtenemos el zip
  ZipUrl = grep(ContenidoR2,"(?<=<a href=)/zip/[0-9]+(?=>)"),
  ?debugFmt("~nObtenemos el primer zip: ~p~n",[ZipUrl]),
  {ok,{{_,StatusCodeZ,StatusMsgZ},_,ContenidoZ}} = httpreq(ZipUrl),
  ?debugFmt("~nSolicitud: ~p~n"
  "Respuesta: ~p ~s~n~s~n",[ZipUrl,StatusCodeZ,StatusMsgZ, ContenidoZ]),
  timer:sleep(200),
  %Comprobamos ahora que una peticion identica no necesita trabajar de nuevo
  ?debugFmt("~nRepetimos la query inicial, para comprobar que~n"
    "esta vez el resultado esta disponible de inmediato:~n"
    "~p~n",[Query]),
  {ok,{{_, StatusCodeQ2, StatusMsgQ2},_, ContenidoQ2}} = httpreq(Query),
  Result2 = grep(ContenidoQ2,"(?<=<a href=)/result/[0-9]+(?=>)"),

  ?debugFmt("~nPreguntamos por el resultado ~p",[Result2]),
  {ok,{{_,StatusCodeR3,StatusMsgR3},_,ContenidoR3}} = httpreq(Result2),
  ?debugFmt("~nSolicitud: ~p~n"
  "Respuesta: ~p ~s~n",[Result2,StatusCodeR3,StatusMsgR3]),

  [
    {"Query responde 202",
      ?_assertMatch({202,"Accepted"},{StatusCodeQ, StatusMsgQ})},
    {"Result no listo [204]",
      ?_assertMatch({204,"No Content"},{StatusCodeR,StatusMsgR})},
    {"Result OK [200]",
      ?_assertMatch({200,"OK"},{StatusCodeR2,StatusMsgR2})},
    {"ZipCode OK [200]",
      ?_assertMatch({200,"OK"},{StatusCodeZ,StatusMsgZ})},
    {"Nueva query responde 202",
      ?_assertMatch({202,"Accepted"},{StatusCodeQ2, StatusMsgQ2})},
    {"Resultado identico al anterior",
      ?_assertEqual(ContenidoQ,ContenidoQ2)},
    {"Result OK [200]",
      ?_assertMatch({200,"OK"},{StatusCodeR3,StatusMsgR3})},
    {"Lista de zips idéntica a la anterior",
      ?_assertEqual(ContenidoR2,ContenidoR3)}

  ].

%% Funciones auxiliares

badreq() ->
  {inparallel,[
    %Ruta fuera de appmod
    {"/rutainexistente [404]", matchcode(404,"/rutainexistente")},
    %Rutas de appmod sin mas
    {"/zip [404]",matchcode(404,"/zip")},
    {"/query [404]",matchcode(404,"/query")},
    {"/result [418]",matchcode(418,"/result")}
  ]}.

zipreq() ->
  {inparallel,[
    %Zip correcto,
    {"46176 (devuelve csv)", fun() -> ?_assertMatch({ok,{{_,200,_},
      [{"date",_},
        {"server","Yaws 1.98"},
        {"content-length",_},
        {"content-type","text/csv"}],_Contenido}},
      httpreq("/zip/46176")) end},
    %Rutas de error
    {"00288 [404]",matchcode(404,"/zip/00288")},
    {"hola [404]",matchcode(404,"/zip/hola")},
    {"46176/23 [404]",matchcode(404,"/zip/46176/23")}
  ]}.

queryreq() ->
  {inparallel,[
    %Queries mal formadas
    {"Caracteres raros [400]",
      matchcode(400,"/query?ads+as-/()&$S%ef")},
    {"No field ni value [400]",
      matchcode(400,"/query?hola=lala&adios=charmander")},
    {"Field y value inexistente [400]",
      matchcode(400,"/query?field=&value=")},
    {"Field no valido [400]",
      matchcode(400,"/query?field=inventado&value=daigual")},
    %Queries adecuadas
    {"Field&Value validos [202]",
      matchcode(202,"/query?field=Place%20Name&value=charmander")},
    {"Value&Field validos (orden inverso) [202]",
      matchcode(202,"/query?value=charmander&field=State")},
    %Mayusculas y minusculas da igual
    {"Value&Field validos (independiente de capitalizacion) [202]",
      matchcode(202,"/query?vAlUe=charmander&FIelD=State")}
    ]}.

% Solo queremos comprobar status code, contenido y headers irrelevantes
matchcode(StatusCode, Ruta) ->
  ?_assertMatch({ok,{{_,StatusCode,_},_,_}},
    httpreq(Ruta)).

httpreq(Path) ->
  httpc:request(get, {?SERVER++Path, []}, [], []).

% Esperar antes de continuar el test a que yaws este listo
wait_start()->
  case httpreq("/") of
    {ok,{{"HTTP/1.1",_,_},_,_}} ->
      ok;
    {error,{failed_connect,_}} ->
      timer:sleep(250),
      wait_start()
  end.

grep(String, Regex) ->
  {match,[{Init,Len}|_]} = re:run(String,Regex),
  string:substr(String,Init+1,Len).
