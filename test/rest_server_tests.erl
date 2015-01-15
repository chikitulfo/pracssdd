-module(rest_server_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SERVER,"http://127.0.0.1:8888").

% Tests que hacen peticiones erróneas
badreq_test_() ->
  {"El servidor arranca y responde adecuadamente a peticiones donde no hay"
    " recursos",
    {setup, fun start/0, fun stop/1, fun badreq/1}}.

badreq(_) ->
  {inparallel,[
    %Ruta fuera de appmod
    {"/rutainexistente", matchcode(404,"/rutainexistente")},
    %Rutas de appmod sin más
    {"/zip",matchcode(404,"/zip")},
    {"/query",matchcode(404,"/query")},
    {"/result",matchcode(418,"/result")}
  ]}.

%necesario arrancar yaws y el servicio de peticiones
start() ->
  inets:start(),
  os:cmd("yaws --id test -D --conf yaws.conf"),
  wait_start().

%paramos yaws e inets tras los tests
stop(_SetupData) ->
  os:cmd("yaws --id test --stop"),
  inets:stop().

% Sólo queremos comprobar status code, contenido y headers irrelevantes
matchcode(StatusCode, Ruta) ->
  ?_assertMatch({ok,{{_,StatusCode,_},_,_}},
    httpreq(Ruta)).

httpreq(Path) ->
  httpc:request(get, {?SERVER++Path, []}, [], []).

% Esperar antes de continuar el test a que yaws esté listo
wait_start()->
  case httpreq("/") of
    {ok,{{"HTTP/1.1",_,_},_,_}} ->
      ok;
    {error,{failed_connect,_}} ->
      timer:sleep(250),
      wait_start()
  end.
%% {ok,
%%   {
%%     {"HTTP/1.1",404,"Not Found"},
%%     [ {"connection","close"},
%%       {"date","Thu, 15 Jan 2015 15:56:01 GMT"},
%%       {"server","Yaws 1.98"},
%%       {"content-length","245"},
%%       {"content-type","text/html"}
%%     ],
%%     "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\"><HTML><HEAD><TITLE>404 Not Found</TITLE></HEAD><BODY><H1>Not Found</H1>The requested URL /falsa was not found on this server.<P><HR><address> Yaws 1.98 Server at *:8888 </address>  </BODY></HTML>"
%%   }
%% }