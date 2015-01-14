-module(wrapper_appmod).

%% Módulo que recibe las peticiones de yaws y realiza el manejo y
%% envío al resto de módulos.

-export([out/1]).
-include("/usr/lib/yaws/include/yaws_api.hrl").

out(Arg) ->
  % Se envía la petición a handle_request con el server_path separado
  Server_Path =  string:tokens(Arg#arg.server_path, "/"),
  handle_request(string:to_lower(hd(Server_Path)), Arg).

% Petición a /zip/
handle_request("zip", Arg) ->
  case is_list(Arg#arg.pathinfo) of
    true ->   %Si pathinfo está definido
      case string:tokens(Arg#arg.pathinfo,"/") of
        [Zipcode] -> % Y tiene solo un elemento tras /zip/
          case zip_server:get_csv(Zipcode) of
            {ok, Csv} ->
              {content, "text/csv", Csv};
            _ ->
              [{status, 404}, {html, io_lib:format(
                "<h1>Codigo Postal ~s no encontrado</h1>",
                [Zipcode])}]
          end;
        _ ->
          wrong_request(404, Arg)
      end;
    false ->
      wrong_request(404, Arg)
  end;
% Petición a /query
handle_request("query", Arg) ->
  case Arg#arg.pathinfo == undefined
    andalso is_list(Arg#arg.querydata) of
    true -> %query es válido
      ParsedQuery = try yaws_api:parse_query(Arg)
                    catch error:badarg -> [] end,
      case length(ParsedQuery) of
        Num when Num /= 2 -> %Query debe tener dos tuplas
          wrong_request(400, Arg);
        2 ->
          case send_query(ParsedQuery) of
            {ok, ResponseURL} ->
              [{status, 202}, {html,
                "<h1>Accepted Query</h1>\n"
                "<a href=\"/resul/"++integer_to_list(ResponseURL)++"\">"
                  "The result can be found here.</a>\n"
              }];
            {error, _Error} ->
              wrong_request(400, Arg)
          end
      end;
    false ->
      wrong_request(418, Arg)
  end;
handle_request(_, Arg) ->
  wrong_request(404, Arg).

% Se envía la tupla a query_handler
send_query(ParsedQuery) ->
  case sort_query_tuple(ParsedQuery) of
    {error, Error} ->
      {error, Error};
    {Field, Value} ->
      query_handler:solve_query(Field,Value)
  end.

% ParsedQuery tiene dos tuplas {K,V}, se comprueba que sean "field" y "value"
% y se ordenan devolviendo sus valores en una sola tupla {Field,Value}
sort_query_tuple([{K1,V1},{K2,V2}]) ->
  case [{string:to_lower(K1),V1},{string:to_lower(K2),V2}] of
    [{"field",V1},{"value",V2}] ->
      {V1,V2};
    [{"value",V1},{"field",V2}] ->
      {V2,V1};
    _Otherwise ->
      {error, badarg}
  end.

% Petición no válida
wrong_request(Number, Arg) ->
  case Number of
    400 ->
      Error = "400 Bad Request",
      Html = io_lib:format("<h1> Error ~s</h1>~n"
        "<br>El recurso ~s no admite esa sintaxis ~s",
        [Error, Arg#arg.server_path, Arg#arg.querydata]);
    404 ->
      Error = "404 Not Found",
      Html = io_lib:format("<h1> Error ~s</h1>~n"
        "<br>URL ~s no encontrada",
        [Error, Arg#arg.server_path]);
    _ ->
      Html = io_lib:format("<h1> Error ~p ~p</h1>~n"
        "<br>Al acceder a ~s",
        [Number, yaws_api:code_to_phrase(Number), Arg#arg.server_path ])
  end,
  [{status, Number},{html, Html}].
