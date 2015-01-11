-module(wrapper_appmod).

%% Módulo que recibe las peticiones de yaws y realiza el manejo y
%% envío al resto de módulos.

-export([out/1]).
-include("/usr/lib/yaws/include/yaws_api.hrl").

out(Arg) ->
  % Se envía la petición handle_request con el server_path separado
  Server_Path =  string:tokens(Arg#arg.server_path, "/"),
  handle_request(string:to_lower(hd(Server_Path)), Arg).
%%   case A#arg.req#http_request.method of
%%     'GET' ->
%%       {content, "text/plain",
%%         io_lib:format("A#arg.pathinfo = ~p~n"
%%         "A#arg.prepath = ~p~n"
%%         "A#arg.querydata = ~p~n"
%%         "A#arg.req = ~p~n"
%%         "A#arg.server_path = ~p~n",
%%           [A#arg.pathinfo,
%%             A#arg.prepath,
%%             A#arg.querydata,
%%             A#arg.req,
%%             A#arg.server_path])};
%%
%%     _ -> {status, 405 }
%%   end.

handle_request("zip", Arg) ->
  case is_list(Arg#arg.pathinfo) of
    true ->   %Si pathinfo está definido
      case string:tokens(Arg#arg.pathinfo,"/") of
        [Zipcode] -> % Y tiene solo un elemento tras /zip/
          io:format("Zipcode = ~p~n", [Zipcode]),
          case zip_server:get_csv(zip_server,Zipcode) of
            {ok, Csv} ->
              {content, "text/csv", Csv};
            _ ->
              [{status, 404}, {html, io_lib:format(
                "<h1>Codigo Postal ~s no encontrado</h1><br>",
                [Zipcode])}]
          end;
        _ ->
          wrong_request(404, Arg)
      end;
    false ->
      wrong_request(404, Arg)
  end;
%%     andalso string:tokens(Arg#arg.pathinfo,"/") /=
%%     andalso length(ZipTokens) < 2
%%   of
%%     true ->
%%       %[Zipcode] = string:tokens(Arg#arg.pathinfo,"/"),
%%
%%       {content, "text/plain",
%%         io_lib:format("ZIPCODE = ~p~n", [Zipcode])};
%%     false ->
%%       wrong_request(404, Arg)
%%   end;


%%   {content, "text/plain",
%%     io_lib:format("ZIPCODE = ~p~n", Zipcode)};
%%   case Arg#arg.pathinfo of
%%     undefined -> {status, 404};
%%     Pathinfo ->
%%       Tokens
%%
%%   end;
handle_request(_, Arg) ->
  wrong_request(404, Arg).


wrong_request(Number, Arg) ->
  case Number of
    404 ->
      Error = '404 No Encontrado',
      Html = io_lib:format("<h1> Error ~s</h1><br>"
        "URL ~s no encontrada",
        [Error, Arg#arg.server_path]);
    _ ->
      Html = io_lib:format("<h1> Error ~p</h1><br>"
        "Al acceder a ~s",
        [Number, Arg#arg.server_path ])
  end,
  [{status, Number},{html, Html}].