-module(wrapper_appmod).

%% Módulo que recibe las peticiones de yaws y realiza el manejo y
%% envío al resto de módulos.

-export([out/1]).
-include("/usr/lib/yaws/include/yaws_api.hrl").

out(A) ->
  {content, "text/plain",
    io_lib:format("A#arg.appmoddata = ~p~n"
      "A#arg.appmod_prepath = ~p~n"
      "A#arg.querydata = ~p~n",
        [A#arg.appmoddata,
          A#arg.appmod_prepath,
          A#arg.querydata])}.