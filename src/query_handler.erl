-module(query_handler).
-behaviour(gen_server).

-include("zip_info.hrl").
%% API
-export([start/0, stop/0]).

%%%%%%%%%%%%%
%% TESTING %%
-compile(export_all).
%%%%%%%%%%%%%

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?QUERYHANDLER).

%%%===================================================================
%%% API
%%%===================================================================

% Arranca el módulo
start() ->
  gen_server:start_link({local,?SERVER},?MODULE, [], []).


solve_query(Field,Value) ->
  gen_server:call(?SERVER,{solve,Field,Value}).

stop() ->
  gen_server:call(?SERVER, salir).

%% Gen_server

init([]) ->
  {ok, {}}.

%Resolver query
handle_call({solve,Field,Value}, _From, State) ->
  {reply, handle_query(Field,Value), State};
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

terminate(_Reason, _State) ->
  ok.

%Sin uso
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Funciones privadas

%% Función principal para gestionar una query y reenviarla a query_solver
handle_query(Field,Value) ->
  % Se comprueba que el campo existe en el record zip
  AtomField = field_to_atom(Field),
  case lists:any( fun(X) -> X == AtomField end, record_info(fields,zip)) of
    true ->
      % Calculamos el número donde se recogerá la petición. Se utiliza un hash
      % para favorecer la reutilización.
      QueryNum = erlang:phash2(atom_to_list(AtomField)++Value),
      query_solver:solve_query(AtomField,Value,QueryNum),
      {ok, QueryNum};
    false ->
      {error, badfield}
  end.

% Pasa el campo de string a atom
field_to_atom(Field) ->
  list_to_atom(string:to_lower(re:replace(Field, " ", "", [{return,list}]))).


