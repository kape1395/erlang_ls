%%==============================================================================
%% Code Lens: server_info
%%==============================================================================

-module(els_code_lens_server_info).

-behaviour(els_code_lens).
-export([ command/1
        , command_args/2
        , is_default/0
        , pois/1
        , precondition/1
        , title/1
        ]).

-include("erlang_ls.hrl").

-spec command(poi()) -> els_command:command_id().
command(_POI) ->
  <<"server-info">>.

-spec command_args(els_dt_document:item(), poi()) -> [any()].
command_args(_Document, _POI) ->
  [].

-spec is_default() -> boolean().
is_default() ->
  false.

-spec precondition(els_dt_document:item()) -> boolean().
precondition(_Document) ->
  true.

-spec pois(els_dt_document:item()) -> [poi()].
pois(Document) ->
  case els_dt_document:pois(Document) of
    [] -> [];
    [H|_] -> [H]
  end.

-spec title(poi()) -> binary().
title(_POI) ->
  Root = filename:basename(els_uri:path(els_config:get(root_uri))),
  <<"Erlang LS (in ", Root/binary, ") info">>.
