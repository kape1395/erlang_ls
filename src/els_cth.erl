-module(els_cth).

%% CTH Callbacks
-export([init/2]).

-export([ on_tc_fail/4
        , on_tc_skip/4
        ]).

-include("erlang_ls.hrl").

%% State
-type id() :: any().
-type options() :: #{uri := els_uri:uri(), line := pos_integer()}.
-type suite() :: atom().
-type testcase() :: atom() | tuple().
-type state() :: #{options := options()}.

-spec init(id(), options()) -> {ok, state()}.
init(_Id, Opts) ->
  {ok, #{options => Opts}}.

-spec on_tc_fail(suite(), testcase(), any(), state()) -> state().
on_tc_fail(_Suite, _TestCase, Reason, State) ->
  Opts = maps:get(options, State),
  Uri = maps:get(uri, Opts),
  Line = maps:get(line, Opts),
  Message = unicode:characters_to_binary(io_lib:format("~p", [Reason])),
  send_notification(Uri, Line, ?DIAGNOSTIC_ERROR, Message),
  State.

-spec on_tc_skip( suite()
                , testcase()
                , {tc_auto_skip | tc_user_skip, term()}
                , state()
                ) -> state().
on_tc_skip(_Suite, _TestCase, {_ReasonType, Reason}, State) ->
  Opts = maps:get(options, State),
  Uri = maps:get(uri, Opts),
  Line = maps:get(line, Opts),
  Message = unicode:characters_to_binary(io_lib:format("~p", [Reason])),
  send_notification(Uri, Line, ?DIAGNOSTIC_ERROR, Message),
  State.

%% TODO: Move to separate module
-spec send_notification(
        els_uri:uri(), pos_integer(), els_diagnostics:severity(), binary()) ->
        ok.
send_notification(Uri, Line, Severity, Message) ->
  Range = els_protocol:range( #{ from => {Line, 1}
                               , to => {Line + 1, 1}
                               }),
  Diagnostics = [  #{ range    => Range
                    , message  => Message
                    , severity => Severity
                    , source   => <<"Common Test">>
                    }
                ],
  Method = <<"textDocument/publishDiagnostics">>,
  Params = #{ uri         => Uri
            , diagnostics => Diagnostics
            },
  els_server:send_notification(Method, Params).
