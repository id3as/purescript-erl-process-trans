-module(erl_processT_trapExitT@foreign).

-export([ parseTrappedExitFFI/2
        ]).

parseTrappedExitFFI({ 'EXIT', Pid,  Reason }, ExitMsg) ->
  {just, (ExitMsg(Pid))(Reason)};

parseTrappedExitFFI(_,_) ->
  {nothing}.
