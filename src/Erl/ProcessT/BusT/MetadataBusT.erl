-module(erl_processT_busT_metadataBusT@foreign).

-export([ createImpl/3
        , deleteImpl/2
        , raisePrimeImpl/3
        , updateMetadataImpl/2
        , subscribeImpl/1
        , monitorImpl/2
        , demonitorImpl/1
        , unsubscribeImpl/2
        , parseBusMsg/1
        ]).

-include_lib("gproc/src/gproc_int.hrl").

-define(left(X), {left, X}).
-define(right(X), {right, X}).
-define(just(X), {just, X}).
-define(nothing, {nothing}).
-define(unit, unit).

-define(metadataBusTag, metadataBusT).
-define(gprocPropertyKey(Name), {p,l,{?metadataBusTag, Name}}).
-define(gprocNameKey(Name),     {n,l,{?metadataBusTag, Name}}).

-define(metadataTag, metadata).
-define(metadataAttribute(Generation, Metadata), {?metadataTag, {Generation, Metadata}}).
-record(dataMsgInternal, {generation, msg}).
-record(metadataMsgInternal, {generation, msg}).

-define(msgTag, '__MetadataBusTMsg').
-define(monitorTag, '__MetadataBusTMonitor').

createImpl(BusName, Generation, InitialMetadata) ->
  fun() ->
      NameKey = ?gprocNameKey(BusName),
      gproc:reg(NameKey, undefined, [?metadataAttribute(Generation, InitialMetadata)]),
      raiseMsgInt(BusName, #metadataMsgInternal{generation = Generation, msg = InitialMetadata}, ?nothing),
      BusName
  end.

deleteImpl(BusName, TerminatedMsg) ->
  fun() ->
      NameKey = ?gprocNameKey(BusName),
      {{Id, Generation}, _Metadata} = gproc:get_attribute(NameKey, ?metadataTag),
      gproc:unreg(?gprocNameKey(BusName)),
      raiseMsgInt(BusName, TerminatedMsg({Id, Generation + 1}), ?nothing),
      ?unit
  end.

raisePrimeImpl(BusName, Msg, #{beforeEachSend := BeforeEachSend}) ->
  fun() ->
    NameKey = ?gprocNameKey(BusName),
    {{Id, Generation}, _} = gproc:get_attribute(NameKey, ?metadataTag),
    raiseMsgInt(BusName, #dataMsgInternal{generation = {Id, Generation}, msg = Msg}, BeforeEachSend)
  end.

updateMetadataImpl(BusName, NewMetadata) ->
  fun () ->
    NameKey = ?gprocNameKey(BusName),
    {{Id, Generation}, _} = gproc:get_attribute(NameKey, ?metadataTag),
    NewGeneration = {Id, Generation + 1},
    gproc:set_attributes(NameKey, [?metadataAttribute(NewGeneration, NewMetadata)]),
    raiseMsgInt(BusName, #metadataMsgInternal{generation = NewGeneration, msg = NewMetadata}, ?nothing)
  end.

subscribeImpl(BusName) ->
  fun() ->
      true = gproc:reg(?gprocPropertyKey(BusName)),
      Pid = gproc:where(?gprocNameKey(BusName)),
      case Pid of
        undefined -> ?nothing;
        _ ->
          Ref = (monitorImpl(Pid, BusName))(),
          try
            {Gen, Metadata} = gproc:get_attribute(?gprocNameKey(BusName), Pid, ?metadataTag),
            ?just({Gen, Metadata, Ref})
          catch
            error:badarg ->
              (demonitorImpl(Ref))(),
              ?nothing
          end
      end
  end.

monitorImpl(Pid, BusName) ->
  fun () -> erlang:monitor(process, Pid, [{tag, {?monitorTag, BusName}}]) end.

demonitorImpl(Ref) ->
  fun () -> erlang:demonitor(Ref, [flush]) end.


%%------------------------------------------------------------------------------
%% raiseMsgInt and groc_send1 are slight variants of send and send1 
%% from gproc.erl - but call the passed function before each message send
%%------------------------------------------------------------------------------
raiseMsgInt(BusName, Msg, BeforeEachSend) ->
  gproc_send1(?gprocPropertyKey(BusName), {?msgTag, BusName, Msg, self()}, BeforeEachSend).

%%------------------------------------------------------------------------------
%% gproc_send1 is a slight variant of send in gproc.erl, adding in support for 
%% an optional side effect prior to each message send (for example to reference count) 
%%------------------------------------------------------------------------------
gproc_send1({T,C,_} = Key, Msg, MaybeSideEffect) when C==l; C==g ->
    if T==p orelse T==c orelse T==r ->
            lists:foreach(fun(Pid) ->
                                  maybe_run(MaybeSideEffect),
                                  Pid ! Msg
                          end, gproc:lookup_pids(Key)),
            Msg;
       true ->
            erlang:error(badarg)
    end;
gproc_send1(_, _, _) ->
    ?THROW_GPROC_ERROR(badarg).


maybe_run(?nothing) -> ok;
maybe_run(?just(SideEffect)) ->
  SideEffect().


% sender exits
% immediately calling unsubscribe from other thread
% unreg message not even on the queue yet, would be received later
% causing conflicts if there's like a GprocT also receiving those kinds of message
% (if we delete the Key from the map)
unsubscribeImpl(MaybeRef, BusName) ->
  fun() ->
      case MaybeRef of
        ?just(Ref) -> (demonitorImpl(Ref))();
        ?nothing -> ok
      end,
      gproc:unreg(?gprocPropertyKey(BusName)),
      ?unit
  end.

parseBusMsg({?msgTag, Name, Msg, Pid}) ->
  ?just(?left({Name, Msg, Pid}));
parseBusMsg({{?monitorTag, BusName}, _MonitorRef, _MonitorType, _MonitorObject, _MonitorInfo}) ->
  ?just(?right(BusName));
parseBusMsg(_) ->
  ?nothing.
