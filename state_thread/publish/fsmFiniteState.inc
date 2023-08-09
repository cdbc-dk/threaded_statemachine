{%MainUnit fsm_engine.pas}
{ (c)2023 Benny Christensen a.k.a. cdbc }

{ TFiniteState }
function TFiniteState.get_ParentFSM: TFiniteStatemachine;
begin
  Result:= fParentFSM;
end;

function TFiniteState.get_StateValues: PfsmInternalProgressRec;
begin { !!!WARNING, should only happen when thread is sleeping!!! }
  Result:= @fStateValues;
end;

procedure TFiniteState.set_StateValues(aValue: PfsmInternalProgressRec);
begin { !!!WARNING, should only happen when thread is sleeping!!! }
  if aValue <> @fStateValues then fStateValues.Assign(aValue);
end;

procedure TFiniteState.ChangeState(aStateCls: TFiniteStateClass);
begin
  fParentFSM.RegChangeState(aStateCls);
end;

function TFiniteState.GetExecState: TfsmExecutionState;
begin { to be called thread-internal only }
  fParentFSM.Sem.Wait; try
    Result:= fStateValues.ipExecState;
  finally fParentFSM.Sem.Signal; end;
end;

procedure TFiniteState.NotifyParent(aStr: shortstring); // pure value param
begin { this happens in-thread and the queueing is thread-safe, no worries }
  fParentFSM.Notify(TbcMessage.Create(-1,1234,-1,-1,aStr));
end;

procedure TFiniteState.SetExecState(aValue: TfsmExecutionState);
begin { to be called thread-internal only }
  fParentFSM.Sem.Wait; try
    fStateValues.ipExecState:= aValue;
  finally fParentFSM.Sem.Signal; end;
end;

constructor TFiniteState.create(aFSM: TFiniteStateMachine);
begin
  inherited Create;
  fParentFSM:= aFSM;
end;

destructor TFiniteState.Destroy;
begin
  fParentFSM:= nil;
  inherited Destroy;
end;

procedure TFiniteState.Abort; { to be called thread-external only }
begin { for explanation about missing sync, see "Pause" }
  fStateValues.Abort;
end;

///////////////////////////////////////////////////////////////////////
// this method serves as an example of how to check status underway, //
// also it provides an insight into the mechanism behind Abort/Pause //
// !!! MUST ALWAYS BE OVERRIDDEN !!! }                               //
///////////////////////////////////////////////////////////////////////
procedure TFiniteState.Execute;
var I: integer;
begin
  { check state and initialize values accordingly }  //fStateValues.ipExecState
  case GetExecState of
    esPause: begin
               I:= fParentFSM.Tag;
               // ...
             end;
    esNone,
    esStart: begin
               I:= 0;
               fParentFSM.Tag:= 0;
             end;
  end;
  SetExecState(esWorking); { we set it ourselves, so others can query it }
  while I < 125000  do begin
    //.... working hard :o)
    fParentFSM.OutQ.EnQueue(pointer(I)); // adds A LOT of integers
    sleep(1); // msecs
    if (I mod 7000) = 0 then begin

      fParentFSM.Notify(TbcMessage.Create(-1,1311,0,I,'Counting MSecs :o)'));
    end;
    { for longer operations we check our statevalues periodically }
    case GetExecState of
      esAbort: exit; // exit cold and hard
      esPause: begin // save values and exit, no need to set esPause, is already set
                 fParentFSM.Tag:= I;
                 exit;
               end;
    end;
    inc(I);
  end; // while
  { finalize values and signal we're done }
  fParentFSM.OutQ.EnQueue(pointer(I)); // remember the last one
  SetExecState(esNone);
end; { this method serves as an example of how to check status underway }

procedure TFiniteState.Pause; { to be called thread-external only }
begin { to avoid dead-lock, i've moved sync-call to statemachine, }
  fStateValues.Pause; { this is because "Get/Set-ExecState" calls }
end; { sync in-thread, which at that time, this class also is...  }


// included by fsm_engine.pas
