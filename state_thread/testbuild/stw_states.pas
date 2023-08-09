{ execution states for FiniteStateMachine pertaining to stopwatch }
unit stw_states;
{$mode ObjFPC}{$H+}
{ ©2023 Benny Christensen a.k.a. cdbc }

interface
uses
  Classes, SysUtils, fsm_engine;
const
  unitVersion = '0.31.07.2023'; { 1.st version }
type
  { TstwStopState A concrete state object used when starting or idle'ing, reset all values }
  TstwStopState = class(TFiniteState)
  public
    procedure Execute; override;
  end;

  { TstwCountState A concrete state object used to count msecs }
  TstwCountState = class(TFiniteState)
  public
    procedure Execute; override;
  end;

  { TstwPauseState A concrete state object used to pause counting msecs }
  TstwPauseState = class(TFiniteState)
  public
    procedure Execute; override;
  end;

  { TstwErrorState A concrete state object used when some error occurred }
  TstwErrorState = class(TFiniteState)
  public
    procedure Execute; override;
  end;
{ registers the states, needed to parse acc-files, with the statemachine and spits out the statenames as well }
procedure RegisterSTWStates(aStateMachine: TFiniteStateMachine;out StateNames: TStringArray);

implementation

procedure RegisterSTWStates(aStateMachine: TFiniteStateMachine;out StateNames: TStringArray);
begin
  SetLength(StateNames,4);
  aStateMachine.RegisterState(TstwStopState.Create(aStateMachine)); // idx 0 "start-state" must be registered first!
  StateNames[0]:= TstwStopState.ClassName;
  aStateMachine.RegisterState(TstwCountState.Create(aStateMachine)); // idx 1
  StateNames[1]:= TstwCountState.ClassName;
  aStateMachine.RegisterState(TstwPauseState.Create(aStateMachine)); // idx 2
  StateNames[2]:= TstwPauseState.ClassName;
  aStateMachine.RegisterState(TstwErrorState.Create(aStateMachine)); // idx 3
  StateNames[3]:= TstwErrorState.ClassName;
end; { RegisterSTWStates }

{ TstwPauseState }
procedure TstwPauseState.Execute; { not needed }
begin
  // actually, there's nothing to do here, 'cause the mechanics are all done in
  // the statemachine and our ancestor
end;

{ TstwCountState }
procedure TstwCountState.Execute;
var I: integer;
begin
  { check state and initialize values accordingly }
  case GetExecState of
    esPause: begin
               I:= fParentFSM.Tag;
               // ...
             end;
    esAbort,esNone,
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
      { shamelessly using the parent-messagequeue, it's what it's there for }
      fParentFSM.MQ.PostMsg(-1,1311,0,I,'Counting MSecs :o)');
    end;
    { for longer operations we check our statevalues periodically,
      if not = NO "Abort" or "Pause" }
    case GetExecState of
      esAbort: begin { "soft" state-switch, within the lock ;-) }
                 fParentFSM.Tag:= 0; // leave it in the state we found it
                 ChangeState(TstwStopState);
                 exit; // exit cold and hard
               end;
      esPause: begin // save values and exit, no need to set esPause, is already set
                 fParentFSM.Tag:= I;
                 exit;
               end;
    end;
    inc(I);
  end; // while
  { finalize values and signal we're done }
  fParentFSM.Tag:= 0; // leave it in the state we found it
  fParentFSM.OutQ.EnQueue(pointer(I)); // remember the last one
  { this last msg I check for in gui, then I know when to reset controls }
  fParentFSM.MQ.PostMsg(-1,1618,0,0,'TstwCountState has finished.'); // golden ratio * 1000
  SetExecState(esNone);
end;

{ TstwStopState }
procedure TstwStopState.Execute;
begin
  { just set the execution status to "esStart", 'cause in countstate we check
    for "esNone"/"esStart" and "esPause". only "esPause" saves values. }
  SetExecState(esStart);
  fParentFSM.MQ.PostMsg(-1,1,0,0,'TstwStopState.Execute');
  { fsGotoNext gets set in "registered" start }
  if fStateValues.ipReserved = fsGotoNext then begin
    fStateValues.ipReserved:= 0;
    ChangeState(TstwCountState);
  end;
end;

{ TstwErrorState }
procedure TstwErrorState.Execute; { not needed (hopefully) }
var
  s: string;
begin { in an error-state, i'ld probably do: fParentFSM.UserData:= @ErrMsg[1];
  and then here i'ld do this: }
  s:= string(pchar(fParentFSM.UserData)); { nifty use of cookie }
  fParentFSM.MQ.PostMsg(-1,-9999,-1,-1,s); { we don't like exceptions in threads }
end;

end.

