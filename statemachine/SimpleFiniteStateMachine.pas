unit SimpleFiniteStateMachine;
{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
{$define dbg}
interface
uses Classes, SysUtils;
const
  UnitVersion = '0.29.07.2023'; // inital version
type
  { keep track of running internal state }
  TfsmExecutionState = (esNone,esStart,esWorking,esPause,esAbort,esDone);
  { some form of track record, to store stuff in, in case of abort,done or pause }
  { TfsmInternalProgressRec }
  PfsmInternalProgressRec = ^TfsmInternalProgressRec;
  TfsmInternalProgressRec = record { this being the ancester... }
    { we just make a placeholder for any kind of data, descendants will define their own }
    ipCurrentValues: pointer;
    { we need to know and somehow let the world around us query it }
    ipExecState: TfsmExecutionState;
    { on the odd chance we need to remember something }
    ipCookie: TObject;
    { we'll save a pointersized field for future use ;-) }
    ipReserved: ptrint;
    { sets an internal flag, to let "Execute" know that it must exit, a.s.a.p. }
    procedure Abort;
    { just in case we need to continue from another state or go back to a previous one, NIL clears }
    procedure Assign(aSrc: PfsmInternalProgressRec);
    { sets an internal flag, to let "Execute" know that it must save current values and exit }
    procedure Pause;
  end;

  { Forward declarations }
  TFiniteStateMachine = class;

  { TFiniteState }
    TFiniteState = class
    private
      function get_ParentFSM: TFiniteStatemachine;
    protected
      fParentFSM: TFiniteStateMachine;
      fStateValues: TfsmInternalProgressRec;
      function get_StateValues: PfsmInternalProgressRec; virtual;
      procedure set_StateValues(aValue: PfsmInternalProgressRec); virtual;
    public
      constructor create(aFSM: TFiniteStateMachine);           // FSM handling this state
      destructor Destroy; override;
      procedure Execute; virtual;
      property ParentFSM: TFiniteStatemachine read get_ParentFSM;
      property StateValues: PfsmInternalProgressRec read get_StateValues write set_StateValues;
    end;
    TFiniteStateClass = class of TFiniteState;

  { TFiniteStateMachine }

  TFiniteStateMachine = class(TThread)
  private
    FState: TFiniteState;
  public
    property State: TFiniteState read FState;                   // Current state
    procedure setState(aStateClass:TFiniteStateClass);          // Change current state
    constructor Create(AInitialStateClass: TFiniteStateClass);  // Create machine and set initial state
    procedure Execute; override;                                // State machine thread worker
    procedure Synchronize(AMethod: TThreadMethod);              // Let states talk to the main thread
  end;

implementation

{$Region 'ProgressRec'}
{ TfsmInternalProgressRec }
procedure TfsmInternalProgressRec.Abort;
begin
  ipExecState:= esAbort;
end;

procedure TfsmInternalProgressRec.Assign(aSrc: PfsmInternalProgressRec);
begin // this will do for now, i'm still thinking :o)
  if aSrc = nil then begin
    Self:= Default(TfsmInternalProgressRec); // dunno.... but it compiles?!?
    exit;
  end;
  ipCookie:= aSrc^.ipCookie;
  ipCurrentValues:= aSrc^.ipCurrentValues;
  ipExecState:= aSrc^.ipExecState;
  ipReserved:= aSrc^.ipReserved;
end;

procedure TfsmInternalProgressRec.Pause;
begin
  ipExecState:= esPause;
end;
{$EndRegion 'ProgressRec'}
{ TFiniteState }
function TFiniteState.get_ParentFSM: TFiniteStatemachine;
begin
  Result:= fParentFSM;
end;

function TFiniteState.get_StateValues: PfsmInternalProgressRec;
begin
  Result:= @fStateValues;
end;

procedure TFiniteState.set_StateValues(aValue: PfsmInternalProgressRec);
begin
  if aValue <> @fStateValues then fStateValues.Assign(aValue);
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

procedure TFiniteState.Execute;
var I: integer;
begin
  { check state and initialize values accordingly }
  case fStateValues.ipExecState of
    esPause: begin
               I:= Integer(fStateValues.ipCurrentValues);
               // ...
             end;
    esNone,
    esStart: begin
               I:= 0;
               // ...
             end;
  end;
  fStateValues.ipExecState:= esWorking; { we set it ourselves, so others  }
  while I < 99  do begin
    //.... working hard :o)
    // ...
    { for longer operations we check our statevalues periodically }
    case fStateValues.ipExecState of
      esAbort: exit; // exit cold and hard
      esPause: begin // save values and exit
                 fStateValues.ipCurrentValues:= pointer(I); // just interim...
                 exit;
               end;
    end;
  end;
  { finalize values and signal we're done }
  fStateValues.ipCurrentValues:= pointer(I); // just interim...
  fStateValues.ipExecState:= esDone;
end; { this method serves as an example of how to check status underway }

{ TFiniteStateMachine }

procedure TFiniteStateMachine.setState(aStateClass: TFiniteStateClass);

begin
  if assigned(FState) then FState.Free;
  FState := AStateClass.Create(Self);
end;

constructor TFiniteStateMachine.Create(AInitialStateClass: TFiniteStateClass);

begin
  inherited Create(True);
  FreeOnTerminate := False;
  SetState(AInitialStateClass);
end;

procedure TFiniteStateMachine.Execute;

begin
  while not terminated do
    if assigned(State) then
      State.Execute
    else
      raise Exception.Create({$I %CURRENTROUTINE%} + 'State not assigned');
  State.Free;
end;

procedure TFiniteStateMachine.Synchronize(AMethod: TThreadMethod);

begin
  inherited Synchronize(AMethod);
end;

end.

