{$Region 'License'}
{<begin bsd 3 clause license>
  Copyright ©2023, Benny Christensen a.k.a. cdbc

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of "fsm_engine" nor the names
      of its contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
<end of bsd license>}
{$EndRegion 'License'}

/////////////////////////////////////////////////////////////
// !!! REMEMBER TO KEEP YOUR DIRTY HANDS OFF THE GUI !!!   //
// (ANY) access to gui elements takes place via the Queues //
// 05.08.2023 Benny Christensen a.k.a. cdbc - first draft  //
/////////////////////////////////////////////////////////////
unit fsm_engine;
{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
interface
uses Classes,Contnrs,SysUtils,bcEvent,bcMessageQueue,bcSemaphore,bcThreadQueue;
const
  UnitVersion = '4.05.08.2023'; { it truly is a work in progress :-D }
  SGUIDIfsmDataProvider = '{0B74901D-51C9-4E66-BAEA-979CD9691207}';
  { errorstates and other useful contants... }
  eNoErrors = 0;
  eFiniteStateNotRegistered = -100;
  fsGotoNext = 99; { (f)inite(s)tate->goto next state -> check "ipReserved" in "TstwStopState.Execute"}
type
  EFiniteStateMachine = class(Exception);
  ///// to be moved at a later date :-) //////
  { a dataprovider API, a descendant of IFPObserved, every TPersistent descendant implements it }
  IfsmDataProvider = interface(IFPObserved)[SGUIDIfsmDataProvider]
    procedure NotifyConsumers(aSender : TObject;anOperation : TFPObservedOperation;aData : Pointer);
    procedure RegisterConsumer(aConsumer: TObject); // optional ~ FPOAttachObserver
    procedure UnRegisterConsumer(aConsumer: TObject); // optional ~ FPODetachObserver
  end; { IfsmDataProvider }
  ///// to be moved at a later date :-) //////
  { keep track of running internal state }
  TfsmExecutionState = (esNone,esStart,esWorking,esPause,esAbort);
  { some form of track record, to store stuff in, in case of abort,done or pause }
  { TfsmInternalProgressRec }
  PfsmInternalProgressRec = ^TfsmInternalProgressRec;
  TfsmInternalProgressRec = record { this being the ancester... used in  }
    { we just make a placeholder for any kind of data, descendants will define their own }
    ipCurrentValues: pointer;
    { we need to know and somehow let the world around us query it, change it if need be :o) }
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
  TFiniteStateMachine = class; // forward decl.
  { dataprovider event, opportunity for user to handle incoming influences on the statemachine,
    outside the statemachine itself, actually necessary to implement... }
  TOnProviderChanged = procedure(aStateMachine: TFiniteStateMachine;aSender: TObject;anOperation: TFPObservedOperation;aData: pointer) of object;
  { our beloved workhorse :o) }
  TfsmSleepThread = class; // forward decl.
  { TThreadCtrlRec carries support data for each thread }
  PThreadCtrlRec = ^TThreadCtrlRec;
  TThreadCtrlRec = record
    tcThread: TfsmSleepThread; { a working thread, that sleeps and then carries out its task }
    tcEvent: IbcEvent;         { an event that controls the sleeping }
    tcTerminated: boolean;     { have the thread been shutdown? }
    tcData: pointer;           { at user's discretion, unknown right now }
    tcSleeping: boolean;       { is the thread sleeping }
  end;
  TFiniteStateClass = class of TFiniteState;
  { TFiniteState }
  TFiniteState = class
  private
    function get_ParentFSM: TFiniteStatemachine;
  protected
    fParentFSM: TFiniteStateMachine;
    fStateValues: TfsmInternalProgressRec;
    function get_StateValues: PfsmInternalProgressRec; virtual;
    procedure set_StateValues(aValue: PfsmInternalProgressRec); virtual;
    procedure ChangeState(aStateCls: TFiniteStateClass);
    function GetExecState: TfsmExecutionState; virtual;
    procedure NotifyParent(aStr: shortstring); //TODO?!? ... change this to something else!
    procedure SetExecState(aValue: TfsmExecutionState);
  public
    constructor create(aFSM: TFiniteStateMachine);
    destructor Destroy; override;
    procedure Abort; virtual;
    procedure Execute; virtual;
    procedure Pause; virtual;
    property ParentFSM: TFiniteStatemachine read get_ParentFSM;
    property StateValues: PfsmInternalProgressRec read get_StateValues write set_StateValues;
  end;

{$i fsmStateListh.inc}

  { TFiniteStateMachine }
  TFiniteStateMachine = class(TObject,IFPObserver)
  private
    { cached state classes registered for performance }
    fCachedStates: TfsmStateList;
    fDataProvider: IfsmDataProvider;
    fDebug: boolean;
    fhOwner: ptrint;
    fInQ: IbcThreadQueue;
    fLoaded: boolean;
    fMsgQ: IbcMessageQueue;
    fName: string;
    fOnProviderChanged: TOnProviderChanged;
    fOutQ: IbcThreadQueue;
    fSem: TbcSemaphore;
    fDefState,fState: TFiniteState; { our default state and our chameleon }
    fTag: ptrint;
    fTR: pointer; { thread data record, to be decided?!? }
    fUserData: pointer;
    function get_Debug: boolean;
    function get_InQ: IbcThreadQueue;
    function get_Name: string;
    function get_OnProviderChanged: TOnProviderChanged;
    function get_OutQ: IbcThreadQueue;
    function get_Sem: IbcSemaphoreRAW;
    function get_Sleeping: boolean;
    function get_State: TFiniteStateClass;
    function get_Terminated: boolean;
    procedure set_Debug(aValue: boolean);
    procedure set_OnProviderChanged(aValue: TOnProviderChanged);
    procedure set_State(aValue: TFiniteStateClass);
  protected
    Tcr: TThreadCtrlRec;
    procedure FPOObservedChanged(aSender: TObject;Operation: TFPObservedOperation;Data: Pointer);
    procedure Notify(aMessage: TbcMessage); ///?!? should be changed into something appropriate...
    { returns 0 on success,  on  }
    function RegChangeState(aStateCls: TFiniteStateClass): ptrint;
    function Setup: boolean; virtual;
    procedure SetDefaultState(aStateCls: TFiniteStateClass);
    function Shutdown: boolean; virtual;
    procedure Wakeup; virtual;
    property Sem: IbcSemaphoreRAW read get_Sem;
    property State: TFiniteStateClass read get_State write set_State; { !!!DO NOT USE DIRECTLY!!! }
  public
    constructor Create(hOwner: ptrint;aMsgQ: IbcMessageQueue;aDbgMsg: boolean = true);
    Destructor Destroy; override;
    { exits "State.Execute" cold and hard! ...if the method checks for us!!! }
    procedure Abort; virtual;
    { intended to be used after "Pause", i.e.: we're staying in state }
    procedure Continue; virtual;
    { uses the messagequeue to send messages about progress,operations & errors etc... }
    property DebugMsg: boolean read get_Debug write set_Debug;
    { soft exit from "State.Execute",possible to save values, 'To be Continued'... :o) }
    procedure Pause; virtual;
    { registers the different states, machine can not run without them }
    procedure RegisterState(aState: TFiniteState); virtual;
    { changes the the dataprovider }
    procedure SetDataProvider(aDataProvider: IfsmDataProvider);
    { changes the active state after the current state is done and executes it,
      this might freeze ui for a short while, if you set "Urgent" = true,
      then it aborts current active "State.Execute" and start the new one }
    procedure SetState(aStateCls: TFiniteStateClass;Urgent: boolean = false);
    { intended for use with registered states, capable of running autonomously,
      depending upon how the states are implemented :o) }
    procedure Start(aName: string);
    { intended for use in cooperation with "SetState", which releases the next state }
    procedure StartWith(aStateCls: TFiniteStateClass;aDataProvider: IfsmDataProvider;aName: string);
    function Terminate: boolean;  virtual;
    { a threadsafe queue to receive input-data if need be }
    property InQ: IbcThreadQueue read get_InQ;
    { the same messagequeue we got fed, only surfaced here for sub-components to reach it }
    property MQ: IbcMessageQueue read fMsgQ;
    { just in case }
    property Name: string read get_Name;
    { event gets fired on changes in our dataprovider, gives us an opportunity,
      to deal with data outside the statemachine, mandatory, when using a dataprovider }
    property OnProviderChanged: TOnProviderChanged read get_OnProviderChanged write set_OnProviderChanged;
    { a threadsafe queue to deliver output-data if need be }
    property OutQ: IbcThreadQueue read get_OutQ;
    { are we doing nothing? }
    property Sleeping: boolean read get_Sleeping;
    { "Tag" is a cookie for user convenience, pointer-sized :o) }
    property Tag: ptrint read fTag write fTag;
    { are we done yet? }
    property Terminated: boolean read get_Terminated;
    { "UserData" is another cookie for user convenience }
    property UserData: pointer read fUserData write fUserData;
  end; { TFiniteStateMachine }
  { TfsmSleepThread is a thread that sleeps and can be woken up again }
  TfsmSleepThread = class(TThread)
  private
    function get_Sync: IReadWriteSync;
  protected
    fCaller: ptrint; { owner handle }
    fCtrl: TFiniteStateMachine;
    fID: ptrint; { own id, given to us by the controller }
    fState: TFiniteState; { our chameleon }
    fSync: IReadWriteSync; { our internal locking object }
    function get_State: TFiniteState; virtual;
    procedure set_State(aValue: TFiniteState); virtual;
    procedure Execute; override;
  public
    constructor Create(aHandle,anID: ptrint;aController: TFiniteStateMachine); // IFiniteStateMachine
    destructor Destroy; override;
    property State: TFiniteState read get_State write set_State; // Current state
    { our internal synchronization object }
    property Sync: IReadWriteSync read get_Sync;
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
{$Region 'SleepThread'}
{ TfsmSleepThread }
procedure TfsmSleepThread.set_State(aValue: TFiniteState);
begin
  if fState <> aValue then fState:= aValue;
end;

function TfsmSleepThread.get_Sync: IReadWriteSync;
begin
  Result:= fSync;
end;

function TfsmSleepThread.get_State: TFiniteState;
begin
  Result:= fState;
end;

procedure TfsmSleepThread.Execute;
var
  lDbg: boolean = true; { we'll only read this once, for every turnaround, fsm defaults to true }
begin
  if lDbg then
    fCtrl.MQ.PostMsg(fCaller,1024,0,1,fID.ToString+' Entering Execute... ');
  { as we are going to sleep once in a while, we're dependant on outside control, NOTHING works when we sleep! }
  while not fCtrl.Tcr.tcTerminated do begin
    fCtrl.Sem.Wait; try
      fCtrl.Tcr.tcSleeping:= true; { mark that we are going to sleep }
    finally fCtrl.Sem.Signal; end;
    fCtrl.Tcr.tcEvent.Waitfor; { sleep until wakeup, calls waitfor on event internally }
    fCtrl.Sem.Wait; try
      fCtrl.Tcr.tcSleeping:= false; { here we go again... }
      lDbg:= fCtrl.DebugMsg; { one read }
    finally fCtrl.Sem.Signal; end;
    if fCtrl.Tcr.tcTerminated then break; { did we get woken up, just to terminate? }
    fCtrl.Tcr.tcEvent.Reset; { reset event, ready for next round }
    if lDbg then
      fCtrl.MQ.PostMsg(fCaller,4711,0,1,fID.ToString+' Ahhh, Hiho off to work we go... ');
    if Assigned(fState) then try { we don't care much for NIL }
      { by locking it here, we can wait for it when we change state... ;-)
        warning! if state is changed uncoordinated, UI WILL FREEZE }
      if fSync.BeginWrite then try
        fState.Execute; // call the heavy lifter
      finally fSync.EndWrite; end;
      if lDbg then
        fCtrl.MQ.PostMsg(fCaller,2048,0,1,fID.ToString+' State: "'+fState.ClassName+'" is done.');
    except on E:Exception do
      begin
        if lDbg then
          fCtrl.MQ.PostMsg(fCaller,-1,0,1,'ERROR: '+E.Message+' Index: '+fID.ToString);
        fState.StateValues^.Abort;
      end;
    end;
    if fCtrl.Tcr.tcTerminated then break; { catering for long running states }
    if lDbg then
      fCtrl.MQ.PostMsg(fCaller,3072,0,1,fID.ToString+' Ahhh, Me tired now, naptime... ');
  end; //main loop
  if lDbg then
    fCtrl.MQ.PostMsg(fCaller,60606,0,1,fID.ToString+' OK, that''s it... Bye bye. ');
end;

constructor TfsmSleepThread.Create(aHandle,anID: ptrint;aController: TFiniteStateMachine);
begin
  inherited Create(true);
  fSync:= TSimpleRWSync.Create; // com obj
  fCaller:= aHandle;
  fCtrl:= aController;
  fID:= anID;
  Start;
end;

destructor TfsmSleepThread.Destroy;
begin
  fSync:= nil; // com object
  inherited Destroy;
end;
{$EndRegion 'SleepThread'}
{$Region 'FiniteStateMachine'}
{ TFiniteStateMachine }
function TFiniteStateMachine.get_InQ: IbcThreadQueue;
begin
  Result:= fInQ;
end;

function TFiniteStateMachine.get_Debug: boolean;
begin
  Result:= fDebug;
end;

function TFiniteStateMachine.get_Name: string;
begin
  Result:= fName;
end;

function TFiniteStateMachine.get_OnProviderChanged: TOnProviderChanged;
begin
  Result:= fOnProviderChanged;
end;

function TFiniteStateMachine.get_OutQ: IbcThreadQueue;
begin
  Result:= fOutQ;
end;

function TFiniteStateMachine.get_Sem: IbcSemaphoreRAW;
begin
  Result:= fSem as IbcSemaphoreRAW;
end;

function TFiniteStateMachine.get_Sleeping: boolean;
begin
  fSem.Wait; try
    Result:= Tcr.tcSleeping;
  finally fSem.Signal; end;
end;

function TFiniteStateMachine.get_State: TFiniteStateClass;
begin
  Result:= TFiniteStateClass(fState.ClassType);
end;

function TFiniteStateMachine.get_Terminated: boolean;
begin
  fSem.Wait; try
    Result:= Tcr.tcTerminated;
  finally fSem.Signal; end;
end;

procedure TFiniteStateMachine.set_Debug(aValue: boolean);
begin
  fDebug:= aValue;
end;

procedure TFiniteStateMachine.set_OnProviderChanged(aValue: TOnProviderChanged);
begin
  fOnProviderChanged:= aValue;
end;

procedure TFiniteStateMachine.set_State(aValue: TFiniteStateClass);
begin
  SetState(aValue); { changes state when sleeping and activates }
(* obsolete / deprecated
  fState:= fCachedStates.GetStateByClass(aValue);
  if fState = nil then raise Exception.CreateFmt('TFiniteStateMachine.set_State, Error: %s is not a registered state!',[aValue.ClassName]);
*)
end;

procedure TFiniteStateMachine.Notify(aMessage: TbcMessage);
begin
  fMsgQ.EnQueue(aMessage);
end;

function TFiniteStateMachine.RegChangeState(aStateCls: TFiniteStateClass): ptrint;
var lStt: TFiniteState;
begin
  lStt:= fCachedStates.GetStateByClass(aStateCls);
  if lStt = nil then exit(eFiniteStateNotRegistered); // -100
  fState:= lStt; { internal state }
  Tcr.tcThread.State:= fState; { active state }
  Wakeup; { immediately releases the thread for another round ~ no sleep }
  Result:= eNoErrors; { signal success ~ 0 }
end;

procedure TFiniteStateMachine.FPOObservedChanged(aSender: TObject;Operation: TFPObservedOperation;Data: Pointer);
begin { relays the outside influence to event-handler, to decide what to do }
  if Assigned(fOnProviderChanged) then fOnProviderChanged(Self,aSender,Operation,Data)
  else raise EObserver.Create('Error! TFiniteStateMachine.OnProviderChanged is unassigned, did you forget?!?');
end;

function TFiniteStateMachine.Setup: boolean;
begin
  try
    FillChar(Tcr,sizeof(TThreadCtrlRec),0);
    Tcr.tcEvent:= TbcEvent.Create;
    Tcr.tcTerminated:= false;
    Tcr.tcData:= nil;
    Tcr.tcThread:= TfsmSleepThread.Create(fhOwner,1970,Self);
    Tcr.tcSleeping:= false;
    fLoaded:= true;
  except fLoaded:= false; end;
  Result:= fLoaded;
end;

procedure TFiniteStateMachine.SetDefaultState(aStateCls: TFiniteStateClass);
begin
  if aStateCls = nil then exit;
  if fDefState <> nil then begin
    fDefState.Free;
    fDefState:= aStateCls.create(Self);
  end else fDefState:= aStateCls.create(Self);
  fState:= fDefState;
end;

function TFiniteStateMachine.Shutdown: boolean;
begin
  Result:= false;
  if fLoaded then try
    Tcr.tcTerminated:= true;
    Tcr.tcEvent.Signal;
    Tcr.tcThread.WaitFor;
    FreeAndNil(Tcr.tcThread);
    if bcSvcIsCorba(TypeInfo(Tcr.tcEvent)) then Tcr.tcEvent.Obj.Free;
    Tcr.tcEvent:= nil;
    fLoaded:= false;
  except fLoaded:= true; end;
  Result:= not fLoaded;
end;

constructor TFiniteStateMachine.Create(hOwner: ptrint;aMsgQ: IbcMessageQueue;aDbgMsg: boolean = true);
begin
  inherited Create;
  fhOwner:= hOwner;
  fDebug:= aDbgMsg;
  fCachedStates:= TfsmStateList.Create(true); { owns the objects within and will free them on exit }
  fMsgQ:= aMsgQ; { plug in to owners messagequeue }
  fSem:= TbcSemaphore.Create(1); { binary semaphore i.e: a mutex / critsect }
  fDefState:= TFiniteState.create(Self); { when all else fails, }
  fState:= fDefState;                    { we're dealing with a thread }
  fInQ:= TbcThreadQueue.Create as IbcThreadQueue;
  fOutQ:= TbcThreadQueue.Create as IbcThreadQueue;
  Setup; { init thread, it'll initiate and go to sleep }
end;

destructor TFiniteStateMachine.Destroy;
begin
  fCachedStates.Free;
  { fini threads, if not -> raise exception, what else to do?!? }
  if not Shutdown then raise EFiniteStateMachine.Create('Error! TFiniteStateMachine.Destroy: Cannot shutdown Thread');
  if bcSvcIsCorba(TypeInfo(fOutQ)) then fOutQ.Obj.Free;
  fOutQ:= nil;
  if bcSvcIsCorba(TypeInfo(fInQ)) then fInQ.Obj.Free;
  fInQ:= nil;
  if Assigned(fDataProvider) then fDataProvider.UnRegisterConsumer(Self);
  FreeAndNil(fSem);
  fDefState.Free;
  fMsgQ:= nil; { we just borrowed it anyway }
  inherited Destroy;
end;

procedure TFiniteStateMachine.Abort;
begin
  Sem.Wait; try
    fState.Abort; { abort interrupts the "Execute" method, IF one checks for it! }
  finally Sem.Signal; end;
end;

procedure TFiniteStateMachine.Continue;
begin
  { intended to be used after "Pause", i.e.: we're staying in state }
  Wakeup; { and just get to reawaken }
end;

procedure TFiniteStateMachine.Pause;
begin
  Sem.Wait; try
    fState.Pause;
  finally Sem.Signal; end;
end;

procedure TFiniteStateMachine.RegisterState(aState: TFiniteState);
begin
  fCachedStates.AddState(aState); { add to our internal list of cached states }
end;

procedure TFiniteStateMachine.SetDataProvider(aDataProvider: IfsmDataProvider);
begin
  if aDataProvider = nil then exit;
  if fDataProvider <> nil then begin
    fDataProvider.UnRegisterConsumer(Self); { avoid dangling references }
    fDataProvider:= aDataProvider;
    fDataProvider.RegisterConsumer(Self);
  end else begin
    fDataProvider:= aDataProvider;
    fDataProvider.RegisterConsumer(Self);
  end;
end;

procedure TFiniteStateMachine.SetState(aStateCls: TFiniteStateClass;Urgent: boolean = false);
begin
  if aStateCls = nil then exit;
  if (Urgent and (not Sleeping)) then Abort; { MUST be important!!! }
  { add it to cache, to make sure it gets free'd and to speed up things }
  if not fCachedStates.Contains(aStateCls) then fCachedStates.AddState(aStateCls.create(Self));
  { now it's easy obtainable and we can wait for execute to finish }
  if not Sleeping then begin { the thread locks execute, possibly we'll wait }
    Tcr.tcThread.Sync.BeginWrite; try
      fState:= fCachedStates.GetStateByClass(aStateCls);
      Tcr.tcThread.State:= fState; { active state }
    finally Tcr.tcThread.Sync.EndWrite; end;
    { NOT within the lock, because the thread locks the execute method,
      immediately after waking up thus causing a DeadLock! }
    Wakeup;
  end else begin { thread is sleeping }
    fState:= fCachedStates.GetStateByClass(aStateCls);
    Tcr.tcThread.State:= fState; { active state }
    Wakeup; { run forrest, run... }
  end;
end;

procedure TFiniteStateMachine.Start(aName: string);
begin
  if fCachedStates.Count = 0 then exit; { we've got nothing to execute?!? }
  if not fLoaded then Setup;
  fName:= aName;
  fState:= fCachedStates.Items[0]; { first registered is initial ~ active }
  fState.StateValues^.ipReserved:= fsGotoNext; // registered "goto next state"
  Tcr.tcThread.State:= fState; { active state }
  Wakeup; { Tju, tjuhhh... }
end;

procedure TFiniteStateMachine.StartWith(aStateCls: TFiniteStateClass;aDataProvider: IfsmDataProvider;aName: string);
begin
  if not fLoaded then Setup;
  SetDataProvider(aDataProvider); { in case we get outside influence }
  SetDefaultState(aStateCls); { state is now our initial state, aswell as our active state }
  Tcr.tcThread.State:= fState; { active state }
  fName:= aName;
  // start thread...
  Wakeup; { Tju, tjuhhh... }
end;

function TFiniteStateMachine.Terminate: boolean;
begin
  Result:= Shutdown;
  if Result then begin
    if fDebug then
      fMsgQ.PostMsg(fhOwner,60606,-1,-1,'Finite State Machine has terminated!');
  end;
end;

procedure TFiniteStateMachine.Wakeup;
begin
  if fLoaded then Tcr.tcEvent.Signal;  // wake up thread
end;
{$EndRegion 'FiniteStateMachine'}
{$i fsmFiniteState.inc}
{$i fsmStateList.inc}

end.

