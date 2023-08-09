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
    * Neither the name of "bc_sleepthread" nor the names
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
{*******************************************************************************
*  Unit name : bc_sleepthread.pas                                              *
*  Copyright : © 2023 Benny Christensen,                                       *
*              Classes, SysUtils, Contnrs © the respective owners              *
*  Programmer: Benny Christensen a.k.a. cdbc                                   *
*  Created   : 01.08.2023 /bc type definitions for "IbcMessageQueue" and       *
*                             TbcMessageQueue, applying the "decorator"        *
*                             -pattern to "TQueue" from "contnrs" unit.        *
*  Updated   : 01.08.2023 /bc added TbcMessageQueue implementation, which      *
*                             adds thread synchronization to TQueue, AMO.      *
*                                                                              *
*******************************************************************************}
unit bc_sleepthread;
{$mode objfpc}{$H+}
{-$define debug}
interface
uses Classes, SysUtils,bcEvent,bcMessageQueue,bcSemaphore{$ifdef debug},heaptrc{$endif};
const
  UnitVersion = '0.01.08.2023'; { initial version }
  SGUIDIbcSleepingThread = '{04EDDD1C-FF53-41BC-AB71-7F8067B82FA0}';
type
  { task definitions we can handle "In Our Sleep" :o) }
  { TNotifyEvent = procedure(Sender: TObject) of object; }
  TTaskGui = TNotifyEvent;
  { TNotifyCallBack = Procedure(Sender : TObject; AData : Pointer); }
  TTaskClbk = TNotifyCallBack;
  ESleepingThreadError = class(Exception); // designated error
  TbcTaskThread = class; // forward decl.
  { TThreadCtrlRec carries support data for each thread }
  PThreadCtrlRec = ^TThreadCtrlRec;
  TThreadCtrlRec = record
    tcData: pointer;           { data for use in task }
    tcEvent: IbcEvent;         { an event that controls the sleeping }
    tcSender: TObject;         { object for use in task }
    tcSleeping: boolean;       { is the thread sleeping }
    tcTerminated: boolean;     { have the thread been shutdown? }
    tcThread: TbcTaskThread;   { a working thread, that sleeps and then carries out its task }
  end; { TThreadCtrlRec }
  PTaskOptions = ^TTaskOptions;
  TTaskOptions = record
    toType: word;              { 0..N ~ 0=TaskClbk, 1=TaskGui, etc... }
  end;

{$Interfaces CORBA} { refcounting OFF }
  { IbcSleepingThread is the corba/raw service interface for TbcSleepingThread, a thread that sleeps :o)
    }
  IbcSleepingThread = interface[SGUIDIbcSleepingThread]
    { property getters and setters }
    function get_Debug: boolean;
    function get_Loaded: boolean;
    function get_MsgQ: IbcMessageQueue;
    function get_Object: TObject;
    function get_Sleeping: boolean;
    function get_TaskClbk: TTaskClBk;
    function get_TaskGui: TTaskGui;
    function get_Tcr: PThreadCtrlRec;
    procedure set_Debug(aValue: boolean);
    procedure set_TaskClbk(aValue: TTaskClBk);
    procedure set_TaskGui(aValue: TTaskGui);
    { Start gets the whole circus in motion, the sequence goes like this:
      1) assign a task to one of the Task*** properties and NIL to the other,
         or assign both tasks, they will both be called.
      2) call "Start" with the appropriate parameters and options }
    procedure Start(aSender: TObject;aData,Options: pointer); virtual;
    function Terminate: boolean;  virtual;
    property DebugMsg: boolean read get_Debug write set_Debug;
    property Loaded: boolean read get_Loaded;
    property MsgQ: IbcMessageQueue read get_MsgQ;
    { handy property in some situations (with raw intf) }
    property Obj: TObject read get_Object;
    property Sleeping: boolean read get_Sleeping;
    property TaskClbk: TTaskClBk read get_TaskClbk write set_TaskClbk; { protected access }
    property TaskGui: TTaskGui read get_TaskGui write set_TaskGui; { protected access }
    property Tcr: PThreadCtrlRec read get_Tcr;
  end; { IbcSleepingThread }
{$Interfaces COM} { refcounting back ON }
{$Region 'TbcSleepingThread'}
  { TbcSleepingThread implements a sleeping thread }
  TbcSleepingThread = class(TObject,IbcSleepingThread)
  private
    fDebug: boolean;
    fhOwner: ptrint;
    fLoaded: boolean;
    fMsgQ: IbcMessageQueue;
    fTag: ptrint; // user cookie
    fUserData: pointer; // user cookie
    function get_Debug: boolean;
    function get_Loaded: boolean;
    function get_MsgQ: IbcMessageQueue;
    function get_Object: TObject;
    function get_Sleeping: boolean;
    function get_TaskClbk: TTaskClBk;
    function get_TaskGui: TTaskGui;
    function get_Tcr: PThreadCtrlRec;
    procedure set_Debug(aValue: boolean);
    procedure set_TaskClbk(aValue: TTaskClBk);
    procedure set_TaskGui(aValue: TTaskGui);
  protected
    fTaskClbk: TTaskClBk;
    fTaskGui: TTaskGui;
    fTcr: TThreadCtrlRec;
    function Setup: boolean; virtual;
    function Shutdown: boolean; virtual;
    procedure Wakeup; virtual;
    property Loaded: boolean read get_Loaded;
    property MsgQ: IbcMessageQueue read get_MsgQ;
    property Tcr: PThreadCtrlRec read get_Tcr;
    property DebugMsg: boolean read get_Debug write set_Debug;
  public
    constructor Create(hOwner: ptrint;aMsgQ: IbcMessageQueue;aDbgMsg: boolean = true);
    Destructor Destroy; override;
    { Start gets the whole circus in motion, the sequence goes like this:
      1) assign a task to one of the Task*** properties and NIL to the other,
         or assign both tasks, they will both be called, if not nil.
      2) call "Start" with the appropriate parameters and options }
    procedure Start(aSender: TObject;aData,Options: pointer); virtual;
    function Terminate: boolean;  virtual;
    property Obj: TObject read get_Object;
    property Sleeping: boolean read get_Sleeping;
    property TaskClbk: TTaskClBk read get_TaskClbk write set_TaskClbk; { protected access }
    property TaskGui: TTaskGui read get_TaskGui write set_TaskGui; { protected access }
  end;

{$EndRegion 'TbcSleepingThread'}
  { TbcTaskThread is a thread that sleeps and can be woken up again, executes a task when awake }
  TbcTaskThread = class(TThread)
  private
    function get_ID: ptrint;
    function get_TaskClbk: TTaskClBk;
    function get_TaskGui: TTaskGui;
    procedure set_TaskClbk(AValue: TTaskClBk);
    procedure set_TaskGui(AValue: TTaskGui);
  protected
    fCaller: ptrint; { owner handle }
    fCtrl: IbcSleepingThread; { as the thread sleeps, we need an outside object to control it }
    fID: ptrint; { own id, given to us by the controller }
    fSync: IReadWriteSync; { our internal locking object }
    fTaskClbk: TTaskClBk;
    fTaskGui: TTaskGui;
    procedure Execute; override;
  public
    constructor Create(aHandle,anID: ptrint;aController: IbcSleepingThread);
    destructor Destroy; override;
    property ID: ptrint read get_ID; { unprotected access }
    property Sync: IReadWriteSync read fSync; { protection }
    property TaskClbk: TTaskClBk read get_TaskClbk write set_TaskClbk; { protected access }
    property TaskGui: TTaskGui read get_TaskGui write set_TaskGui; { protected access }
  end;



implementation
uses TypInfo;
{$i bcUtils.inc} // include a couple of uility funcs
{$Region 'TbcSleepingThread'}
{ TbcSleepingThread }

function TbcSleepingThread.get_Debug: boolean;
begin { DON'T put sync here! DEADLOCK }
  Result:= fDebug;
end;

function TbcSleepingThread.get_Loaded: boolean;
begin
  Result:= fLoaded;
end;

function TbcSleepingThread.get_MsgQ: IbcMessageQueue;
begin
  Result:= fMsgQ;
end;

function TbcSleepingThread.get_Object: TObject;
begin
  Result:= Self;
end;

function TbcSleepingThread.get_Sleeping: boolean;
begin
  if fTcr.tcThread.Sync.BeginWrite then try
    Result:= Tcr^.tcSleeping;
  finally fTcr.tcThread.Sync.EndWrite; end;
end;

function TbcSleepingThread.get_TaskClbk: TTaskClBk;
begin
  Result:= fTaskClbk;
end;

function TbcSleepingThread.get_TaskGui: TTaskGui;
begin
  Result:= fTaskGui;
end;

function TbcSleepingThread.get_Tcr: PThreadCtrlRec;
begin
  Result:= @fTcr;
end;

procedure TbcSleepingThread.set_Debug(aValue: boolean);
begin { DON'T put sync here! DEADLOCK }
  fDebug:= aValue;
end;

procedure TbcSleepingThread.set_TaskClbk(aValue: TTaskClBk);
begin
  fTaskClbk:= aValue;
end;

procedure TbcSleepingThread.set_TaskGui(aValue: TTaskGui);
begin
  fTaskGui:= aValue;
end;

function TbcSleepingThread.Setup: boolean;
begin
  try
    FillChar(fTcr,sizeof(TThreadCtrlRec),0);
    fTcr.tcData:= nil;
    fTcr.tcEvent:= TbcEvent.Create;
    fTcr.tcSender:= nil;
    fTcr.tcSleeping:= false;
    fTcr.tcTerminated:= false;
    fTcr.tcThread:= TbcTaskThread.Create(fhOwner,1970,Self);
    fLoaded:= true;
  except fLoaded:= false; end; // TODO: free resources
  Result:= fLoaded;
end;

function TbcSleepingThread.Shutdown: boolean;
begin
  Result:= false;
  if fLoaded then try
    fTcr.tcTerminated:= true;
    fTcr.tcEvent.Signal;
    fTcr.tcThread.WaitFor;
    FreeAndNil(fTcr.tcThread);
    if bcSvcIsCorba(TypeInfo(fTcr.tcEvent)) then fTcr.tcEvent.Obj.Free;
    fTcr.tcEvent:= nil;
    fLoaded:= false;
  except end;
  Result:= not fLoaded;
end;

procedure TbcSleepingThread.Wakeup;
begin
  if fLoaded then fTcr.tcEvent.Signal;  // wake up thread
end;

constructor TbcSleepingThread.Create(hOwner: ptrint;aMsgQ: IbcMessageQueue;aDbgMsg: boolean);
begin
  inherited Create;
  fhOwner:= hOwner;
  fMsgQ:= aMsgQ; { plug in to owners messagequeue }
  fDebug:= aDbgMsg;
  Setup; { init thread, it'll initiate and go to sleep }
end;

destructor TbcSleepingThread.Destroy;
begin
  inherited Destroy;
end;

procedure TbcSleepingThread.Start(aSender: TObject; aData, Options: pointer);
begin
  if not Loaded then setup;
  if fTcr.tcSleeping then begin
    fTcr.tcSender:= aSender;
    fTcr.tcData:= aData;
    fTcr.tcThread.TaskClbk:= fTaskClbk;
    fTcr.tcThread.TaskGui:= fTaskGui;
    Wakeup;
  end;
end;

function TbcSleepingThread.Terminate: boolean;
begin
  Result:= Shutdown;
  if Result then fMsgQ.PostMsg(fhOwner,60606,-1,-1,'SleepingThread has terminated!');
end;
{$EndRegion 'TbcSleepingThread'}
{$Region 'TbcTaskThread'}
{ TbcTaskThread }

function TbcTaskThread.get_ID: ptrint;
begin
  Result:= fID; // just a read, unprotected
end;

function TbcTaskThread.get_TaskClbk: TTaskClbk;
begin
  fSync.BeginRead; try Result:= fTaskClbk; finally fSync.EndRead; end;
end;

function TbcTaskThread.get_TaskGui: TTaskGui;
begin
  fSync.BeginRead; try Result:= fTaskGui; finally fSync.EndRead; end;
end;

procedure TbcTaskThread.set_TaskClbk(aValue: TTaskClBk);
begin
  if fSync.BeginWrite then try fTaskClbk:= aValue; finally fSync.EndWrite; end;
end;

procedure TbcTaskThread.set_TaskGui(aValue: TTaskGui);
begin
  if fSync.BeginWrite then try fTaskGui:= aValue; finally fSync.EndWrite; end;
end;

procedure TbcTaskThread.Execute;
var
  lDbg: boolean = true; { we'll only read this once, for every turnaround, fsm defaults to true }
begin
  if lDbg then
    fCtrl.MsgQ.PostMsg(fCaller,1024,0,1,fID.ToString+' Entering Execute... '); // notify must be commented later
  { as we are going to sleep once in a while, we're dependant on outside control, NOTHING works when we sleep! }
  while not fCtrl.Tcr^.tcTerminated do begin
    if Sync.BeginWrite then try
      fCtrl.Tcr^.tcSleeping:= true; { mark that we are going to sleep }
    finally Sync.EndWrite; end;
    fCtrl.Tcr^.tcEvent.Waitfor; { sleep until wakeup, calls waitfor on event internally }
    if Sync.BeginWrite then try
      lDbg:= fCtrl.DebugMsg; { will last us this turnaround }
      fCtrl.Tcr^.tcSleeping:= false; { here we go again... }
    finally Sync.EndWrite; end;
    if fCtrl.Tcr^.tcTerminated then break; { did we get woken up, just to terminate? }
    fCtrl.Tcr^.tcEvent.Reset; { reset event, ready for next round }
    if lDbg then
      fCtrl.MsgQ.PostMsg(fCaller,4711,0,1,fID.ToString+' Ahhh, Hiho off to work we go... '); // notify must be commented later
    if Assigned(fTaskGui) then try { we don't care much for NIL }
      fTaskGui(fCtrl.Tcr^.tcSender); // call the heavy lifter with data provided
      if lDbg then fCtrl.MsgQ.PostMsg(fCaller,2048,0,1,fID.ToString+' <TaskGui> is done.');
    except on E:Exception do
      begin
        if lDbg then fCtrl.MsgQ.PostMsg(fCaller,-1,0,1,'ERROR: '+E.Message+' ID: '+fID.ToString);
      end;
    end; { taskgui }
    if Assigned(fTaskClbk) then try { we don't care much for NIL }
      fTaskClbk(fCtrl.Tcr^.tcSender,fCtrl.Tcr^.tcData); // call the heavy lifter with data provided
      if lDbg then fCtrl.MsgQ.PostMsg(fCaller,2048,0,1,fID.ToString+' "TaskClbk" is done.');
    except on E:Exception do
      begin
        if lDbg then fCtrl.MsgQ.PostMsg(fCaller,-1,0,1,'ERROR: '+E.Message+' ID: '+fID.ToString);
      end;
    end; { taskclbk }
    if fCtrl.Tcr^.tcTerminated then break; { catering for long running states }
    if lDbg then
      fCtrl.MsgQ.PostMsg(fCaller,4711,0,1,fID.ToString+' Ahhh, Me tired now, naptime... ');
  end; //main loop
  if lDbg then fCtrl.MsgQ.PostMsg(fCaller,60606,0,1,fID.ToString+' OK, that''s it... Bye bye. ');
end;

constructor TbcTaskThread.Create(aHandle,anID: ptrint;aController: IbcSleepingThread);
begin
  inherited Create(true);
  fCaller:= aHandle;
  fCtrl:= aController;
  fID:= anID;
  fSync:= TSimpleRWSync.Create; // com obj
  Start;
end; { TbcTaskThread.Create }

destructor TbcTaskThread.Destroy;
begin
  fSync:= nil; // com object
  inherited Destroy;
end;
{$EndRegion 'TbcTaskThread'}
(* example task

procedure TForm1.Task5Sec(Sender: TObject);
var w: word;
begin
  for w:= 0 to 4 do begin
    MsgQ.PostMsg(Handle,1031,GetTickCount64,w,'Hello from '+Sender.ClassName);
    sleep(1000);
  end;
end;
 *)

(* example task2
if Assigned(fSlt) then begin
  if fSlt.Sleeping then begin
    fSlt.TaskGui:= @Task5Sec;
    fSlt.Start(MsgQ.Obj,nil,nil);
  end;
end;
*)
 
{$ifdef debug}  {$endif}

initialization
//  Singleton:= nil;

finalization 
//  if Singleton <> nil then Singleton.Free;
  
end.

