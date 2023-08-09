{$Region 'License'}
{ <begin bsd 3 clause license>
  Copyright ©2023, Benny Christensen a.k.a. cdbc

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of "bc_syncobj" nor the names
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
<end of bsd license> }
{$EndRegion 'License'}
{ synchronization service for use with multi-threading
  this service registers itself with the SvcMgr as:
  SvcMgr.RegisterService('IbcSemaphoreRAW','bcSemaphore'
  usage: var ISem: IbcSemaphoreRAW;
         pointer(ISem):= SvcMgr.GetSvcWeak('IbcSemaphoreRAW','bcSemaphore');
         ... use ISem semaphore ...
         if bcSvcIsCorba(TypeInfo(ISem)) then ISem.Obj.Free;
         ISem:= nil;
  above is a corba/raw example, without parameter creates a mutex (maxpermits = 1)
         ALSO SEE: "GetSemaphoreAsMutex" factory below, it creates
         a (mut)ually (ex)clusive lock for use with threading sync }
unit bc_syncobj;
{$mode objfpc}{$H+}
{-$define regsvc}
{-$define debug}
interface
uses Classes, SysUtils, Contnrs, TypInfo{$ifdef debug},bcErrorLog{$endif};
const
  uVersion = '04.30.07.2023'; { added property ID }
  SGUIDIbcSemaphoreRAW = '{EA409651-1738-4D78-BC57-CE42D7816F18}';
type
  TbcSemaphore = class; // forward decl.
{$Region 'Service'}
{$interfaces corba}
  { IbcSemaphoreRAW is used in thread synchronization, maxpermits = 1 ~ acts like a mutex/critsect }
  IbcSemaphoreRAW = interface[SGUIDIbcSemaphoreRAW]
    function get_ID: ptrint;
    function get_MaxPermits: cardinal;
    function get_Object: TbcSemaphore;
    function get_Permits: cardinal;
    function get_WaitCount: cardinal;
    procedure set_ID(aValue: ptrint);
    { Signal releases a slot for others waiting to use }
    procedure Signal;
    { are any of the slots filled }
    function Used: Boolean;
    { if a slot is available then passthrough else wait for a slot to be signaled }
    procedure Wait;
    { optional ID to distinguish it from others }
    property ID: ptrint read get_ID write set_ID;
    { number of slots available in the semphore }
    property MaxPermits: cardinal read get_MaxPermits;
    { implementing object }
    property Obj: TbcSemaphore read get_Object;
    { number of slots occupied }
    property Permits: cardinal read get_Permits;
    { number of clients waiting }
    property WaitCount: cardinal read get_WaitCount;
  end; { IbcSemaphoreRAW }
{$interfaces com}
{$EndRegion 'Service'}
  { TbcSemaphore }
  TbcSemaphore = class(TObject,IbcSemaphoreRAW)
  private
    fID: ptrint;
    fMaxPermits: Cardinal;
    fPermits: cardinal;
    fLock: TRTLCriticalSection;
    fBlockQueue: Contnrs.TQueue;
    function get_ID: ptrint;
    function get_MaxPermits: cardinal;
    function get_Object: TbcSemaphore;
    function get_Permits: cardinal;
    function get_WaitCount: cardinal;
    procedure set_ID(aValue: ptrint);
  public
    constructor Create; overload; { defaults to 1 ~ a mutex }
    constructor Create(MaxPermits: cardinal); overload;
    destructor Destroy; override;
    procedure Signal; { used to be post! }
    function Used: Boolean;
    procedure Wait;
    property ID: ptrint read get_ID write set_ID; { optional ID to distinguish it from others }
    property MaxPermits: cardinal read get_MaxPermits;
    property Obj: TbcSemaphore read get_Object;
    property Permits: cardinal read get_Permits;
    property WaitCount: cardinal read get_WaitCount;
  end; { TbcSemaphore }
{ semaphore weak factory }
function GetSemIntfWeakRAW: pointer;
function GetSemIntfWeakParamsRAW(const Args: array of const): pointer;
{ semaphore "mutex" factory, returns a "corba/raw" interface, i.e.: no ref-counting
  and it exposes an Obj property, that you can use to free it...
  this is NOT a singleton object, creates a new service/interface every time it's invoked! }
function GetSemaphoreAsMutex: IbcSemaphoreRAW;
{ bcIntfIsCorba returns true if the interface provided, is of kind = tkInterfaceRaw, i.e.: corba }
function bcSvcIsCorba(const aSvc: PTypeInfo): boolean; { usage: SEE ABOVE ^^, same as with SvcMgr }

implementation
{$ifdef regsvc}uses bc_servicemanager;{$endif}
{ utility function }
function bcSvcIsCorba(const aSvc: PTypeInfo): boolean;
begin
  Result:= (aSvc^.Kind = tkInterfaceRaw);
end; { bcSvcIsCorba }
{$Region 'Semaphore Factory'}
function GetSemIntfWeakRAW: pointer;
begin
  TbcSemaphore
    .Create
    .GetInterface(SGUIDIbcSemaphoreRAW,Result);
end;

function GetSemIntfWeakParamsRAW(const Args: array of const): pointer;
begin
  TbcSemaphore
    .Create(Args[0].VInteger)
    .GetInterface(SGUIDIbcSemaphoreRAW,Result);
end;

function GetSemaphoreAsMutex: IbcSemaphoreRAW;
begin { this is NOT a singleton object, creates a new service/interface every time it's invoked! }
  TbcSemaphore
    .Create
    .GetInterface(SGUIDIbcSemaphoreRAW,Result);
end;
{$EndRegion 'Semaphore Factory'}
{$Region 'TbcSemaphore'}
{ TbcSemaphore }
function TbcSemaphore.get_WaitCount: cardinal;
begin
  EnterCriticalSection(fLock); try Result:= fBlockQueue.Count; finally LeaveCriticalSection(fLock); end;
end;

procedure TbcSemaphore.set_ID(aValue: ptrint);
begin
  if fID <> aValue then fID:= aValue;
end;

function TbcSemaphore.get_Object: TbcSemaphore;
begin
  Result:= Self;
end;

function TbcSemaphore.get_MaxPermits: cardinal;
begin
  Result:= fMaxPermits;
end;

function TbcSemaphore.get_ID: ptrint;
begin
  Result:= fID;
end;

function TbcSemaphore.get_Permits: cardinal;
begin
  Result:= fPermits;
end;

procedure TbcSemaphore.Wait;
var
  aWait: boolean;
  aEvent: PRTLEvent;
begin
  {$ifdef debug} ErrorLog.LogLn(format('[%s]TbcSemaphore.Wait  : locking... ThreadID = %d',[timetostr(now),ThreadID])); {$endif}
  EnterCriticalSection(fLock);
  try
    {$ifdef debug} ErrorLog.LogLn(format('[%s]TbcSemaphore.Wait  : locked     ThreadID = %d',[timetostr(now),ThreadID])); {$endif}
    if (fPermits > 0) then begin
      dec(fPermits);
      aWait:= false;
    end else begin
      aEvent:= RTLEventCreate;
      fBlockQueue.Push(aEvent);
      aWait:= True;
    end;
  finally
    LeaveCriticalSection(fLock);
  end;
  if aWait then begin
    {$ifdef debug} ErrorLog.LogLn(format('[%s]TbcSemaphore.Wait  : waiting... ThreadID = %d',[timetostr(now),ThreadID])); {$endif}
    RTLeventWaitFor(aEvent);
    RTLEventDestroy(aEvent);
  end;
  {$ifdef debug} ErrorLog.LogLn(format('[%s]TbcSemaphore.Wait  : acquired    ThreadID = %d',[timetostr(now),ThreadID])); {$endif}
end;
 
procedure TbcSemaphore.Signal;
begin
  EnterCriticalSection(fLock);
  try
    if fBlockQueue.Count > 0 then
      RTLEventSetEvent(PRTLEvent(fBlockQueue.Pop))
    else
      inc(fPermits);
  finally
    LeaveCriticalSection(fLock);
  end;
  {$ifdef debug} ErrorLog.LogLn(format('[%s]TbcSemaphore.Signal: released    ThreadID = %d',[timetostr(now),ThreadID])); {$endif}
end;
 
function TbcSemaphore.Used: Boolean;
begin
  EnterCriticalSection(fLock);
  try
    Result:= fPermits < fMaxPermits;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

constructor TbcSemaphore.Create;
begin
  Create(1); { creates a mutex }
end;

constructor TbcSemaphore.Create(MaxPermits: cardinal);
begin
  inherited Create;
  fMaxPermits := MaxPermits;
  fPermits := MaxPermits;
  InitCriticalSection(fLock);
  fBlockQueue:= TQueue.Create;
end;
 
destructor TbcSemaphore.Destroy;
begin
  DoneCriticalSection(fLock);
  fBlockQueue.Free;
  {$ifdef regsvc} SvcMgr.DoNotify(-1,60606,0,0,'(i) TbcSemaphore Destroyed!'); {$endif}
  inherited Destroy;
end;
{$EndRegion 'TbcSemaphore'}
{$ifdef regsvc}
initialization
  SvcMgr.RegisterService('IbcSemaphoreRAW',
                         'bcSemaphore',
                         SGUIDIbcSemaphoreRAW,
                         tkInterfaceRaw,
                         @GetSemIntfWeakRAW,
                         @GetSemIntfWeakParamsRAW,
                         true);
{$endif}
end.
 
