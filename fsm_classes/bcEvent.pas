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
    * Neither the name of "bcEvent" nor the names
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
unit bcEvent;
{$mode ObjFPC}{$H+}
interface
uses
  Classes, SysUtils;
const
  { string-GUID for IbcEvent }
  SGUIDIbcEvent  = '{E80C509F-337A-4278-9BA0-B328A7400AA2}';
type
{$interfaces corba}  { NO reference counting! }
  { IbcEvent is a corba interface for TbcEvent i.e: the contract TbcEvent fulfills }
  IbcEvent = interface [SGUIDIbcEvent]
    function get_Object: TObject;
    { reset, waitfor will sleep until signaled or timeout (could be called sleep) }
    procedure Reset;
    { signal whomever is waiting, they can go on, wake the sleeping (could be called wakeup) }
    procedure Signal;
    { waiting/sleeping indefinitely until signaled ~ released }
    procedure Waitfor; overload;
    { waiting/sleeping maximum aTimeout or being signaled ~ released }
    procedure Waitfor(aTimeout: ptrint); overload;
    { handy in some situations (with raw intf) }
    property Obj: TObject read get_Object;
  end; { IbcEvent }
{$interfaces com}
  { TbcEvent encapsulates the event mechanism in fpc }
  TbcEvent = class(TObject,IbcEvent)
  private
    fEvent: PRTLEvent;
    fLock: TRTLCriticalSection;
    function get_Object: TObject;
  public
    constructor Create; { fevent starts life as "reset" ie. sleeping, you can wait for it }
    Destructor Destroy; override;
    { reset, "waitfor" will sleep until signaled or timeout (could be called sleep) }
    procedure Reset;
    { signal whomever is waiting, they can go on, wake the sleeping (could be called wakeup) }
    procedure Signal;
    { waiting/sleeping indefinitely until signaled ~ released }
    procedure Waitfor; overload;
    { waiting/sleeping maximum aTimeout or being signaled ~ released }
    procedure Waitfor(aTimeout: ptrint); overload;
    { handy in some situations (with raw intf) }
    property Obj: TObject read get_Object;
  end;

implementation

{ TbcEvent }
function TbcEvent.get_Object: TObject;
begin
  Result:= Self;
end;

constructor TbcEvent.Create;
begin
  inherited Create;
  InitCriticalSection(fLock);
  fEvent:= RTLEventCreate; { fevent starts life as "reset" ie. sleeping, you can wait for it }
end;

destructor TbcEvent.Destroy;
begin
  RTLEventDestroy(fEvent);
  DoneCriticalSection(fLock);
  inherited Destroy;
end;

procedure TbcEvent.Reset;
begin { reset, you can wait for it }
  EnterCriticalSection(fLock); try RTLEventResetEvent(fEvent); finally LeaveCriticalSection(fLock); end;
end;

procedure TbcEvent.Signal;
begin { signal whomever is waiting, they can go on }
  EnterCriticalSection(fLock); try RTLEventSetEvent(fEvent); finally LeaveCriticalSection(fLock); end;
end;

procedure TbcEvent.Waitfor;
begin { waiting to be signaled ~ released }
  RTLeventWaitFor(fEvent);
end;

procedure TbcEvent.Waitfor(aTimeout: ptrint);
begin { waiting maximum aTimeout to be signaled ~ released }
  RTLeventWaitFor(fEvent,aTimeout);
end;

end.

