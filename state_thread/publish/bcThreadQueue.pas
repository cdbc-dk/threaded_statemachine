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
    * Neither the name of "bcThreadQueue" nor the names
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
*        Unit name : bcThreadQueue.pas                                         *
*        Copyright : © 2023 Benny Christensen,                                 *
*                    Classes, SysUtils, Contnrs © the respective owners        *
*        Programmer: Benny Christensen a.k.a. cdbc                             *
*        Created   : 26.07.2023 /bc type definitions for "IbcThreadQueue" and  *
*                                   TbcThreadQueue, applying the "decorator"   *
*                                   -pattern to "TQueue" from "contnrs" unit.  *
*        Updated   : 26.07.2023 /bc added TbcThreadQueue implementation, which *
*                                   adds thread synchronization to TQueue.     *
*                    Usage:                                                    *
*                    var                                                       *
*                      IQ: IbcThreadQueue;                                     *
*                    begin                                                     *
*                      gThreadQueue.GetInterface(SGUIDIbcThreadQueue,IQ);  OR  *
*                      IQ:= ItsQueue; // these 2 accomplish the same thing.    *
*                      ... use IQ.EnQueue(^Something); ...                     *
*                      ... etc.                                                *
*                    end;                                                      *
*                                                                              *
*******************************************************************************}
unit bcThreadQueue;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Contnrs;
const
  SGUIDIbcThreadQueue = '{D9DDD96F-0BCC-4FCF-80B4-1CCED193F451}';
type
  TQueue = Contnrs.TQueue; { alias for the real McCoy }
  TbcThreadQueue = class; { forward decl. }
  EQueueError = class(Exception);
  { IbcThreadQueue is the corba/raw service interface for TbcThreadQueue, thread safe }
{$Interfaces corba} { refcounting OFF }
  IbcThreadQueue = interface[SGUIDIbcThreadQueue]
    function get_Object: TbcThreadQueue; { nifty getter }
    { how many items currently in queue }
    function Count: ptrint;
    { DeQueue retrieves data and removes item from queue, it's the reponsibility of the user,
      to free the object returned!!! throws "EQueueError" on empty queue }
    function DeQueue: pointer;
    { EnQueue puts instantiated data in queue and returns, it is FROWNED UPON to EnQueue(NIL);!!! (AV).... }
    procedure EnQueue(anItem: pointer);
    { well duh! }
    function IsEmpty: boolean;
    { Lock provides locked access to the underlying queue, use try...finally UnLock end; }
    function Lock: TQueue;
    { Peek provides a sneak preview of data, does NOT remove item, DON'T FREE! }
    function Peek: pointer;
    { UnLock releases the queue again, after been locked by "Lock" }
    procedure UnLock;
    { handy in some situations (with raw intf) }
    property Obj: TbcThreadQueue read get_Object;
  end;
{$Interfaces COM} { refcounting back ON }
  { TbcThreadQueue, thread safe implementation of a queue of pointers }
  TbcThreadQueue = class(TObject,IbcThreadQueue)
  private
    fLock: TRTLCriticalSection;
    fQ: TQueue;
    function get_Object: TbcThreadQueue;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: ptrint;
    function DeQueue: pointer;
    procedure EnQueue(anItem: pointer);
    function IsEmpty: boolean;
    function Lock: TQueue;
    function Peek: pointer;
    procedure UnLock;
    property Obj: TbcThreadQueue read get_Object;
  end;

///// ThreadQueue factories, if need be :o) \\\\\
function gThreadQueue: TbcThreadQueue; { singleton, thread safe class }
function ItsQueue: IbcThreadQueue; { singleton, thread safe interface }

implementation
{$Region 'Singleton factories'}
var
  Singleton: TbcThreadQueue;

function gThreadQueue: TbcThreadQueue; { singleton }
begin
  if not assigned(Singleton) then Singleton:= TbcThreadQueue.Create;
  Result:= Singleton;
end; { gets released on progam end }

function ItsQueue: IbcThreadQueue;
begin
  if not assigned(Singleton) then gThreadQueue;
  Result:= Singleton as IbcThreadQueue;
end;
{$EndRegion 'Singleton factories'}
{ TbcThreadQueue }
function TbcThreadQueue.get_Object: TbcThreadQueue;
begin
  EnterCriticalSection(fLock);
  try
    Result:= Self;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

constructor TbcThreadQueue.Create;
begin
  inherited Create;
  InitCriticalSection(fLock);
  fQ:= TQueue.Create;
end;

destructor TbcThreadQueue.Destroy;
begin
  fQ.Free;
  DoneCriticalSection(fLock);
  inherited Destroy;
end;

function TbcThreadQueue.Count: ptrint;
begin
  EnterCriticalSection(fLock);
  try
    Result:= fQ.Count;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

function TbcThreadQueue.DeQueue: pointer;
begin
  if (Count = 0) then raise EQueueError.Create('TbcThreadQueue.DeQueue: Queue is empty');
  EnterCriticalSection(fLock);
  try
    Result:= fQ.Pop;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

procedure TbcThreadQueue.EnQueue(anItem: pointer);
begin
  EnterCriticalSection(fLock);
  try
    fQ.Push(anItem);
  finally
    LeaveCriticalSection(fLock);
  end;
end;

function TbcThreadQueue.IsEmpty: boolean;
begin
  EnterCriticalSection(fLock);
  try
    Result:= (fQ.Count = 0);
  finally
    LeaveCriticalSection(fLock);
  end;
end;

function TbcThreadQueue.Lock: TQueue;
begin
  EnterCriticalSection(fLock);
  Result:= fQ;
end;

function TbcThreadQueue.Peek: pointer;
begin
  if (Count = 0) then raise EQueueError.Create('TbcThreadQueue.Peek: Queue is empty');
  EnterCriticalSection(fLock);
  try
    Result:= fQ.Peek;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

procedure TbcThreadQueue.UnLock;
begin
  LeaveCriticalSection(fLock);
end;

initialization
  Singleton:= nil;

finalization 
  if Singleton <> nil then Singleton.Free;
  
end.

