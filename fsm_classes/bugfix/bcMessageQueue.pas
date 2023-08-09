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
    * Neither the name of "bcMessageQueue" nor the names
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
*  Unit name : bcMessageQueue.pas                                              *
*  Copyright : © 2023 Benny Christensen,                                       *
*              Classes, SysUtils, Contnrs © the respective owners              *
*  Programmer: Benny Christensen a.k.a. cdbc                                   *
*  Created   : 27.07.2023 /bc type definitions for "IbcMessageQueue" and       *
*                             TbcMessageQueue, applying the "decorator"        *
*                             -pattern to "TQueue" from "contnrs" unit.        *
*  Updated   : 27.07.2023 /bc added TbcMessageQueue implementation, which      *
*                             adds thread synchronization to TQueue, AMO.      *
*              28.07.2023 /bc added "Get", "Peek" & "Post" -Msg, mechanism     *
*              29.07.2023 /bc fixed bug in "PeekMsg" that broke contract,added *
*                             "debug" define, "heaptrc" & "ReleaseSingleton"   *
*              Usage:                                                          *
*              var                                                             *
*                IQ: IbcMessageQueue;                                          *
*              begin                                                           *
*                IQ:= IMsgQ;                                                   *
*                ... use IQ.PostMsg(params);                                   *
*                //somewhere else: while IMsgQ.GetMsg(params) do stuff...      *
*                OR                                                            *
*                gMessageQueue.PostMsg(-1,rest of params);                     *
*                //somewhere else:                                             *
*                with gMessageQueue.DeQueue do begin                           *
*                  ... use properties of TbcMessage: Memo1.Append(Text);       *
*                  Free; // free the msg-object, IMPORTANT!                    *
*                end;                                                          *
*              end;                                                            *
*  ...Or one can just use them as the classes and interfaces they are :o)      *
*                                                                              *
*******************************************************************************}
unit bcMessageQueue;
{$mode objfpc}{$H+}
{-$define debug}
interface
uses Classes, SysUtils, Contnrs{$ifdef debug},heaptrc{$endif};
const
  UnitVersion = '01.29.07.2023'; { v2, bugfix in "PeekMsg" }
  SGUIDIbcMessageQueue = '{D7579DD2-2CA1-453B-BC75-253E60AB549B}';
type
  TQueue = Contnrs.TQueue; { alias for the real McCoy }
  TbcMessageQueue = class; { forward decl. }
  EQueueError = class(Exception);
{$Region 'TbcMessage'}
  { TbcMessage is the actual msg send back and forth via the msgqueue }
  TbcMessage = class
  private
    fHandle: ptrint;
    fMsg: ptrint;
    fWParam: ptrint; { pointer sized integer, could hold anything ;-) }
    fLParam: ptrint; { pointer sized integer, could hold anything ;-) }
    fText: string;
    function CloneString(const aString: string): string;
  public
    constructor Create(const aHandle,aMsg,aWParam,aLParam: ptrint;const aText: string); overload;
    constructor Create(const aSerializedString: string); overload;
    procedure Assign(const aSrc: TbcMessage);
    function Clone: TbcMessage;
    function Serialize: string;
    property Handle: ptrint read fHandle write fHandle;
    property Msg: ptrint read fMsg write fMsg;
    property WParam: ptrint read fWParam write fWParam;
    property LParam: ptrint read fLParam write fLParam;
    property Text: string read fText write fText; // optional string param
    property AsString: string read Serialize;
  end; { TbcMessage }
{$EndRegion 'TbcMessage'}
  { IbcMessageQueue is the corba/raw service interface for TbcMessageQueue, thread safe msgqueue }
{$Interfaces CORBA} { refcounting OFF }
  IbcMessageQueue = interface[SGUIDIbcMessageQueue]
    function get_Object: TbcMessageQueue; { nifty getter }
    { Clear throws all messages away and clears the queue }
    procedure Clear;
    { how many items currently in queue }
    function Count: ptrint;
    { DeQueue retrieves data and removes item from queue, it's the reponsibility of the user,
      to free the object returned!!! throws "EQueueError" on empty queue }
    function DeQueue: TbcMessage;
    { EnQueue puts instantiated data in queue and returns, it is FROWNED UPON to EnQueue(NIL);!!! (AV).... }
    procedure EnQueue(aMessage: TbcMessage);
    { GetMsg checks the queue for messages sent to "aHandle" or messages sent to -1 (anyone),
      if found, fills in the variables and returns true, if queue is empty or the message waiting,
      is not for us or (anyone), it clears variables and return false }
    function GetMsg(var aHandle: ptrint;out aMsg,aWParam,aLParam: ptrint;out aText: string): boolean;
    { IsEmpty -> well duh! }
    function IsEmpty: boolean;
    { Lock provides locked access to the underlying queue, use try...finally UnLock end; }
    function Lock: TQueue;
    { PeekMsg provides a sneak preview of data, does NOT remove item, DON'T FREE!,
      if unsuccessful returns NIL in "aMessage" and false }
    function PeekMsg(out aMessage: TbcMessage): boolean;
    { :o) almost modelled after win-msg-mechanism }
    procedure PostMsg(const aHandle,aMsg,aWParam,aLParam: ptrint;const aText: string);
    { UnLock releases the queue again, after been locked by "Lock" }
    procedure UnLock;
    { handy property in some situations (with raw intf) }
    property Obj: TbcMessageQueue read get_Object;
  end;
{$Interfaces COM} { refcounting back ON }
  { TbcMessageQueue, thread safe implementation of a messagequeue }
  TbcMessageQueue = class(TObject,IbcMessageQueue)
  private
    fLock: TRTLCriticalSection;
    fQ: TQueue;
    function get_Object: TbcMessageQueue;
  public
    constructor Create;
    destructor Destroy; override;
    { throws all messages away and clears the queue }
    procedure Clear;
    { how many items currently in queue }
    function Count: ptrint;
    { DeQueue retrieves data and removes item from queue, it's the reponsibility of the user,
      to free the object returned!!! throws "EQueueError" on empty queue }
    function DeQueue: TbcMessage;
    { puts instantiated data in queue and returns, it is frowned upon to EnQueue(NIL);!!! AVvvvv.... }
    procedure EnQueue(aMessage: TbcMessage);
    { GetMsg checks the queue for messages sent to "aHandle" or messages sent to -1 (anyone),
      if found, fills in the variables and returns true, if queue is empty or the message waiting,
      is not for us or (anyone), it clears variables and return false }
    function GetMsg(var aHandle: ptrint;out aMsg,aWParam,aLParam: ptrint;out aText: string): boolean;
    { well duh! }
    function IsEmpty: boolean;
    { provides locked access to the underlying queue, use try...finally UnLock end; }
    function Lock: TQueue;
    { PeekMsg provides a sneak preview of data, does NOT remove item, DON'T FREE!,
      if unsuccessful returns NIL in "aMessage" and false }
    function PeekMsg(out aMessage: TbcMessage): boolean;
    { :o) }
    procedure PostMsg(const aHandle,aMsg,aWParam,aLParam: ptrint;const aText: string);
    { releases the queue again, after been locked by "Lock" }
    procedure UnLock;
    { handy in some situations (with raw intf) }
    property Obj: TbcMessageQueue read get_Object;
  end; { TbcMessageQueue }

///// MessageQueue Singleton factories, interchangeable = same Q, if need be :o) \\\\\
function gMessageQueue: TbcMessageQueue; { singleton, thread safe MsgQ class }
function IMsgQ: IbcMessageQueue; { singleton, thread safe MsgQ interface }
{$ifdef debug}procedure ReleaseSingleton;{$endif} { for debugging purposes ONLY! }

implementation
{$Region 'Singleton factories'}
var
  Singleton: TbcMessageQueue;

function gMessageQueue: TbcMessageQueue; { singleton }
begin
  if not assigned(Singleton) then Singleton:= TbcMessageQueue.Create;
  Result:= Singleton;
end; { gets released on progam end }

function IMsgQ: IbcMessageQueue;
begin
  if not assigned(Singleton) then gMessageQueue;
  Result:= Singleton as IbcMessageQueue;
end; { follows class }
{$EndRegion 'Singleton factories'}
{$i bcUtils.inc} // include a couple of uility funcs
{$i bcMessage.inc} // include TbcMessage implementation
{ TbcMessageQueue }
function TbcMessageQueue.get_Object: TbcMessageQueue;
begin
  Result:= Self;
end;

constructor TbcMessageQueue.Create;
begin
  inherited Create;
  InitCriticalSection(fLock);
  fQ:= TQueue.Create;
end;

destructor TbcMessageQueue.Destroy;
begin
  Clear;
  fQ.Free;
  DoneCriticalSection(fLock);
  inherited Destroy;
end;

procedure TbcMessageQueue.Clear;
begin
  EnterCriticalSection(fLock);
  try while fQ.Count > 0 do TbcMessage(fQ.Pop).Free;
  finally LeaveCriticalSection(fLock); end;
end;

function TbcMessageQueue.Count: ptrint;
begin
  EnterCriticalSection(fLock);
  try Result:= fQ.Count;
  finally LeaveCriticalSection(fLock); end;
end;

function TbcMessageQueue.DeQueue: TbcMessage;
begin
  if (Count = 0) then raise EQueueError.Create('TbcMessageQueue.DeQueue: Queue is empty');
  EnterCriticalSection(fLock);
  try Result:= TbcMessage(fQ.Pop);
  finally LeaveCriticalSection(fLock); end;
end;

procedure TbcMessageQueue.EnQueue(aMessage: TbcMessage);
begin
  EnterCriticalSection(fLock);
  try fQ.Push(pointer(aMessage));
  finally LeaveCriticalSection(fLock); end;
end;

function TbcMessageQueue.GetMsg(var aHandle: ptrint;out aMsg,aWParam,aLParam: ptrint;out aText: string): boolean;
var M: TbcMessage;
begin
  EnterCriticalSection(fLock); { acquire lock }
  try
    Result:= (fQ.Count > 0);
    if Result then begin
      if TbcMessage(fQ.Peek).Handle = aHandle then begin { it's for us }
        M:= TbcMessage(fQ.Pop); { removes msg from q }
        aHandle:= M.Handle;
        aMsg:= M.Msg;
        aWParam:= M.WParam;
        aLParam:= M.LParam;
        aText:= M.Text;
        M.Free; { dispose of msg-item }
      end else begin
        if TbcMessage(fQ.Peek).Handle = -1 then begin { it's for anyone }
          M:= TbcMessage(fQ.Pop); { removes msg from q }
          aHandle:= M.Handle;
          aMsg:= M.Msg;
          aWParam:= M.WParam;
          aLParam:= M.LParam;
          aText:= M.Text;
          M.Free; { dispose of msg-item }
        end else begin       { when not (Handle = aHandle or Handle = -1) then }
          aHandle:= -1;      { clear }
          aMsg:= -1;         { the }
          aWParam:= -1;      { variables }
          aLParam:= -1;      { & }
          aText:= '';        { return false, }
          exit(false);       { not a msg that concerns us }
        end; { someone else's stuff }
      end; { Handle <> aHandle }
    end else begin { queue is empty, clear vars }
      aHandle:= -1;
      aMsg:= -1;
      aWParam:= -1;
      aLParam:= -1;
      aText:= '';
    end; { not result }
  finally LeaveCriticalSection(fLock); end; { release lock }
end;

function TbcMessageQueue.IsEmpty: boolean;
begin
  EnterCriticalSection(fLock);
  try Result:= (fQ.Count = 0);
  finally LeaveCriticalSection(fLock); end;
end;

function TbcMessageQueue.Lock: TQueue;
begin
  EnterCriticalSection(fLock);
  Result:= fQ;
end;

function TbcMessageQueue.PeekMsg(out aMessage: TbcMessage): boolean;
begin
  EnterCriticalSection(fLock);
  try
    Result:= (fQ.Count > 0);
    if Result then aMessage:= TbcMessage(fQ.Peek) else aMessage:= nil;
  finally LeaveCriticalSection(fLock); end;
end;

procedure TbcMessageQueue.PostMsg(const aHandle,aMsg,aWParam,aLParam: ptrint;const aText: string);
var M: TbcMessage;
begin
  EnterCriticalSection(fLock);
  try
    M:= TbcMessage.Create(aHandle,aMsg,aWParam,aLParam,aText);
    fQ.Push(pointer(M));
  finally LeaveCriticalSection(fLock); end;
end;

procedure TbcMessageQueue.UnLock;
begin
  LeaveCriticalSection(fLock);
end;

{$ifdef debug} procedure ReleaseSingleton; begin if Singleton <> nil then FreeAndNil(Singleton); end; {$endif}

initialization
  Singleton:= nil;

finalization 
  if Singleton <> nil then Singleton.Free;
  
end.

