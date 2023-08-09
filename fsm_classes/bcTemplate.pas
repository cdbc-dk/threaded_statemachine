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
*                                                                              *
*******************************************************************************}
unit bcTemplate;
{$mode objfpc}{$H+}
{-$define debug}
interface
uses Classes, SysUtils{$ifdef debug},heaptrc{$endif};
const
  UnitVersion = '01.29.07.2023'; { v2, bugfix in "PeekMsg" }
  SGUID = '{D7579DD2-2CA1-453B-BC75-253E60AB549B}';
type
  TQueue = Contnrs.TQueue; { alias for the real McCoy }
  TbcMessageQueue = class; { forward decl. }
  EQueueError = class(Exception);
{$Region 'TbcMessage'}

{$EndRegion 'TbcMessage'}
  { IbcMessageQueue is the corba/raw service interface for TbcMessageQueue, thread safe msgqueue }
{$Interfaces CORBA} { refcounting OFF }
  IbcSomething = interface[SGUIDIbcMessageQueue]
    function get_Object: TbcMessageQueue; { nifty getter }

    { handy property in some situations (with raw intf) }
    property Obj: TbcMessageQueue read get_Object;
  end;
{$Interfaces COM} { refcounting back ON }
  { TbcMessageQueue, thread safe implementation of a messagequeue }
  TbcSomething = class(TObject,IbcMessageQueue)
  private
    function get_Object: TbcMessageQueue; { nifty getter }
  public
    constructor Create;
    destructor Destroy; override;


    property Obj: TbcMessageQueue read get_Object;
  end; { TbcMessageQueue }

///// MessageQueue Singleton factories, interchangeable = same Q, if need be :o) \\\\\
function gMessageQueue: TbcMessageQueue; { singleton, thread safe MsgQ class }
function IMsgQ: IbcMessageQueue; { singleton, thread safe MsgQ interface }
{$ifdef debug}procedure ReleaseSingleton;{$endif} { for debugging purposes ONLY! }

implementation
{$Region 'Singleton factories'}

{$EndRegion 'Singleton factories'}
{$i bcUtils.inc} // include a couple of uility funcs
{-$i bcMessage.inc} // include TbcMessage implementation
{ TbcMessageQueue }
function TbcMessageQueue.get_Object: TbcMessageQueue;
begin
  Result:= Self;
end;




procedure TbcMessageQueue.UnLock;
begin
  LeaveCriticalSection(fLock);
end;

{$ifdef debug}  {$endif}

initialization
  Singleton:= nil;

finalization 
  if Singleton <> nil then Singleton.Free;
  
end.

