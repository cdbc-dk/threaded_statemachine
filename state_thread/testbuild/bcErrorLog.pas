{$Region 'License'}
{ <begin bsd 3 clause license>
  Copyright ©2020-2023, Benny Christensen a.k.a. cdbc

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of "bcErrorLog" nor the names
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
{*******************************************************************************
*     Unit name : bcErrorLog.pas                                               *
*     Copyright : ©cdbc 2020-2023, portions from RTL © the respective owners   *
*     Programmer: Benny Christensen                                            *
*     Created   : 2020.04.29 /bc basic error log singleton.                    *
*     Updated   : 2022.08.01 /bc added thread safety.                          *
*                 2023.07.27 /bc added interface IbcErrorLog                   *
*                                                                              *
*******************************************************************************}
unit bcErrorLog;
{$mode objfpc}{$H+}
{.$define debug}
interface
uses Classes, SysUtils;
const
  UnitVersion = '6.26.07.2023'; { refactored the whole shebang :o) }
  OSDIRSEPARATOR = {$ifdef unix} '/' {$else} '\' {$endif};
  CRLF = {$ifdef unix} #10 {$else} #13#10 {$endif};
  SGUIDIbcErrorLog = '{51A049E0-7266-4F21-8D8F-0C1AE05CD10B}';

type
  TbcErrorLog = class;
{$Interfaces CORBA} { refcounting OFF }
  { IbcErrorLog is a very basic corba interface/service for logging error-strings,
    you can get at it like this: ErrorLog.GetInterface(SGUIDIbcErrorLog,iELog);
    ErrorLog being the class factory below, which returns a singleton object, so DON'T FREE it! }
  IbcErrorLog = interface[SGUIDIbcErrorLog]
    function get_Filename: string;
    function get_Object: TbcErrorLog;
    procedure set_Filename(aValue: string);
    procedure LogLn(const anErrorLine: string);
    property Filename: string read get_Filename write set_Filename;
    property Obj: TbcErrorLog read get_Object; { handy in some situations (with raw intf) }
  end;
{$Interfaces COM} { refcounting back ON }
  { TbcErrorLog is a very basic errorlogging class }
  TbcErrorLog = class(TObject,IbcErrorLog)
  private
    function get_Object: TbcErrorLog;
  protected
    fFilename: string;
    fFile: TFileStream;
    fLock: TRTLCriticalSection;
    fVersion: shortstring;
    function get_Filename: string; virtual;
    procedure set_Filename(aValue: string); virtual;
    function CheckFile: boolean; virtual;
    procedure CreateLogfile; virtual;
    procedure OpenLogfile; virtual;
  public
    constructor Create(const aFilename: string = '');
    destructor Destroy; override;
    procedure LogLn(const anErrorLine: string); virtual;
    property Filename: string read get_Filename write set_Filename;
    property Obj: TbcErrorLog read get_Object;
    property Version: shortstring read fVersion;
  end;

{ factory-func creates a singleton errorlog }
function ErrorLog(const aFilename: string = ''): TbcErrorLog;

implementation

var   Singleton: TbcErrorLog;

function ErrorLog(const aFilename: string = ''): TbcErrorLog;
begin
	if not assigned(Singleton) then Singleton:= TbcErrorLog.Create(aFilename);
	Result:= Singleton;
end;

{ *** TbcErrorLog *** }
function TbcErrorLog.get_Object: TbcErrorLog;
begin
  Result:= Self;
end;

function TbcErrorLog.get_Filename: string;
begin
  if fFilename <> '' then begin
    Result:= ExtractFileName(fFilename);
  end else Result:= fFilename;
end;

procedure TbcErrorLog.set_Filename(aValue: string);
begin
  EnterCriticalSection(fLock);
  try
    if assigned(fFile) then FreeAndNil(fFile);
    if aValue = '' then fFilename:= ExtractFilePath(ParamStr(0)) + 'error.log'
    else fFilename:= ExtractFilePath(ParamStr(0)) + aValue;
    if not CheckFile then CreateLogfile else OpenLogfile;
  finally LeaveCriticalSection(fLock); end;
end;

function TbcErrorLog.CheckFile: boolean;
begin // locked by set_Filename
  Result:= FileExists(fFilename);
end;

procedure TbcErrorLog.CreateLogfile;
begin // locked by set_Filename
  fFile:= TFileStream.Create(fFilename,fmcreate or fmOpenReadWrite or fmShareDenyNone);
  fFile.Seek(0,soFromEnd); // appending logs to end of file
end;

procedure TbcErrorLog.OpenLogfile;
begin // locked by set_Filename
  fFile:= TFileStream.Create(fFilename,fmOpenReadWrite or fmShareDenyNone);
  fFile.Seek(0,soFromEnd); // appending logs to end of file
end;

constructor TbcErrorLog.Create(const aFilename: string = '');
begin
  inherited Create;
  InitCriticalSection(fLock);
  set_Filename(aFilename); { creates fFile }
  fVersion:= UnitVersion;
end;

destructor TbcErrorLog.Destroy;
begin
  DoneCriticalSection(fLock);
  fFile.Free;
  inherited Destroy;
end;

procedure TbcErrorLog.LogLn(const anErrorLine: string);
var S: string;
begin
  EnterCriticalSection(fLock);
  try
    S:= anErrorLine+CRLF;
    fFile.Write(S[1],length(S));
  finally LeaveCriticalSection(fLock); end;
end;

initialization
  Singleton:= nil;
finalization
  if Singleton <> nil then Singleton.Free;
end.

