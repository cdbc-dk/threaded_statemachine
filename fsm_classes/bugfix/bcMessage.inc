{%MainUnit bcMessageQueue.pas}

{ TbcMessage }
function TbcMessage.CloneString(const aString: string): string;
var Len: ptrint;
begin
  if aString = '' then exit('');
  Len:= Length(aString);
  SetLength(Result,Len);
  move(aString[1],Result[1],Len); // should make it unique?!?
end;

constructor TbcMessage.Create(const aHandle, aMsg, aWParam, aLParam: ptrint;
  const aText: string);
begin
  inherited Create;
  fHandle:= aHandle;
  fMsg:= aMsg;
  fWParam:= aWParam;
  fLParam:= aLParam;
  fText:= CloneString(aText); { should be unique?!? }
end;

constructor TbcMessage.Create(const aSerializedString: string);
begin
  inherited Create;
  fHandle:= bcGetFieldToken(1,aSerializedString,'|').ToInt64;
  fMsg:= bcGetFieldToken(2,aSerializedString,'|').ToInt64;
  fWParam:= bcGetFieldToken(3,aSerializedString,'|').ToInt64;
  fLParam:= bcGetFieldToken(4,aSerializedString,'|').ToInt64;
  fText:= bcGetFieldToken(5,aSerializedString,'|');
end;

procedure TbcMessage.Assign(const aSrc: TbcMessage);
begin
  Self.fHandle:= aSrc.Handle;
  Self.fMsg:= aSrc.Msg;
  Self.fWParam:= aSrc.WParam;
  Self.fLParam:= aSrc.LParam;
  Self.fText:= aSrc.Text;
end;

function TbcMessage.Clone: TbcMessage;
begin
  Result:= TbcMessage.Create(Self.fHandle,
                             Self.fMsg,
                             Self.fWParam,
                             Self.fLParam,
                             Self.fText);
end;

{ serializes message fields to a string, handy for e.g.: socket comms }
function TbcMessage.Serialize: string;
begin { TODO: perhaps format return string as json?!? }
  Result:= fHandle.ToString+'|'+
           fMsg.ToString+'|'+
           fWParam.ToString+'|'+
           fLParam.ToString+'|'+
           fText;
end;
// included by bcMessageQueue.pas
