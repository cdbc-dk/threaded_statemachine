program QueueTest;
{$mode objfpc}{$H+}
uses {$IFDEF UNIX} cthreads, {$ENDIF} Classes,sysutils, bcMessageQueue;
type TProc = procedure(const S: string);
var
  m: TbcMessage = nil;
  h,ms,w,l: ptrint;
  txt: string;

procedure ErrLn(const aStr: string);
const fmt = '(!) Error: %s';
begin
  writeln(format(fmt,[aStr]));
end;

procedure ProcTest(const S: string); begin writeln('(T) ',S,' Queued procs works \o/'); end;

{$R *.res}
begin
  writeln('> QueueTest, testing bcMessageQueue :o) <');
  writeln('> QueueTest [-e] test the only exception ;-) <');
  if ParamStr(1) = '-e' then begin
    writeln('(?) Testing "DeQueue" on empty queue...');
    try m:= IMsgQ.DeQueue; except on E:EQueueError do ErrLn(E.Message); end;
  end;
  writeln('(*) EnQueueing messages...');
  IMsgQ.PostMsg(97,4711,53,1970,'Benny Christensen');
  gMessageQueue.EnQueue(TbcMessage.Create(-1,1961,0,0,'Sabine Hofmann'));
  IMsgQ.PostMsg(88,4711,2,2021,'Sheeba Møjert');
  h:= 97; // our handle
  writeln('(i) Our Handle: ',h,', Count of MsgQ: ',IMsgQ.Count,', DeQueueing messages with GetMsg...');
  while IMsgQ.GetMsg(h,ms,w,l,txt) do begin
    writeln('(Q) ',h,#9,ms,#9,w,#9,l,#9,txt);
  end;
  writeln('(i) Our Handle: ',h,', Count of MsgQ: ',IMsgQ.Count,', DeQueueing messages with DeQueue...');
  while gMessageQueue.Count > 0 do with gMessageQueue.DeQueue do begin
    writeln('(Q) ',Handle,#9,Msg,#9,WParam,#9,LParam,#9,Text);
    Free;
  end;
  h:= 111;
  writeln('(i) Our Handle: ',h,', Count of MsgQ: ',IMsgQ.Count,', EnQueueing message containing TProc...');
  IMsgQ.PostMsg(111,1024,0,ptrint(@ProcTest),'(*) Lyserøde Gyngehesterøvhuller (*)');
  writeln('(i) Our Handle: ',h,', Count of MsgQ: ',IMsgQ.Count,', Testing PeekMsg...');
  if IMsgQ.PeekMsg(m) then writeln('(P) Success! ',m.Text);
  writeln('(i) Our Handle: ',h,', Count of MsgQ: ',IMsgQ.Count,', Testing GetMsg with TProc...');
  if IMsgQ.GetMsg(h,ms,w,l,txt) then TProc(l)(txt);
//  ReleaseSingleton; // debug purposes, do not use in production!
  writeln('(i) The 2 singletons class & interface, are interchangeable ;-) ,Bye');
end.

