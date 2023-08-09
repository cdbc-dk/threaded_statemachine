unit lfm_wake;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons,
  bcMessageQueue, fsm_engine;

type
  { TForm1, NOTICE how the form implements a dataprovider }
  TForm1 = class(TForm,IfsmDataProvider) { <- right here }
    btnCreThrd: TButton;
    btnFreeThrd: TButton;
    btnStart: TButton;
    btnPause: TButton;
    btnAbort: TButton;
    btnMsgClear: TButton;
    btnDP: TButton;
    chbFsm: TCheckBox;
    chbPaused: TCheckBox;
    chbDbg: TCheckBox;
    gbxAction: TGroupBox;
    gbxMessages: TGroupBox;
    img46: TImageList;
    memMsg: TMemo;
    pnlVis: TPanel;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    btnStartHold: TSpeedButton;
    btnResetStop: TSpeedButton;
    Splitter1: TSplitter;
    Timer1: TTimer;
    procedure btnAbortClick(Sender: TObject);
    procedure btnCreThrdClick(Sender: TObject);
    procedure btnDPClick(Sender: TObject);
    procedure btnFreeThrdClick(Sender: TObject);
    procedure btnMsgClearClick(Sender: TObject);
    procedure btnResetStopClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStartHoldClick(Sender: TObject);
    procedure chbDbgChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fMsgQ: IbcMessageQueue;
    fPause: boolean;
    fSM: TFiniteStateMachine;
    fStateNames: TStringArray;
  public
    procedure HandleDPChange(aStateMachine: TFiniteStateMachine;
                             aSender: TObject;
                             anOperation: TFPObservedOperation;
                             aData: pointer);
    procedure NotifyConsumers(aSender: TObject;anOperation: TFPObservedOperation;aData: Pointer); { <- right here }
    procedure RegisterConsumer(aConsumer: TObject); { <- right here }
    procedure UnRegisterConsumer(aConsumer: TObject); { <- right here }
    property MsgQ: IbcMessageQueue read fMsgQ; { global msgQ }
  end;

var
  Form1: TForm1;

implementation
uses stw_states,LazUTF8;
{$R *.lfm}

{ TForm1 }

procedure TForm1.btnCreThrdClick(Sender: TObject);
begin
  fSM:= TFiniteStateMachine.Create(Handle,MsgQ,chbDbg.Checked);
  RegisterSTWStates(fSM,fStateNames);
  btnCreThrd.Enabled:= false;
  fSM.OnProviderChanged:= @HandleDPChange; // <- remember this to make dataprovider work
end;

procedure TForm1.btnDPClick(Sender: TObject);
var x: string;
begin
  x:= InputBox('JuHuuuu...','Type: Abort / Pause / Stop','Stop');
  { sends the Observed "Self" ~ TForm1 in "aSender" anyway, thus nil }
  NotifyConsumers(nil,ooChange,@x[1]);
end;

procedure TForm1.btnAbortClick(Sender: TObject);
begin
  if Assigned(fSM) then begin
    fSM.Abort;
    fPause:= false;
    fSM.OutQ.EnQueue(pointer(0)); // nifty way to reset counter-panel
  end;
end;

procedure TForm1.btnFreeThrdClick(Sender: TObject);
begin
  if fSM = nil then exit;
  if fSM.Terminate then begin fSM.Free; fSM:= nil; SetLength(fStateNames,0); end
  else memMsg.Append('Cannot free statemachine!');
  btnCreThrd.Enabled:= true;
end;

procedure TForm1.btnMsgClearClick(Sender: TObject);
begin
  memMsg.Clear;
end;

procedure TForm1.btnPauseClick(Sender: TObject);
begin
  if Assigned(fSM) then fSM.Pause;
  fPause:= true;
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  if fSM = nil then exit;

  if btnStart.Tag = 1 then begin { was paused }
    btnStart.Tag:= 0;
    if fPause then fSM.Continue;
    fPause:= false;
    fMsgQ.PostMsg(Handle,4711,0,0,'Continue called...');
  end else begin
    btnStart.Tag:= 1;
    fSM.StartWith(TFiniteState,Self,'Homer');
    fPause:= false;
    fMsgQ.PostMsg(Handle,4711,0,0,'Start called...');
  end;
end;

procedure TForm1.btnStartHoldClick(Sender: TObject);
begin
  if Assigned(fSM) then begin
    if btnStartHold.Tag = 1 then begin
      btnStartHold.Tag:= 0;
      fSM.Pause;
      fPause:= true;
      btnStartHold.ImageIndex:= 1; // play
    end else begin
      if fPause then begin
        fSM.Continue;
        fPause:= false;
        fMsgQ.PostMsg(Handle,4711,0,0,'Continue called...');
        btnStartHold.ImageIndex:= 0; // pause
        btnStartHold.Tag:= 1;
        exit;
      end;
      fSM.Start('Bart');
      fPause:= false;
      btnStartHold.ImageIndex:= 0; // pause
      btnStartHold.Tag:= 1;
      fMsgQ.PostMsg(Handle,4711,0,0,'Start called...');
    end;
  end;
end;

procedure TForm1.chbDbgChange(Sender: TObject);
begin
  if Assigned(fSM) then begin
    fSM.DebugMsg:= chbDbg.Checked;
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(fSM) then btnFreeThrdClick(Sender);
  CanClose:= true;
end;

procedure TForm1.btnResetStopClick(Sender: TObject);
begin
  if Assigned(fSM) then begin
    { both solutions work, the first requires knowledge about the states, }
//    fSM.SetState(TstwStopState,true);
    { but this is the better solution, as it does a "soft" SetState internally ~ no wait }
    fSM.Abort;
    fPause:= false;
    btnStartHold.ImageIndex:= 1; // play
    btnStartHold.Tag:= 0; // start
    fSM.OutQ.EnQueue(pointer(0));
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  memMsg.Clear;
  fMsgQ:= IMsgQ; { connect to our app-wide message queue }
  fMsgQ.PostMsg(Handle,1024,0,0,'Global message queue is up and running... \o/');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  fMsgQ:= nil; { disconnect from our app-wide message queue }
end;

procedure TForm1.Timer1Timer(Sender: TObject); // 40ms => 25 fps ~ like in a movie
const fmt = 'Handle: %d, Msg: %d, Wparam: %d, Lparam: %d%sText: "%s"';
var
  h,msg,w,l,cnt: ptrint;
  txt: string;
begin
  h:= Handle;
  { handle is a var param, i.e.: if it were a while-loop we'd have to put h:= Handle; in the loop last }
  if fMsgQ.GetMsg(h,msg,w,l,txt) then begin
    memMsg.Append(format(fmt,[h,msg,w,l,#10,txt]));
    memMsg.SelStart:= UTF8Length(memMsg.Caption);
    case msg of
      1618: begin { golden ratio ~ 1,618 = state is done, reset controls }
              fPause:= false;
              btnStartHold.ImageIndex:= 1; // play
              btnStartHold.Tag:= 0; // start
            end;
    end;
  end;
  if Assigned(fSM) then begin
    Caption:= 'Items in result-queue: '+fSM.OutQ.Count.ToString;
    while not fSM.OutQ.IsEmpty do begin
      cnt:= ptrint(fSM.OutQ.DeQueue);
      pnlVis.Caption:= format('%10.3f',[Cnt / 1000]);
    end;
    chbFsm.Checked:= fSM.Sleeping;
  end;
  if Assigned(fSM) then chbPaused.Checked:= fPause;
end;

procedure TForm1.HandleDPChange(aStateMachine: TFiniteStateMachine;
  aSender: TObject; anOperation: TFPObservedOperation; aData: pointer);
var s: string;
begin
  s:= pchar(aData);
  pnlBottom.Caption:= s;
  if LowerCase(s) = 'abort' then aStateMachine.Abort;
  if LowerCase(s) = 'pause' then begin
    aStateMachine.Pause;
    fPause:= true;
  end;
  if LowerCase(s) = 'stop' then aStateMachine.SetState(TstwStopState,true);
end;

procedure TForm1.NotifyConsumers(aSender: TObject;
  anOperation: TFPObservedOperation; aData: Pointer);
begin { relay to observed, in another class, you'll have to implement yourself :-) }
  FPONotifyObservers(aSender,anOperation,aData);
end;

procedure TForm1.RegisterConsumer(aConsumer: TObject);
begin { relay to observed, in another class, you'll have to implement yourself :-) }
  FPOAttachObserver(aConsumer);
end;

procedure TForm1.UnRegisterConsumer(aConsumer: TObject);
begin { relay to observed, in another class, you'll have to implement yourself :-) }
  FPODetachObserver(aConsumer);
end;

end.

