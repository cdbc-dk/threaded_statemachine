unit TestStateMachineMain;

{$mode objfpc}{$H+}
{$apptype console}

interface

uses
  Classes, SysUtils, Interfaces, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls,SimpleFiniteStateMachine;

type

  { TState }

  TState = class(TFiniteState)
  public
    procedure Execute; override;
    // Event handler templates
    procedure onStartStopButton; virtual; abstract;
    procedure onResetButton;
    procedure onTimerElapsed; virtual; abstract;
  end;

  { TStateStopped }

  TStateStopped = class(TState)
    procedure onStartStopButton; override;
    procedure onTimerElapsed; override;
  end;

  { TStateCounting }

  TStateCounting = class(TState)
    procedure onStartStopButton; override;
    procedure onTimerElapsed; override;
  end;

  { TStateHold }

  TStateHold = class(TState)
    procedure onStartStopButton; override;
    procedure onTimerElapsed; Override;
  end;


  TFSM = class(TFiniteStateMachine)

  end;


  { TForm1 }

  TForm1 = class(TForm)
    CounterLabel: TLabel;
    StartButton: TButton;
    ResetButton: TButton;
    Timer1: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FCounter: Longint;
    FFirstActivate: boolean;
    FFSM: TFiniteStateMachine;
    procedure SetCounter(AValue: Longint);
  public
    property FSM: TFiniteStateMachine read FFSM;
    property Counter:Longint read FCounter write SetCounter;
    procedure ShowCounter;
    procedure ResetCounter;
    procedure IncCounter;
  end;

var
  Form1: TForm1;

implementation
uses bcSemaphore;
{$R *.lfm}

{ TState }

procedure TState.Execute;

begin
  // Does nothing but display running state in a command window
  writeln('Executing: ' + self.ClassName);
end;

procedure TState.onResetButton;
begin
  ParentFSM.Synchronize(@Form1.ResetCounter);
end;

{ TStateStopped }

procedure TStateStopped.onStartStopButton;

begin
  ParentFSM.setState(TStateCounting);
end;

procedure TStateStopped.onTimerElapsed;
begin
  // Nothing to do here
end;

{ TStateCounting }

procedure TStateCounting.onStartStopButton;

begin
  ParentFSM.setState(TStateHold);
end;

procedure TStateCounting.onTimerElapsed;
begin
  ParentFSM.Synchronize(@Form1.IncCounter);
  ParentFSM.Synchronize(@Form1.ShowCounter);
end;

{ TStateHold }

procedure TStateHold.onStartStopButton;

begin
  ParentFSM.setState(TStateCounting);
end;

procedure TStateHold.onTimerElapsed;
begin
  ParentFSM.Synchronize(@Form1.IncCounter);
end;

{ TForm1 }

procedure TForm1.StartButtonClick(Sender: TObject);

begin
  TState(FSM.State).onStartStopButton;
end;

procedure TForm1.ResetButtonClick(Sender: TObject);
begin
  TState(FSM.State).onResetButton;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  TState(FFSM.State).onTimerElapsed;
end;

procedure TForm1.SetCounter(AValue: Longint);
begin
  if FCounter=AValue then Exit;
  FCounter:=AValue;
end;

procedure TForm1.ShowCounter;

begin
  CounterLabel.Caption := Format('%f',[Counter / 100]);
end;

procedure TForm1.ResetCounter;
begin
  Counter := 0;
  ShowCounter;
end;

procedure TForm1.IncCounter;

begin
  Counter := Counter + 1;
end;

procedure TForm1.FormCreate(Sender: TObject);

begin
  FFirstActivate := True;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  if FFirstActivate then
  begin
    ResetCounter;
    FFSM := TFiniteStateMachine.Create(TStateStopped);
    FFSM.Start;
    Timer1.Enabled := true;
    FFirstActivate := False;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled:= false;
  FFSM.Terminate;
  FFSM.WaitFor;
  FFSM.Free;
end;

end.
