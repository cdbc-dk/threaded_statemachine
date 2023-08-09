unit SimpleFiniteStateMachineStates;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SimpleFiniteStateMachine;

type

  { TState - common routines }

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
    procedure onTimerElapsed; override;
  end;

implementation

uses
  TestStateMachineMain;


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

end.

