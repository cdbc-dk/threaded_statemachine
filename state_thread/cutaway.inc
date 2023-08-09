(* CUTAWAY file for lfm_wake.pas

{$i cutaway.inc}

procedure TForm1.btnRegStartClick(Sender: TObject);
begin
  if Assigned(fSM) then begin
    if fPause then begin
      fSM.Continue;
      fPause:= false;
      fMsgQ.PostMsg(Handle,4711,0,0,'Reg. Continue called...');
      exit;
    end;
    fSM.Start('Gyngehesterøvhul');
    fPause:= false;
    fMsgQ.PostMsg(Handle,4711,0,0,'Reg. Start called...');
  end;
end;

procedure TForm1.btnRegPauseClick(Sender: TObject);
begin
  if Assigned(fSM) then fSM.Pause;
  fPause:= true;
end;


*) 
