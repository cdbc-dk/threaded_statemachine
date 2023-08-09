{%MainUnit fsm_engine.pas}
{ (c)2023 Benny Christensen a.k.a. cdbc }

{ TfsmStateList }
function TfsmStateList.get_Count: integer;
begin
  Result:= fList.Count;
end;

function TfsmStateList.get_Item(Index: integer): TFiniteState;
begin
  Result:= TFiniteState(fList.Items[Index]);
end;

procedure TfsmStateList.set_Item(Index: integer; aValue: TFiniteState);
begin
  if fOwnsObjects then fList.Items[Index].Free;
  fList.Items[Index]:= aValue;
end;

constructor TfsmStateList.Create(OwnsObjects: boolean);
begin
  inherited Create;
  fList:= TFPObjectList.Create(OwnsObjects);
  fOwnsObjects:= OwnsObjects;
end;

destructor TfsmStateList.Destroy;
begin
  Clear; { frees objects if owned }
  inherited Destroy;
end;

function TfsmStateList.AddState(aState: TFiniteState): integer;
begin
  Result:= fList.Add(aState);
end;

procedure TfsmStateList.Clear;
begin
  fList.Clear; { frees objects per default }
end;

function TfsmStateList.Contains(aClass: TFiniteStateClass): boolean;
begin
  Result:= (GetStateByClass(aClass) <> nil);
end;

function TfsmStateList.GetStateByClass(aClass: TFiniteStateClass): TFiniteState;
var I: integer;
begin
  Result:= nil;
  for I:= 0 to fList.Count-1 do begin
    if fList[I].ClassType = aClass.ClassType then begin
      Result:= TFiniteState(fList[I]);
      break;
    end;
  end;
end;

function TfsmStateList.GetStateByName(const aName: string): TFiniteState;
var I: integer;
begin
  Result:= nil;
  for I:= 0 to fList.Count-1 do begin
    if fList[I].ClassName = aName then begin
      Result:= TFiniteState(fList[I]);
      break;
    end;
  end;
end;

// included by fsm_engine.pas
