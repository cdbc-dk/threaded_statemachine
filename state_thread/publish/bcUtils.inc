{%MainUnit bcMessageQueue.pas}
// a couple of utility funcs :o)
{ bcGetFieldTokenCount takes a string and returns the number of fields, separated by token,
  in it. returns 0 on error, to be used with bcGetFieldToken }
function bcGetFieldTokenCount(const aStr: string; const Token: char): ptrint;
var I,Len: ptrint;
begin
  Len:= Length(aStr); Result:= 0;
  if Len = 0 then exit(Result);
  for I:= 1 to Len do if aStr[I] = Token then inc(Result);
  if aStr[Len] <> Token then inc(Result); // last field
end; { bcGetFieldTokenCount }

{ bcGetFieldToken allows the user to pick a "field" in a string(ansi) separated by a token/char(ansi) }
{ param "FieldNo" is 1-based, i.e.: first field => FieldNo = 1 }
{ usage: Surname:= GetFieldToken(2,'Benny|Christensen','|') => 'Christensen'  or }
{        Firstname:= GetFieldToken(1,'Benny Christensen',' ') => 'Benny' }
{ FieldNo start at 1 and upwards, returns an empty string on error.
  To be used with bcGetFieldTokenCount }
function bcGetFieldToken(const FieldNo: ptrint;
                         const S: string;
                         const Token: char): string;
var
  B,E,C,I,Len: ptrint;
  InField: boolean;
begin
  Len:= system.length(S);                                      { scan sentinel }
  if (Len > 0) and (FieldNo > 0) then begin                 { save clockcycles }
    if pos(Token,S) = 0 then exit(S); { no delimiter?!? return the string untouched }
    I:= 0; C:= 1; InField:= false; B:= -1; E:= -1;            { initialization }
    while (C <= Len) do begin
      if (system.copy(S,C,1) = Token) or
         (C = Len) then inc(I);            { check for Token and end of string }
      if (I = FieldNo-1) and not InField then begin        { 0-based by nature }
        B:= C;                             { point b to beginning of substring }
        InField:= true;               { flag field found, now look for the end }
      end;
      if (I = FieldNo) and InField then begin
        E:= C;                                   { point e to end of substring }
        break;                                  { Field found, we're done here }
      end;
      inc(C);                                               { increment cursor }
    end; { while }                                                { continue ? }
    if (B <> -1) and (E <> -1) then begin
      if E = Len then Result:= system.copy(S,B+1,E-B)       { special cases at }
      else if B = 1 then Result:= system.copy(S,B,E-B)     { beginning and end }
      else Result:= system.copy(S,B+1,E-B-1);
    end else Result:= '';                       { return empty string on error }
  end else Result:= '';           { if fed an empty string return it untouched }
end; { bcgetfieldtoken }

function bcIntfIsCorba(const anIntf: PTypeInfo): boolean;
begin
  Result:= (anIntf^.Kind = tkInterfaceRaw);
end; { bcSvcIsCorba }

// included by bcMessageQueue.pas
// included by bc_sleepthread.pas
