unit AM.UnitReader.Readers.MethodReader;

interface

uses
  AM.UnitReader.Readers.CustomReader,
  AM.UnitReader.Members.MethodMember,
  AM.UnitReader.Enumerations, AM.UnitReader.Members.CustomMember, AM.UnitReader.Members.ArgumentMember,
  AM.UnitReader.Helper.MethodType;

type
  TMethodReader = class(TCustomReader)
  strict protected
    function ExtractName: string; override;
    function GetMemberClass: TMemberClass; override;
    procedure DoRead; override;
  public
    class function MethodFromStr(pMethodStr: String; pVisibility: TVisibilityScope): TMethodMember;
  end;

implementation

uses
  System.StrUtils, System.SysUtils;

{ TMethodReader }

procedure TMethodReader.DoRead;
var
  lArgument: String;
  lMethod: TMethodMember;
  lMethodStr: string;
  lName: String;
  procedure GetIsClassMethod;
  begin
    lMethod.IsClassMethod := StartsText('class', lMethodStr);
    if lMethod.IsClassMethod then
    begin
      Delete(lMethodStr, 1, 5);
    end;
  end;
  procedure GetMethodType;
  var
    lCounter: Integer;
  begin
    for lCounter := Ord(Low(TMethodType)) to Ord(High(TMethodType)) do
    begin
      if ContainsText(lMethodStr, TMethodType(lCounter).ToString) then
      begin
        lMethod.MethodType :=  TMethodType(lCounter);
        Break;
      end;
    end;
  end;
begin
  lMethod := TMethodMember(CreatedMember);
  lMethod.Arguments.Clear;
  lMethodStr  := RemoveLineBreaks(RemoveSpaces(GetMemberStr));
  GetIsClassMethod;
  GetMethodType;
  if ContainsText(lMethodStr, '(') then
  begin
    lMethodStr := DeleteAndTrim(lMethodStr, 1, Pos('(', lMethodStr));
    lMethodStr := DeleteAndTrim(lMethodStr, Pos(')', lMethodStr), length(lMethodStr)) + ';';
    while ContainsText(';', lMethodStr) or (lMethodStr <> '') do
    begin
      lArgument := CopyAndTrim(lMethodStr, 1, PosStart(':', lMethodStr));
      while ContainsText(lArgument, ',') and (lArgument <> '') do
      begin
        lName := CopyAndTrim(lArgument, 1, PosStart(',', lArgument));
        lMethod.Arguments.Add(TArgumentMember.Create(lName));
        if ContainsText(lArgument, ',') then
        begin
          lArgument := DeleteAndTrim(lArgument, 1, Pos(',', lArgument));
        end else
        begin
          lArgument := '';
        end;
      end;
      if lArgument <> '' then
      begin
        lMethod.Arguments.Add(TArgumentMember.Create(lArgument));
      end;
      if Pos(';', lMethodStr) > 0 then
      begin
        lMethodStr := DeleteAndTrim(lMethodStr, 1, Pos(';', lMethodStr))
      end else
      begin
        lMethodStr := '';
      end;
    end;
  end;
end;

function TMethodReader.ExtractName: string;
var
  lPos: Integer;
  procedure SearchInitialPosition;
  begin
    if ContainsText(Result, 'function') then
    begin
      lPos := PosEnd('function', Result);
    end
    else if ContainsText(Result, 'procedure') then
    begin
      lPos := PosEnd('procedure', Result);
    end
    else if ContainsText(Result, 'constructor') then
    begin
      lPos := PosEnd('constructor', Result);
    end
    else if ContainsText(Result, 'destructor') then
    begin
      lPos := PosEnd('destructor', Result);
    end;
  end;
  procedure SearchEndPosition;
  begin
    if ContainsText(Result, '(') then
    begin
      lPos := PosStart('(', Result);
    end
    else if ContainsText(Result, ':') then
    begin
      lPos := PosStart(':', Result);
    end
    else
    begin
      lPos := PosStart(';', Result);
    end;
  end;

begin
  Result := RemoveLineBreaks(RemoveSpaces(GetMemberStr));
  SearchInitialPosition;
  Result := Trim(Copy(Result, lPos, Length(Result)));
  SearchEndPosition;
  Result := Copy(Result, 1, lPos);
end;

function TMethodReader.GetMemberClass: TMemberClass;
begin
  Result := TMethodMember;
end;

class function TMethodReader.MethodFromStr(pMethodStr: String; pVisibility: TVisibilityScope): TMethodMember;
var
  lMethodReader: TMethodReader;
begin
  lMethodReader := TMethodReader.Create(pMethodStr, pVisibility);
  try
    Result := TMethodMember(lMethodReader.Read);
  finally
    lMethodReader.Free;
  end;
end;

end.
