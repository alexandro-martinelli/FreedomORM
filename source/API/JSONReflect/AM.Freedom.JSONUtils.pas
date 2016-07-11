unit AM.Freedom.JSONUtils;

interface

uses
  System.SysUtils;

type
  TJSONUtils = class sealed
  public
    class function QuoteString(pString: String): String;
    class function JSONCharOfSimpleChar(pSimpleChar: Char): String;
    class function ClassIsList(pMetaClass: TClass): Boolean;
    class function ClassListItemTypeString(pMetaClass: TClass): String;
    class function RemoveFloatingCharacteres(pFloatString: String): String;
  end;

implementation

uses
  System.StrUtils,
  AM.Freedom.JSONConsts;

{ TJSONUtils }

class function TJSONUtils.QuoteString(pString: String): String;
begin
  Result := '"' + pString + '"';
end;

class function TJSONUtils.JSONCharOfSimpleChar(pSimpleChar: Char): String;
var
  lIndex: Integer;
begin
  Result := pSimpleChar;
  for lIndex := Low(TJSONConsts.cSimpleChars) to High(TJSONConsts.cSimpleChars) do
  begin
    if (TJSONConsts.cSimpleChars[lIndex] = pSimpleChar) then
    begin
      Result := TJSONConsts.cJSONChar[lIndex];
      Break;
    end;
  end;
end;

class function TJSONUtils.ClassIsList(pMetaClass: TClass): Boolean;
var
  lClass: TClass;
begin
  Result := False;
  lClass := pMetaClass;
  while (lClass <> nil) and (not Result) do
  begin
    if (ContainsText(lClass.ClassName, 'TList<')) then
    begin
      Result := True;
      Break;
    end;
    lClass := lClass.ClassParent;
  end;
end;

class function TJSONUtils.ClassListItemTypeString(pMetaClass: TClass): String;
var
  lClass: TClass;
  lPosIni: Integer;
begin
  lClass := pMetaClass;
  Result := '';
  while (lClass <> nil) and (Result = '') do
  begin
    if (ContainsText(lClass.ClassName, 'TList<')) then
    begin
      lPosIni := Pos('TList<', lClass.ClassName) + 6;
      Result := Copy(lClass.ClassName, lPosIni, Length(lClass.ClassName) - lPosIni);
      Break;
    end;
    lClass := lClass.ClassParent;
  end;
end;

class function TJSONUtils.RemoveFloatingCharacteres(pFloatString: String): String;
begin
  Result := pFloatString;
  Result := StringReplace(Result, '.', '', [rfReplaceAll]);
  Result := StringReplace(Result, ',', '.', [rfReplaceAll]);
end;

end.