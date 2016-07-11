unit AM.UnitReader.Utils.StrUtils;

interface

type
  TStrUtils = class sealed
  strict private
    const
      cDelimiters = ' ;.,()';
  public
    class function ContainsText(pText: String; pSubText: String): Boolean;
    class function ReplaceText(pText: String; pFromText: String; pToText: String): String;
    class function RemoveSpaces(pString: String): String;
    class function RemoveLineBreaks(pString: String): String;
    class function RemoveDuplicateLineBreaks(pString: String): String;
    class function PosEnd(pSubStr, pString: String): Integer;
    class function PosEndEx(pSubStr, pString: String; pOffSet: Integer = 1): Integer;
    class function PosStart(pSubStr, pString: String): Integer;
    class function PosStartEx(pSubStr, pString: String; pOffSet: Integer = 1): Integer;
    class function CopyAndTrim(pString: String; pPosIni: Integer; pCount: Integer): String;
    class function DeleteAndTrim(pString: String; pPosIni: Integer; pCount: Integer): String;
  end;

implementation

uses
  System.Math,
  System.SysUtils,
  System.StrUtils;

{ TStrUtils }

class function TStrUtils.ContainsText(pText, pSubText: String): Boolean;
var
  lString: String;
  lOffSet, lPosIni: Integer;
  procedure DoCompare;
  begin
    lPosIni := PosStartEx(pSubText, pText, lOffSet);
    lOffSet := lPosIni + Length(pSubText);
    lString := Copy(pText, lPosIni, Length(pSubText));
    Result := SameText(lString, pSubText);
  end;
begin
  lOffSet := 1;
  DoCompare;
  while not Result and (lPosIni > 0) do
  begin
    DoCompare;
  end;
end;

class function TStrUtils.CopyAndTrim(pString: String; pPosIni, pCount: Integer): String;
begin
  Result := Trim(Copy(pString, pPosIni, pCount));
end;

class function TStrUtils.DeleteAndTrim(pString: String; pPosIni, pCount: Integer): String;
begin
  Result := pString;
  Delete(Result, pPosIni, pCount);
  Result := Trim(Result);
end;

class function TStrUtils.PosEnd(pSubStr, pString: String): Integer;
begin
  Result := PosStart(pSubStr, pString) + ifthen(Length(pSubStr) > 1, Length(pSubStr) + 1);
end;

class function TStrUtils.PosEndEx(pSubStr, pString: String; pOffSet: Integer): Integer;
begin
  Result := PosStartEx(pSubStr, pString, pOffSet) + ifthen(Length(pSubStr) > 1, Length(pSubStr));
end;

class function TStrUtils.PosStart(pSubStr, pString: String): Integer;
begin
  Result := Pos(pSubStr, pString) - 1;
end;

class function TStrUtils.PosStartEx(pSubStr, pString: String; pOffSet: Integer): Integer;
begin
  Result := PosEx(pSubStr, pString, pOffSet);
end;

class function TStrUtils.RemoveDuplicateLineBreaks(pString: String): String;
begin
  Result := ReplaceText(pString, sLineBreak + sLineBreak, sLineBreak);
end;

class function TStrUtils.RemoveLineBreaks(pString: String): String;
begin
  Result := ReplaceText(pString, sLineBreak, '');
end;

class function TStrUtils.RemoveSpaces(pString: String): String;
begin
  Result := '';
  if pString <> '' then
  begin
    Result := pString;
    while (Result <> '') and (Result[1] = ' ') do
    begin
      Result := DeleteAndTrim(Result, 1, 1);
    end;
    while (Result <> '') and (Result[Length(Result)] = ' ') do
    begin
      Result := DeleteAndTrim(Result, Length(Result), 1);
    end;
  end;
end;

class function TStrUtils.ReplaceText(pText, pFromText, pToText: String): String;
var
  lPosIni, lOffSet: Integer;
  procedure GetPosIni;
  begin
    lPosIni := PosStartEx(pFromText, Result, lOffSet);
    lOffSet := lPosIni + Length(pFromText);
  end;
begin
  Result := pText;
  lOffSet := 1;
  GetPosIni;
  while lPosIni > 0 do
  begin
    Delete(Result, lPosIni, Length(pFromText));
    Insert(pToText, Result, lPosIni);
    GetPosIni;
  end;
end;

end.
