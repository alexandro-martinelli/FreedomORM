unit AM.Freedom.SQLMappers.SQLFormatter;

interface

uses
  System.Classes,
  AM.Freedom.SQLMappers.ISQLFormatter,
  AM.Freedom.EnumerationTypes, AM.Freedom.DefaultsClassRegister;

type
  TSQLFormatter = class(TInterfacedObject, ISQLFormatter)
  strict private const
    cAspas       = '''';
    cEspaco      = ' ';
    cEspacoDuplo = '  ';
    cSepVirgula  = ', ';
  strict private
    FCodeWordCharCase: TCharCase;
    FKeyWordCharCase: TCharCase;
    function SplitStringEx(pSQL, pDelimiters: String): TArray<String>;
    function FormatToCase(pWord: String; pCase: TCharCase): String;
    function IsToken(const pStr: String): Boolean;
    function IsValidSQLWord(pStr: String): Boolean;
  strict protected
    function GetCodeWordCharCase: TCharCase;
    function GetKeyWordCharCase: TCharCase;
    procedure SetCodeWordCharCase(const pCharCase: TCharCase);
    procedure SetKeyWordCharCase(const pCharCase: TCharCase);
  public
    constructor Create(pAsDefault: Boolean = True); virtual;
    function FormatSQLText(const pSQL: string; pKeyWords: TStrings): string;
    property CodeWordCharCase: TCharCase read GetCodeWordCharCase write SetCodeWordCharCase default TCharCase.Upper;
    property KeyWordCharCase: TCharCase read GetKeyWordCharCase write SetKeyWordCharCase default TCharCase.Lower;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.Consts,
  System.Types;

{ TSQLFormater }

constructor TSQLFormatter.Create(pAsDefault: Boolean = True);
begin
  FCodeWordCharCase := TCharCase.Upper;
  FKeyWordCharCase := TCharCase.Lower;
  if pAsDefault then
  begin
    TDefaultsClassRegister.DefaultSQLFormatter := Self;
  end;
end;

function TSQLFormatter.FormatSQLText(const pSQL: string; pKeyWords: TStrings): string;
var
  lWord: string;
  lArStrs: TArray<string>;
  I: Integer;
begin
  lArStrs := SplitStringEx(pSQL, ' ,.()/*-+|=;');
  try
    Result := EmptyStr;
    for I := Low(lArStrs) to High(lArStrs) do
    begin
      lWord := lArStrs[I];
      if (Pos(cAspas, lWord) <= 0) and IsValidSQLWord(lWord) then
      begin
        if pKeyWords.IndexOf(lWord) >= 0 then
        begin
          lWord := FormatToCase(lWord, FKeyWordCharCase);
        end
        else
        begin
          lWord := FormatToCase(lWord, FCodeWordCharCase);
        end;
      end;
      Result := Result + lWord;
    end;
  finally
    FreeAndNil(pKeyWords);
  end;
end;

function TSQLFormatter.FormatToCase(pWord: String; pCase: TCharCase): String;
begin
  case pCase of
    Upper: Result := AnsiUpperCase(pWord);
    Lower: Result := AnsiLowerCase(pWord);
  end;
end;

function TSQLFormatter.GetCodeWordCharCase: TCharCase;
begin
  Result := FCodeWordCharCase;
end;

function TSQLFormatter.GetKeyWordCharCase: TCharCase;
begin
  Result := FKeyWordCharCase;
end;

function TSQLFormatter.IsToken(const pStr: String): Boolean;
var
  I: Integer;
begin
  for I := Low(TConsts.cSQLTokens) to High(TConsts.cSQLTokens) do
  begin
    Result := AnsiSameText(TConsts.cSQLTokens[I], pStr);
    if Result then
    begin
      Break;
    end;
  end;
end;

function TSQLFormatter.IsValidSQLWord(pStr: String): Boolean;
var
  lTryInt: Integer;
begin
  Result := not TryStrToInt(pStr, lTryInt) and not IsToken(pStr) and (pStr <> EmptyStr);
end;

procedure TSQLFormatter.SetCodeWordCharCase(const pCharCase: TCharCase);
begin
  FCodeWordCharCase := pCharCase;
end;

procedure TSQLFormatter.SetKeyWordCharCase(const pCharCase: TCharCase);
begin
  FKeyWordCharCase := pCharCase;
end;

function TSQLFormatter.SplitStringEx(pSQL, pDelimiters: String): TArray<String>;
var
  I: Integer;
  lStr: String;
  procedure AddResult(pStr: String);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := pStr;
  end;

  procedure ReadSQLInsideCommas;
  begin
    lStr := lStr + pSQL[I];
    Inc(I);
    while pSQL[I] <> cAspas do
    begin
      lStr := lStr + pSQL[I];
      Inc(I);
    end;
    lStr := lStr + pSQL[I];
    Inc(I);
    AddResult(lStr);
    lStr := EmptyStr;
    if I <= Length(pSQL) then
    begin
      AddResult(pSQL[I]);
    end;
  end;

begin
  SetLength(Result, 0);
  I := 1;
  while I <= Length(pSQL) do
  begin
    if pSQL[I] <> '' then
    begin
      if not ContainsText(pDelimiters, pSQL[I]) then
      begin
        if pSQL[I] = cAspas then
        begin
          ReadSQLInsideCommas;
        end else
        begin
          lStr := lStr + pSQL[I];
        end;
      end else
      begin
        AddResult(lStr);
        lStr := '';
        AddResult(pSQL[I]);
      end;
    end;
    Inc(I);
  end;
  if Trim(lStr) <> '' then
  begin
    AddResult(lStr);
  end;
end;

end.
