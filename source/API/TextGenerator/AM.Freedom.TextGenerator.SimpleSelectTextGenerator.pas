unit AM.Freedom.TextGenerator.SimpleSelectTextGenerator;

interface

uses
  System.Generics.Collections,
  AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.TextGenerator.CustomTextGenerator;

type
  TSimpleSelectTextGenerator = class(TCustomTextGenerator)
  strict private
    FHasWhere: Boolean;
    FSelect: String;
    FOrder: string;
    procedure Initialize(pSQL: String);
    procedure IncludeLimitRows(var pSQL: String; const pLimitRows: Integer);
    procedure FormatSQLText(var pSQL: String);
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String; override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.GroupCriteria,
  AM.Freedom.SQLGenerator.SimpleSQLGenerator,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Helper.Policy;

{ TSimpleSelectTextGenerator }

function TSimpleSelectTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lSQLCriteria, lSQL: string;
  lGroupCriterias: TList<TGroupCriteria>;
  lWherePolicy: TPolicy;
  lGroupCriteria: TGroupCriteria;
begin
  lGroupCriterias := TSimpleSQLGenerator(pObject).GroupCriterias;
  Initialize(TSimpleSQLGenerator(pObject).SQL);
  lWherePolicy := TSimpleSQLGenerator(pObject).WherePolicy;
  lSQLCriteria := '';
  for lGroupCriteria in lGroupCriterias do
  begin
    lSQL := GetTextFromGenerator(lGroupCriteria);
    lSQLCriteria := lSQLCriteria + ifthen(((lSQLCriteria <> '') or FHasWhere) and (lSQL <> ''), lWherePolicy.ToString) + lSQL;
  end;
  lGroupCriteria := nil;
  if lGroupCriterias.Count > 0 then
  begin
    lGroupCriteria := lGroupCriterias.Items[0];
  end;
  Result := FSelect;
  if lSQLCriteria <> EmptyStr then
  begin
    if not FHasWhere then
    begin
      Result := Result + ' WHERE ';
    end;
//    else
//    begin
//      Result := Result + lWherePolicy.ToString;
//    end;
    Result := Result + lSQLCriteria;
  end;
  if FOrder <> EmptyStr then
  begin
    Result := Result + ' ' + FOrder;
  end;
  if Assigned(lGroupCriteria) and (lGroupCriteria.LimitRows > 0) then
  begin
    IncludeLimitRows(Result, lGroupCriteria.LimitRows);
  end;
  FormatSQLText(Result);
end;

procedure TSimpleSelectTextGenerator.Initialize(pSQL: String);
var
  lPos, lSelectCount, lLastPos, lFinalPos: Integer;
  lSQl: string;
  procedure DefineFinalPos(pWord: String);
  begin
    lFinalPos := lLastPos;
    lFinalPos := PosEx(pWord, lSQl, lLastPos + Length(pWord));
    if lFinalPos > 0 then
    begin
      lLastPos := lFinalPos;
    end;
    while lFinalPos > 0 do
    begin
      lFinalPos := PosEx(pWord, lSQl, lFinalPos + Length(pWord));
      if lFinalPos > 0 then
      begin
        lLastPos := lFinalPos;
      end;
    end;
  end;
begin
  lSQl := AnsiUpperCase(pSQL);
  lPos := PosEx('SELECT', lSQl, 1) + 7;
  lSelectCount := 1;
  while PosEx('SELECT', lSQl, lPos) > 0 do
  begin
    lPos := PosEx('SELECT', lSQl, lPos) + 7;
    Inc(lSelectCount);
  end;
  lPos := 1;
  while lSelectCount > 0 do
  begin
    lPos := PosEx('FROM', lSQl, lPos) + 4;
    Dec(lSelectCount);
  end;
  FHasWhere := PosEx('WHERE', lSQl, lPos) > 0;
  lLastPos := PosEx('GROUP', lSQl, lPos);
  if lLastPos <= 0 then
  begin
    lLastPos := PosEx('ORDER', lSQl, lPos);
    if lLastPos > 0 then
    begin
      DefineFinalPos('ORDER');
      Dec(lLastPos, 2)
    end;
  end else
  begin
    lLastPos := PosEx('GROUP', lSQl, lPos);
    if lLastPos > 0 then
    begin
      DefineFinalPos('GROUP');
      Dec(lLastPos, 2)
    end;
  end;
  if lLastPos <= 0 then
  begin
    lLastPos := Length(lSQl);
  end;
  FSelect := Copy(pSQL, 1, lLastPos);
  Delete(pSQL, 1, lLastPos + 1);
  FOrder := pSQL;
end;

procedure TSimpleSelectTextGenerator.IncludeLimitRows(var pSQL: String; const pLimitRows: Integer);
begin
  if GetSQLMapper <> nil then
  begin
    GetSQLMapper.IncludeLimitRows(pSQL, pLimitRows);
  end;
end;

procedure TSimpleSelectTextGenerator.FormatSQLText(var pSQL: String);
begin
  if GetSQLMapper <> nil then
  begin
    GetSQLMapper.FormatSQLText(pSQL);
  end;
end;

end.
