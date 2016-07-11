unit AM.Freedom.TextGenerator.CustomSelectTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.TextGenerator.CustomClauseTextGenerator;

type
  TCustomSelectTextGenerator = class abstract(TCustomClauseTextGenerator)
  strict protected
    procedure IncludeLimitRows(var pSQL: String; const pLimitRows: Integer);
    procedure FormatSQLText(var pSQL: String);
    function GetClauseName: string; override;
    function GetTextArgument(pObject: TObject): string; override;
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String; override;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  System.StrUtils,
  AM.Freedom.Helper.Policy,
  AM.Freedom.SQLMapper.CustomArgument, AM.Freedom.SQLMappers.CustomSelect;

{ TCustomSQLGenerator }

procedure TCustomSelectTextGenerator.FormatSQLText(var pSQL: String);
begin
  if GetSQLMapper <> nil then
  begin
    GetSQLMapper.FormatSQLText(pSQL);
  end;
end;

function TCustomSelectTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lSelect: TCustomSelect;
begin
  lSelect := TCustomSelect(pObject);
  if Assigned(pParams) and pParams.BetweenParentesis then
  begin
    Result := '(';
  end;
  Result := Result + inherited;
  if Assigned(pParams) and pParams.BetweenParentesis then
  begin
    Result := Result + ')';
  end;
  if Assigned(pParams) and pParams.WithAlias and (lSelect.Alias <> EmptyStr) then
  begin
    Result := Result + ' as ' + lSelect.Alias;
  end;
  if lSelect.WhereClause.LimitRows > 0 then
  begin
    IncludeLimitRows(Result, lSelect.WhereClause.LimitRows);
  end;
  FormatSQLText(Result);
end;

function TCustomSelectTextGenerator.GetClauseName: string;
begin
  Result := 'SELECT';
end;

function TCustomSelectTextGenerator.GetTextArgument(pObject: TObject): string;
var
  lSelect: TCustomSelect;
  lArgument: TCustomArgument;
  lTextClause: String;
begin
  lSelect := TCustomSelect(pObject);
  for lArgument in lSelect.FieldList do
  begin
    Result := Result + ifthen(Result <> EmptyStr, ', ') + GetTextFromGenerator(lArgument, TGenerateTextParams.Create(True, True));
  end;
  lTextClause := GetTextFromGenerator(lSelect.FromClause);
  if lTextClause <> EmptyStr then
  begin
    Result := Result + ' ' + lTextClause;
  end;
  lTextClause := GetTextFromGenerator(lSelect.JoinClause);
  if lTextClause <> EmptyStr then
  begin
    Result := Result + ' ' + lTextClause;
  end;
  lTextClause := GetTextFromGenerator(lSelect.WhereClause, TGenerateTextParams.Create(True, True));
  if lTextClause <> EmptyStr then
  begin
    Result := Result + ' where ' + lTextClause;
  end;
  lTextClause := GetTextFromGenerator(lSelect.GroupByClause, TGenerateTextParams.Create(True, True));
  if lTextClause <> EmptyStr then
  begin
    Result := Result + ' ' + lTextClause;
  end;
  lTextClause := GetTextFromGenerator(lSelect.HavingClause, TGenerateTextParams.Create(True, True));
  if lTextClause <> EmptyStr then
  begin
    Result := Result + ' ' + lTextClause;
  end;
  lTextClause := GetTextFromGenerator(lSelect.OrderByClause, TGenerateTextParams.Create(True, True));
  if lTextClause <> EmptyStr then
  begin
    Result := Result + ' ' + lTextClause;
  end;
end;

procedure TCustomSelectTextGenerator.IncludeLimitRows(var pSQL: String; const pLimitRows: Integer);
begin
  if GetSQLMapper <> nil then
  begin
    GetSQLMapper.IncludeLimitRows(pSQL, pLimitRows);
  end;
end;

end.
