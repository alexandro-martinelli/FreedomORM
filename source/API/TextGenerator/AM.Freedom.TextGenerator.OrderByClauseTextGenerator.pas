unit AM.Freedom.TextGenerator.OrderByClauseTextGenerator;

interface

uses
  AM.Freedom.SQLMappers.OrderByClause,
  AM.Freedom.TextGenerator.CustomClauseTextGenerator;

type
  TOrderByClauseTextGenerator = class(TCustomClauseTextGenerator)
  strict private
    function GenerateTextForOrderItem(pOrderItem: TOrderBy): String;
  strict protected
    function GetClauseName: string; override;
    function GetTextArgument(pObject: TObject): string; override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.TextGenerator.GenerateTextParams;

{ TOrderByClauseTextGenerator }

function TOrderByClauseTextGenerator.GenerateTextForOrderItem(pOrderItem: TOrderBy): String;
var
  lDirective: string;
begin
  Result := GetTextFromGenerator(pOrderItem.Field, TGenerateTextParams.Create(False, True));
  if Assigned(pOrderItem.Directive) then
  begin
    lDirective := GetTextFromGenerator(pOrderItem.Directive);
  end else
  begin
    lDirective := EmptyStr;
  end;
  if (lDirective <> EmptyStr) then
  begin
    Result := Result + ' ' + lDirective;
  end else
  begin
    Result := Result + ifthen(pOrderItem.Order = TOrderType.Desc, ' DESC');
  end;
end;

function TOrderByClauseTextGenerator.GetClauseName: string;
begin
  Result := 'ORDER BY';
end;

function TOrderByClauseTextGenerator.GetTextArgument(pObject: TObject): string;
var
  lOrder: TOrderByClause;
  lItem: TOrderBy;
begin
  lOrder := TOrderByClause(pObject);
  Result := EmptyStr;
  for lItem in lOrder do
  begin
    Result := Result + ifthen(Result <> EmptyStr, ', ') + GenerateTextForOrderItem(lItem);
  end;
end;

end.
