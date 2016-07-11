unit AM.Freedom.TextGenerator.GroupByClauseTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomClauseTextGenerator, AM.Freedom.TextGenerator.GenerateTextParams;

type
  TGroupByClauseTextGenerator = class(TCustomClauseTextGenerator)
  strict protected
    function GetClauseName: string; override;
    function GetTextArgument(pObject: TObject): string; override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.SQLMappers.GroupByClause,
  AM.Freedom.SQLMappers.Arguments, AM.Freedom.SQLMapper.CustomArgument;

{ TGroupByClauseTextGenerator }

function TGroupByClauseTextGenerator.GetClauseName: string;
begin
  Result := 'GROUP BY'
end;

function TGroupByClauseTextGenerator.GetTextArgument(pObject: TObject): string;
var
  lGroup: TGroupByClause;
  lField: TCustomArgument;
begin
  Result := EmptyStr;
  lGroup := TGroupByClause(pObject);
  for lField in lGroup do
  begin
    Result := Result + ifThen(Result <> EmptyStr, ', ') + GetTextFromGenerator(lField, TGenerateTextParams.Create(False, True));
  end;
end;

end.
