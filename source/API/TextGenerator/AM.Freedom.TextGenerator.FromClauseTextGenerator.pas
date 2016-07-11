unit AM.Freedom.TextGenerator.FromClauseTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomClauseTextGenerator;

type
  TFromClauseTextGenerator = class(TCustomClauseTextGenerator)
  strict protected
    function GetClauseName: string; override;
    function GetTextArgument(pObject: TObject): string; override;
  end;

implementation

uses
  AM.Freedom.SQLMappers.FromClause,
  AM.Freedom.TextGenerator.GenerateTextParams;

{ TFromTextGenerator }

function TFromClauseTextGenerator.GetClauseName: string;
begin
  Result := 'FROM';
end;

function TFromClauseTextGenerator.GetTextArgument(pObject: TObject): string;
begin
  Result := GetTextFromGenerator(TFromClause(pObject).Argument, TGenerateTextParams.Create(True, True));
  if (GetSQLMapper <> nil) then
  begin
    Result := GetSQLMapper.IncludeDirectives(Result);
  end;
end;

end.
