unit AM.Freedom.TextGenerator.JoinClauseTextGenerator;

interface

uses
  AM.Freedom.SQLMappers.JoinClause,
  AM.Freedom.TextGenerator.CustomClauseTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TJoinClauseTextGenerator = class(TCustomClauseTextGenerator)
  strict protected
    function GetTextForJoinItem(pJoin: TJoin): String;
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String; override;
  end;


implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.Helper.JoinKind;

{ TJoinClauseTextGenerator }

function TJoinClauseTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lJoin: TJoin;
  lJoinClause: TJoinClause;
begin
  lJoinClause := TJoinClause(pObject);
  for lJoin in lJoinClause do
  begin
    Result := Result + ifthen(Result <> EmptyStr, ' ') + GetTextForJoinItem(lJoin);
  end;
end;

function TJoinClauseTextGenerator.GetTextForJoinItem(pJoin: TJoin): String;
begin
  Result := pJoin.Kind.ToSQL + ' ' + GetTextFromGenerator(pJoin.Argument, TGenerateTextParams.Create(True, True)) +
      ' on ' + GetTextFromGenerator(pJoin.JoinOn);
end;

end.
