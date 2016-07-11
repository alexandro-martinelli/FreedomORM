unit AM.Freedom.TextGenerator.CustomExpressionTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TExpressionTextGeneratorClass = class of TCustomExpressionTextGenerator;

  TCustomExpressionTextGenerator = class abstract(TCustomTextGenerator)
  strict protected
    function GetExpressionName: String; virtual; abstract;
    function GetTextParams: TGenerateTextParams; virtual;
    function GetTextArgument(pObject: TObject): String; virtual;
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String; override;
  end;

implementation

uses
  AM.Freedom.SQLMappers.CustomExpression;

{ TCustomExpressionTextGenerator }

function TCustomExpressionTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := GetExpressionName + '(' + GetTextArgument(pObject) + ')';
  if Assigned(pParams) and pParams.WithAlias and (TCustomExpression(pObject).Alias <> '') then
  begin
    Result := Result + ' as ' + TCustomExpression(pObject).Alias;
  end;
end;

function TCustomExpressionTextGenerator.GetTextArgument(pObject: TObject): String;
begin
  Result := GetTextFromGenerator(TCustomExpression(pObject).Argument, GetTextParams);
end;

function TCustomExpressionTextGenerator.GetTextParams: TGenerateTextParams;
begin
  Result := TGenerateTextParams.Create(False, True);
end;

end.
