unit AM.Freedom.TextGenerator.BetweenArgumentTextGenerator;

interface

uses
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TBetweenArgumentTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String; override;
  end;

implementation

{ TBetweenArgumentTextGenerator }

function TBetweenArgumentTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lBetween: TBetweenArgument;
begin
  lBetween := TBetweenArgument(pObject);
  Result := GetTextFromGenerator(lBetween.BetweenArgument);
  Result := Result + ' and ' + GetTextFromGenerator(lBetween.AndArgument);
end;

end.
