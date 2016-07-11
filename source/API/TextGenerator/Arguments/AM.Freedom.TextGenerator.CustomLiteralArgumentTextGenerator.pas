unit AM.Freedom.TextGenerator.CustomLiteralArgumentTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TCustomLiteralArgumentTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String; override;
  end;

implementation

uses
  AM.Freedom.SQLMappers.Arguments;

{ TLiteralArgumentTextGenerator }

function TCustomLiteralArgumentTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := TLiteralArgument(pObject).LiteralValue;
end;

end.
