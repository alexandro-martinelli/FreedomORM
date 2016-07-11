unit AM.Freedom.TextGenerator.InArgumentTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TInArgumentTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String; override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLMapper.CustomArgument;

{ TInArgumentTextGenerator }

function TInArgumentTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lInArgument: TInArgument;
  lItem: TCustomArgument;
begin
  lInArgument := TInArgument(pObject);
  for lItem in lInArgument.InArguments do
  begin
    Result := Result + ifthen(Result <> EmptyStr, ', ') + GetTextFromGenerator(lItem);
  end;
end;

end.
