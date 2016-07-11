unit AM.Freedom.TextGenerator.CustomTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.TextGenerator.ITextGenerator,
  AM.Freedom.TextGenerator.AbstractTextGenerator, AM.Freedom.Exceptions;

type
  TTextGeneratorClass = class of TCustomTextGenerator;

  TCustomTextGenerator = class abstract(TAbstractTextGenerator, ITextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String; virtual;
  public
    function GenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string;
  end;

implementation

{ TCustomTextGenerator }

function TCustomTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): String;
begin
  raise EInvalidMethodCallOnClass.Create('DoGenerateText', ClassName);
end;

function TCustomTextGenerator.GenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := DoGenerateText(pObject, pParams);
  if Result <> '' then
  begin
    FormatSQLText(Result);
  end;
end;

end.
