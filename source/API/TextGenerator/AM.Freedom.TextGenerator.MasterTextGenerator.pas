unit AM.Freedom.TextGenerator.MasterTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TMasterTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  public
    class function ExtractText(pObject: TObject; pParams: TGenerateTextParams = nil): String;
  end;

implementation

{ TMasterTextGenerator }

function TMasterTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := GetTextFromGenerator(pObject, pParams);
end;

class function TMasterTextGenerator.ExtractText(pObject: TObject; pParams: TGenerateTextParams): String;
var
  lMasterTextGenerator: TMasterTextGenerator;
begin
  lMasterTextGenerator := TMasterTextGenerator.Create;
  try
    Result := lMasterTextGenerator.GenerateText(pObject, pParams);
  finally
    lMasterTextGenerator.Free;
  end;
end;

end.
