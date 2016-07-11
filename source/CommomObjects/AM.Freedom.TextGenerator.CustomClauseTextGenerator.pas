unit AM.Freedom.TextGenerator.CustomClauseTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TCustomClauseTextGenerator = class abstract(TCustomTextGenerator)
  strict protected
    function GetClauseName: String; virtual; abstract;
    function GetTextArgument(pObject: TObject): String; virtual; abstract;
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String; override;
  end;

implementation

uses
  System.SysUtils;

{ TCustomClauseTextGenerator }

function TCustomClauseTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := GetTextArgument(pObject);
  if Result <> EmptyStr then
  begin
    Result := GetClauseName + ' ' + Result;
  end;
end;

end.
