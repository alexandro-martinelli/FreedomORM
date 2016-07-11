unit AM.Freedom.TextGenerator.TableArgumentTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TTableArgumentTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String; override;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.SQLMappers.Arguments;

{ TTableTextGenerator }

function TTableArgumentTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lTable: TTableArgument;
begin
  lTable := TTableArgument(pObject);
  Result := lTable.Name;
  if Assigned(pParams) and pParams.WithAlias and (lTable.Alias <> '') then
  begin
    Result := Result + ' ' + lTable.Alias;
  end;
  if (lTable.Schema <> '') then
  begin
    Result := lTable.Schema + '.' + Result;
  end;
end;

end.
