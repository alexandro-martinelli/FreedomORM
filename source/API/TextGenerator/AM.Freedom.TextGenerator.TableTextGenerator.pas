unit AM.Freedom.TextGenerator.TableTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TTableTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String; override;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.SQLMappers.Arguments;

{ TTableTextGenerator }

function TTableTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lTable: TTableArgument;
begin
  lTable := TTableArgument(pObject);
  Result := lTable.Name;
  if Assigned(pParams) and pParams.WithAlias and (lTable.Alias <> EmptyStr) then
  begin
    Result := Result + ' ' + lTable.Alias;
  end;
end;

end.
