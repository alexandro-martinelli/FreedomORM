unit AM.Freedom.TextGenerator.FieldTextGenerator;

interface

uses
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TFieldTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string; override;
  end;

implementation

uses
  System.StrUtils,
  System.SysUtils;

{ TFieldTextGenerator }

{ TFieldTextGenerator }

function TFieldTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lField: TFieldArgument;
  lInt: Integer;
begin
  lField := TFieldArgument(pObject);
  Result := lField.Name;
  if not TryStrToInt(Result, lInt) then
  begin
    Result := ifthen(lField.TableAlias <> EmptyStr, lField.TableAlias + '.') + Result;
  end;
  if (lField.Alias <> EmptyStr) and Assigned(pParams) and pParams.WithAlias then
  begin
    Result := Result + ' as ' + lField.Alias;
  end;
end;

end.
