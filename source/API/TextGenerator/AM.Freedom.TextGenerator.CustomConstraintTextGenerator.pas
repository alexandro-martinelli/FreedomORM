unit AM.Freedom.TextGenerator.CustomConstraintTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.SQLCommands.Constraints, AM.Freedom.Helper.ConstraintType;

type
  TCustomConstraintTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils;

{ TCustomConstraintTextGenerator }

function TCustomConstraintTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lConstraint: TCustomConstraint;
  lField: String;
begin
  lConstraint := TCustomConstraint(pObject);
  Result := EmptyStr;
  for lField in lConstraint.Fields do
  begin
    Result := Result + ifthen(Result <> EmptyStr, ', ') + lField;
  end;
  Result := 'constraint ' + lConstraint.Name + ' ' + lConstraint.ConstraintType.ToString + ' (' + Result + ')';
end;

end.
