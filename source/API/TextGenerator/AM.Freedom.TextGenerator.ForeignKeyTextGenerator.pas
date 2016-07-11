unit AM.Freedom.TextGenerator.ForeignKeyTextGenerator;

interface

uses
  System.Generics.Collections,
  AM.Freedom.TextGenerator.CustomConstraintTextGenerator,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Helper.ForeignOption,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TForeignKeyTextGenerator = class(TCustomConstraintTextGenerator)
  strict private
    function GetReferencesFields(pReferenceFields: TList<String>): String;
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.SQLCommands.Constraints;

{ TForeignKeyTextGenerator }

function TForeignKeyTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lConstraint: TForeignKey;
begin
  lConstraint := TForeignKey(pObject);
  Result := inherited;
  Result := Result + ' references ' + lConstraint.References + ' (' +
      GetReferencesFields(lConstraint.ReferenceFields) + ')';
  if lConstraint.OnUpdate <> TForeignOption.NoAction then
  begin
    Result := Result + ' on update ' + lConstraint.OnUpdate.ToString;
  end;
  if lConstraint.OnDelete <> TForeignOption.NoAction then
  begin
    Result := Result + ' on delete ' + lConstraint.OnDelete.ToString;
  end;
end;

function TForeignKeyTextGenerator.GetReferencesFields(pReferenceFields: TList<String>): String;
var
  lField: String;
begin
  Result := EmptyStr;
  for lField in pReferenceFields do
  begin
    Result := Result + ifthen(result <> EmptyStr, ', ') + lField;
  end;
end;

end.
