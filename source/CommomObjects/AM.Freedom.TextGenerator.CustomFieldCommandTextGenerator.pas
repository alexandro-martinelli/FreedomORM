unit AM.Freedom.TextGenerator.CustomFieldCommandTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TCustomFieldCommandTextGenerator = class(TCustomTextGenerator)
  strict protected
    function GetFieldTypeAsString(pObject: TObject; pParams: TGenerateTextParams = nil): String; virtual;
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.EnumerationTypes;

{ TCustomFieldTextGenerator }

function TCustomFieldCommandTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lOptions, lFieldType: string;
  lParams: TGenerateTextParams;
begin
  Result := TCustomFieldCommand(pObject).Name;
  lFieldType := '';
  if not Assigned(pParams) or (pParams.ChangedProperties = []) or (DataType in pParams.ChangedProperties) then
  begin
    if TCustomFieldCommand(pObject).Domain = '' then
    begin
      lFieldType := GetFieldTypeAsString(pObject, pParams);
    end else
    begin
      lFieldType := TCustomFieldCommand(pObject).Domain;
    end;
    if Assigned(pParams) and (UseTypeOnDataTypeChanged in pParams.AlterFieldCommandParams) then
    begin
      lFieldType := 'type ' + lFieldType;
    end;
  end;
  if lFieldType <> '' then
  begin
    Result := Result +  ifthen(Result <> '', ' ') + lFieldType
  end;
  lOptions := '';
  lParams := nil;
  if Assigned(pParams) then
  begin
    lParams := pParams.CreateNew;
  end;
  lOptions := lOptions + ifthen(lOptions <> '', ' ') +
      GetTextFromGenerator(TCustomFieldCommand(pObject).FieldOptions, lParams);
  if TCustomFieldCommand(pObject).FieldOptions.Identity and not ContainsText(lOptions, 'identity') then
  begin
    lOptions := 'identity' + ifthen(lOptions <> '', ' ') + lOptions;
  end;
    if lOptions <> EmptyStr then
  begin
    Result := Result + ifthen(Result <> '', ' ') + lOptions;
  end;
  if GetSQLMapper <> nil then
  begin
    Result := GetSQLMapper.FormatFieldCommand(Result);
  end;
end;

function TCustomFieldCommandTextGenerator.GetFieldTypeAsString(pObject: TObject; pParams: TGenerateTextParams): String;
begin
  Result := GetSQLMapper.FieldClassTypeToString(pObject.ClassType);
end;

end.
