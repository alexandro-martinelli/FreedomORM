unit AM.Freedom.TextGenerator.CustomFieldOptionsTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLMapper.CustomArgument;

type
  TCustomFieldOptionsTextGenerator = class(TCustomTextGenerator)
  strict protected
    function GenerateTextForSimpleOptions(pObject: TObject; pParams: TGenerateTextParams = nil): String; virtual;
    function DoBeforeGenerateTextForSimpleOptions(pObject: TObject; pParams: TGenerateTextParams): String; virtual;
    function GenerateTextForCheck(pCheck: TCustomArgument; pParams: TGenerateTextParams = nil): String; virtual;
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TFieldOptionsTextGenerator = class(TCustomFieldOptionsTextGenerator)
  strict protected
    function DoBeforeGenerateTextForSimpleOptions(pObject: TObject; pParams: TGenerateTextParams): String; override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.SQLCommands.FieldOptions;

{ TCustomFieldOptionsTextGenerator }

function TCustomFieldOptionsTextGenerator.DoBeforeGenerateTextForSimpleOptions(pObject: TObject;
  pParams: TGenerateTextParams): String;
begin
  // do when necessary
end;

function TCustomFieldOptionsTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := GenerateTextForSimpleOptions(pObject, pParams);
end;

function TCustomFieldOptionsTextGenerator.GenerateTextForCheck(pCheck: TCustomArgument; pParams: TGenerateTextParams): String;
begin
  Result := 'check (' + GetTextFromGenerator(pCheck) + ')';
end;

function TCustomFieldOptionsTextGenerator.GenerateTextForSimpleOptions(pObject: TObject; pParams: TGenerateTextParams): String;
var
  lOptions: TCustomFieldOptions;
  lNotNull: string;
begin
  lOptions := TCustomFieldOptions(pObject);
  lNotNull := '';
  Result := DoBeforeGenerateTextForSimpleOptions(pObject, pParams);
  if not Assigned(pParams) or (pParams.ChangedProperties = []) or (Nullable in pParams.ChangedProperties) then
  begin
    if lOptions.Nullable = nNotNull then
    begin
      lNotNull := 'not null';
    end;
    if Assigned(pParams) and not (DontChangeNullable in pParams.AlterFieldCommandParams) and
       (UseSetOnNullabeChanged in pParams.AlterFieldCommandParams) then
    begin
      case lOptions.Nullable of
        nNull: lNotNull := 'drop not null';
        nNotNull: lNotNull := 'set not null';
        else
          lNotNull := '';
      end;
    end;
    if lNotNull <> '' then
    begin
      Result := Result + ifthen(Result <> EmptyStr, ' ') + lNotNull;
    end;
  end;
  if Assigned(lOptions.Default) then
  begin
    Result := Result + ifthen(Result <> EmptyStr, ' ') + 'default ' + GetTextFromGenerator(lOptions.Default, TGenerateTextParams.Create(False, True));
  end;
end;

{ TFieldOptionsTextGenerator }

function TFieldOptionsTextGenerator.DoBeforeGenerateTextForSimpleOptions(pObject: TObject; pParams: TGenerateTextParams): String;
begin
  if TFieldOptions(pObject).Identity then
  begin
    if not Assigned(pParams) or (pParams.ChangedProperties <> []) then
    begin
      Result := 'identity';
    end;
  end;

end;

end.


