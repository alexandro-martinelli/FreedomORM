unit AM.Freedom.TextGenerator.DomainTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLCommands.DomainCommands;

type
  TDomainTextGenerator = class(TCustomTextGenerator)
  strict private
    function OnGetFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass;
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

implementation

{ TDomainTextGenerator }

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.SQLCommands.SizedFieldCommand,
  AM.Freedom.FieldCommandFactory, AM.Freedom.DefaultsClassRegister;

function TDomainTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lCreate: TCreateDomainCommand;
  lField: TCustomFieldCommand;
  lParams: TCreateFieldCommandParams;
begin
  Result := '';
  if GetSQLMapper <> nil then
  begin
    case TCustomDomainCommand(pObject).CommandType of
      TCommandType.Create:
        begin
          lCreate := TCreateDomainCommand(pObject);
          lParams := TCreateFieldCommandParams.Create;
          try
            lParams.ColumnType := lCreate.ColumnType;
            lParams.Size := lCreate.Size;
            lParams.Scale := lCreate.Scale;
            lParams.OnGetFieldCommandClass := OnGetFieldCommandClass;
            lParams.SQLMapper := TDefaultsClassRegister.DefaultSQLMapper;
            lField := TFieldCommandFactory.CreateFieldCommandFromColumnType(lParams);
          finally
            lParams.Free;
          end;
          try
            Result := Trim(Format(GetSQLMapper.GetSQLForCreateDomain, [lCreate.Name, GetTextFromGenerator(lField),
                ifthen(lCreate.DomainOptions.Nullable = nNotNull, 'not null')]));
          finally
            lField.Free;
          end;
        end;
      TCommandType.Drop:
        Result := Format(GetSQLMapper.GetSQLForDropDomain, [TCustomDomainCommand(pObject).Name]);
    end;
  end;
end;

function TDomainTextGenerator.OnGetFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass;
begin
  Result := GetSQLMapper.ColumnTypeToFieldCommandClass(pColumnType);
end;

end.
