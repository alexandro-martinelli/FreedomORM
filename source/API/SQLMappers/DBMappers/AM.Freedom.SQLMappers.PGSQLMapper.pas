unit AM.Freedom.SQLMappers.PGSQLMapper;

interface

uses
  System.Generics.Collections,
  System.Classes,
  AM.Freedom.SQLMappers.CustomSQLMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLMappers.SQLFormatSettings,
  AM.Freedom.SQLMapper.CustomArgument;

type
  TPGSQLMapper = class(TCustomSQLMapper)
  strict protected
    function GetKeyWords: TStrings; override;
// function DescriptionText(pDescriptionTextType: TDescriptionTextType): string; override;
    procedure IncludeLimitRows(var pSQL: string; const pLimitRows: Integer); override;
    procedure IncludeReturningClause(var pSQLText: string; const pFieldNames: TList<string>); override;
    function AdjustNameLength(pName: string): string; override;
    function DoGetFieldClassTypeToString(pClassType: TClass): string; override;
    function FormatFieldCommand(pFieldCommandText: string): string; override;
    function AlterFieldCommandParams: TAlterFieldCommandParams; override;
    function DoGetColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass; override;
    function SequenceSQL(pSequenceName: string; pSchemaName: string; pRestartWith: Integer; pCommandType: TCommandType): string; override;
    procedure WriteSQLFormatSettings(pSQLFormatSettings: TSQLFormatSettings); override;
    procedure AdjustIndexCommand(lIndexCommand: TCustomCommand); override;
    function GenerateDropIndexCommand(lIndexCommand: TCustomCommand): string; override;
    function DoGetCurrentDateExpression: String; override;
    function DoGetCurrentDateTimeExpression: String; override;
    function DoGetCurrentTimeExpression: String; override;
    function BooleanToDefaultExpression(pBooleanValue: Boolean): Variant; override;
    function FixIntColumnTypeForDDLColumn(pColumnType: TColumnType): TColumnType; override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.SQLCommands.Fields,
  AM.Freedom.SQLCommands.IndexCommands;

{ TPGSQLMapper }

procedure TPGSQLMapper.AdjustIndexCommand(lIndexCommand: TCustomCommand);
var
  lCreateIndex: TCreateIndexCommand;
begin
  if (lIndexCommand.InheritsFrom(TCreateIndexCommand)) then
  begin
    lCreateIndex := TCreateIndexCommand(lIndexCommand);
    if lCreateIndex.Option = ioClustered then
    begin
      lCreateIndex.Option := ioNone;
    end;
  end;
end;

function TPGSQLMapper.AdjustNameLength(pName: string): string;
begin
  Result := Copy(pName, 1, 64);
end;

function TPGSQLMapper.AlterFieldCommandParams: TAlterFieldCommandParams;
begin
  Result := [UseSetOnNullabeChanged, UseTypeOnDataTypeChanged];
end;

function TPGSQLMapper.BooleanToDefaultExpression(pBooleanValue: Boolean): Variant;
begin
  Result := ifthen(pBooleanValue, 't', 'f');
end;

function TPGSQLMapper.DoGetColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass;
begin
  case pColumnType of
    ctyByte:
      Result := TSmallintFieldCommand;
    ctySingle:
      Result := TFloatFieldCommand;
    ctyDouble, ctyCurrency, ctyExtended:
      Result := TDoubleFieldCommand;
    ctyXML:
      Result := TXMLFieldCommand;
    else
      Result := nil;
  end;
end;

function TPGSQLMapper.DoGetCurrentDateExpression: String;
begin
  Result := 'CURRENT_DATE';
end;

function TPGSQLMapper.DoGetCurrentDateTimeExpression: String;
begin
  Result := 'CURRENT_TIMESTAMP';
end;

function TPGSQLMapper.DoGetCurrentTimeExpression: String;
begin
  Result := 'CURRENT_TIME';
end;

function TPGSQLMapper.DoGetFieldClassTypeToString(pClassType: TClass): string;
begin
  if pClassType = TFloatFieldCommand then
  begin
    Result := 'real';
  end
  else if pClassType = TDoubleFieldCommand then
  begin
    Result := 'double precision';
  end
  else if pClassType = TBlobFieldCommand then
  begin
    Result := 'bytea'
  end
  else if pClassType = TMemoFieldCommand then
  begin
    Result := 'text'
  end
  else if pClassType = TXMLFieldCommand then
  begin
    Result := 'xml';
  end
  else if pClassType = TDateTimeFieldCommand then
  begin
    Result := 'timestamp';
  end
  else if pClassType = TBooleanFieldCommand then
  begin
    Result := 'boolean';
  end;
end;

function TPGSQLMapper.FixIntColumnTypeForDDLColumn(pColumnType: TColumnType): TColumnType;
begin
  Result := pColumnType;
  if Result = ctyByte then
  begin
    Result := ctySmallint;
  end;
end;

function TPGSQLMapper.FormatFieldCommand(pFieldCommandText: string): string;
begin
  Result := inherited FormatFieldCommand(pFieldCommandText);
  if ContainsText(pFieldCommandText, 'bigint identity') then
  begin
    Result := ReplaceText(pFieldCommandText, 'bigint identity', 'bigserial');
  end
  else if ContainsText(pFieldCommandText, 'integer identity') then
  begin
    Result := ReplaceText(pFieldCommandText, 'integer identity', 'serial');
  end
  else if ContainsText(pFieldCommandText, 'smallint identity') then
  begin
    Result := ReplaceText(pFieldCommandText, 'smallint identity', 'smallserial');
  end
  else if ContainsText(pFieldCommandText, 'identity') then
  begin
    Result := ReplaceText(pFieldCommandText, 'identity', '');
  end;
  Result := Trim(Result);
end;

function TPGSQLMapper.GenerateDropIndexCommand(lIndexCommand: TCustomCommand): string;
var
  lDropIndex: TDropIndexCommand;
begin
  Result := '';
  if (lIndexCommand.InheritsFrom(TDropIndexCommand)) then
  begin
    lDropIndex := TDropIndexCommand(lIndexCommand);
    Result := Format('drop index %s%s', [ifthen(lDropIndex.Schema <> '', lDropIndex.Schema + '.'), lDropIndex.Name]);
  end;
end;

function TPGSQLMapper.GetKeyWords: TStrings;
begin
  Result := inherited GetKeyWords;
  Result.DelimitedText := Result.DelimitedText + ';RESTART;WITH;INCREMENT;MINVALUE;NO;MAXVALUE;START;CACHE;CYCLE;' +
      'OWNED;NONE;CURRVAL;LASTVAL;NEXTVAL;SETVAL;LIMIT;REAL;SERIAL;BIGSERIAL;SMALLSERIAL;NULLS;FIRST;CONCURRENTLY';
end;

procedure TPGSQLMapper.IncludeLimitRows(var pSQL: string; const pLimitRows: Integer);
begin
  if Pos('SELECT', UpperCase(pSQL)) > 0 then
  begin
    pSQL := pSQL + ' limit ' + IntToStr(pLimitRows);
  end;
end;

procedure TPGSQLMapper.IncludeReturningClause(var pSQLText: string; const pFieldNames: TList<string>);
var
  lReturnField: string;
  lReturningFields: string;
begin
  lReturningFields := '';
  for lReturnField in pFieldNames do
  begin
    lReturningFields := lReturningFields + ifthen(lReturningFields <> '', ', ') + lReturnField;
  end;
  if lReturningFields <> '' then
  begin
    pSQLText := pSQLText + ' returning ' + lReturningFields;
  end;
end;

function TPGSQLMapper.SequenceSQL(pSequenceName: string; pSchemaName: string; pRestartWith: Integer; pCommandType: TCommandType): string;
  function CompleteSequenceName: string;
  begin
    Result := ifthen(pSchemaName <> '', pSchemaName + '.') + pSequenceName;
  end;
begin
  case pCommandType of
    Select:
      Result := Format('nextval(%s)', [QuotedStr(CompleteSequenceName)]);
  else
    Result := inherited;
  end;
end;

procedure TPGSQLMapper.WriteSQLFormatSettings(pSQLFormatSettings: TSQLFormatSettings);
begin
  pSQLFormatSettings.DateFormat := 'MM/dd/yyyy';
  inherited;
end;

end.
