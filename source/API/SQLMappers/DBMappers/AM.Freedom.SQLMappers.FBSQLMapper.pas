unit AM.Freedom.SQLMappers.FBSQLMapper;

interface

uses
  System.Classes,
  System.Generics.Collections,
  AM.Freedom.SQLMappers.CustomSQLMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLCommands.FBFields,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLMappers.SQLFormatSettings,
  AM.Freedom.SQLMapper.CustomArgument;

type
  TFBSQLMapper = class(TCustomSQLMapper)
  strict protected
    function DoGetFieldClassTypeToString(pClassType: TClass): string; override;
    function GetKeyWords: TStrings; override;
    function DescriptionText(pDescriptionTextType: TDescriptionTextType): string; override;
    procedure IncludeLimitRows(var pSQL: string; const pLimitRows: Integer); override;
    procedure IncludeReturningClause(var pSQLText: string; const pFieldNames: TList<string>); override;
    function GetColumnTypeFromEnumType(pEnumType: TEnumerationType): TColumnType; override;
    function AdjustNameLength(pName: string): string; override;
    function AlterFieldCommandParams: TAlterFieldCommandParams; override;
    function DoGetColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass; override;
    function SequenceSQL(pSequenceName: string; pSchemaName: string; pRestartWith: Integer; pCommandType: TCommandType): string; override;
    procedure WriteSQLFormatSettings(pSQLFormatSettings: TSQLFormatSettings); override;
    procedure AdjustIndexCommand(lIndexCommand: TCustomCommand); override;
    function GenerateDropIndexCommand(lIndexCommand: TCustomCommand): string; override;
    function TableFieldCommandToString(pCommandType: TCommandType): string; override;
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
  AM.Freedom.SQLCommands.Fields, AM.Freedom.SQLCommands.IndexCommands,
  AM.Freedom.IndexColumn, AM.Freedom.Helper.CommandType, AM.Freedom.Exceptions;
{ TFBSQLMapper }

procedure TFBSQLMapper.AdjustIndexCommand(lIndexCommand: TCustomCommand);
var
  lCreateIndex: TCreateIndexCommand;
  lColumn: TIndexColumn;
begin
  if (lIndexCommand.InheritsFrom(TCreateIndexCommand)) then
  begin
    lCreateIndex := TCreateIndexCommand(lIndexCommand);
    if lCreateIndex.Option <> ioDescending then
    begin
      lCreateIndex.Option := ioNone;
    end;
    if (lCreateIndex.Schema <> '') then
    begin
      lCreateIndex.Schema := '';
    end;
    for lColumn in lCreateIndex.Columns do
    begin
      if (lColumn.SortType <> Asc) then
      begin
        lColumn.SortType := Asc;
      end;
      if (lColumn.NullOption <> noNone) then
      begin
        lColumn.NullOption := noNone;
      end;
    end;
  end;
end;

function TFBSQLMapper.AdjustNameLength(pName: string): string;
begin
  Result := Copy(pName, 1, 31);
end;

function TFBSQLMapper.AlterFieldCommandParams: TAlterFieldCommandParams;
begin
  Result := [DontChangeNullable, UseTypeOnDataTypeChanged];
end;

function TFBSQLMapper.BooleanToDefaultExpression(pBooleanValue: Boolean): Variant;
begin
  Result := '';
end;

function TFBSQLMapper.DescriptionText(pDescriptionTextType: TDescriptionTextType): string;
begin
  case pDescriptionTextType of
    dttField:
      Result := 'comment on column %s IS %s';
    dttTable:
      Result := 'comment on table %s IS %s';
    dttDomain:
      Result := 'comment on trigger %s IS %s';
  end;
end;

function TFBSQLMapper.DoGetColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass;
begin
  case pColumnType of
    ctyByte:
      Result := TSmallintFieldCommand;
    ctySingle:
       Result := TFloatFieldCommand;
    ctyDouble, ctyCurrency, ctyExtended:
      begin
        Result := TDoubleFieldCommand;
      end;
    else
      Result := nil;
  end;
end;

function TFBSQLMapper.DoGetCurrentDateExpression: String;
begin
  Result := 'CURRENT_DATE';
end;

function TFBSQLMapper.DoGetCurrentDateTimeExpression: String;
begin
  Result := 'CURRENT_TIMESTAMP';
end;

function TFBSQLMapper.DoGetCurrentTimeExpression: String;
begin
  Result := 'CURRENT_TIME';
end;

function TFBSQLMapper.DoGetFieldClassTypeToString(pClassType: TClass): string;
begin
  if pClassType = TFBDecimalFieldCommand then
  begin
    Result := 'decimal';
  end
  else if pClassType = TDateTimeFieldCommand then
  begin
    Result := 'timestamp';
  end
  else if pClassType = TBooleanFieldCommand then
  begin
    Result := 'boolean';
  end
  else if pClassType = TBlobFieldCommand then
  begin
    Result := 'blob sub_type 0 segment size';
  end
  else if pClassType = TMemoFieldCommand then
  begin
    Result := 'blob sub_type 1 segment size';
  end
  else if pClassType = TByteFieldCommand then
  begin
    Result := 'smallInt';
  end
  else if (pClassType = TFloatFieldCommand) then
  begin
    Result := 'float';
  end
  else if (pClassType = TDoubleFieldCommand) then
  begin
    Result := 'double precision';
  end;
end;

function TFBSQLMapper.FixIntColumnTypeForDDLColumn(pColumnType: TColumnType): TColumnType;
begin
  Result := pColumnType;
  if Result = ctyByte then
  begin
    Result := ctySmallint;
  end;
end;

function TFBSQLMapper.GenerateDropIndexCommand(lIndexCommand: TCustomCommand): string;
var
  lDropIndex: TDropIndexCommand;
begin
  Result := '';
  if (lIndexCommand.InheritsFrom(TDropIndexCommand)) then
  begin
    lDropIndex := TDropIndexCommand(lIndexCommand);
    Result := Format('drop index %s', [lDropIndex.Name]);
  end;
end;

function TFBSQLMapper.GetColumnTypeFromEnumType(pEnumType: TEnumerationType): TColumnType;
begin
  case pEnumType of
    emChar:
      Result := ctyString;
  else
    Result := ctySmallint;
  end;
end;

function TFBSQLMapper.GetKeyWords: TStrings;
begin
  Result := inherited GetKeyWords;
  Result.DelimitedText := Result.DelimitedText + ';FIRST;NULLS;LAST;ROWS;LIST;RESTART;WITH;STARTING;GEN_ID';
end;

procedure TFBSQLMapper.IncludeLimitRows(var pSQL: string; const pLimitRows: Integer);
begin
  if Pos('SELECT', AnsiUpperCase(pSQL)) > 0 then
  begin
    Insert(' first ' + IntToStr(pLimitRows), pSQL, Pos('SELECT', AnsiUpperCase(pSQL)) + 6);
  end;
end;

procedure TFBSQLMapper.IncludeReturningClause(var pSQLText: string; const pFieldNames: TList<string>);
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

function TFBSQLMapper.SequenceSQL(pSequenceName: string; pSchemaName: string; pRestartWith: Integer; pCommandType: TCommandType): string;
begin
  case pCommandType of
    Select:
      Result := Format('gen_id(%s, 1)', [pSequenceName]);
  else
    Result := inherited;
  end;
end;

function TFBSQLMapper.TableFieldCommandToString(pCommandType: TCommandType): string;
begin
  Result := pCommandType.ToString;
end;

procedure TFBSQLMapper.WriteSQLFormatSettings(pSQLFormatSettings: TSQLFormatSettings);
begin
  pSQLFormatSettings.DateFormat := 'dd.MM.yyyy';
  inherited;
end;

end.
