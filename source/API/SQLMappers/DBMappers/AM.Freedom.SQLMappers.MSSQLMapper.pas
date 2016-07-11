unit AM.Freedom.SQLMappers.MSSQLMapper;

interface

uses
  System.Classes,
  System.Generics.Collections,
  AM.Freedom.SQLMappers.CustomSQLMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Exceptions,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLMappers.SQLFormatSettings,
  AM.Freedom.SQLMappers.ISQLFormatter,
  AM.Freedom.SQLCommands.IndexCommands, AM.Freedom.IndexColumn;

type
  TSelectOptions = class sealed
  private
    FWithNoLock: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetWithNoLock(const Value: Boolean);
    procedure DoChange;
  public
    procedure Assign(pSource: TSelectOptions);
    property WithNoLock: Boolean read FWithNoLock write SetWithNoLock;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TMSSQLMapper = class(TCustomSQLMapper)
  strict private
    FSelectOptions: TSelectOptions;
    FDateFormat: String;
    function OnGetDateFormat: string;
    function ExtractDateFormat(pCharFormat: Char): String;
  strict protected
    function GetKeyWords: TStrings; override;
    procedure IncludeLimitRows(var pSQL: string; const pLimitRows: Integer); override;
    procedure IncludeReturningClause(var pSQLText: string; const pFieldNames: TList<string>); override;
    function IncludeDirectives(pFrom: string): string; override;
    function GetAlterTableSingleCommand: Boolean; override;
    function DescriptionText(pDescriptionTextType: TDescriptionTextType): string; override;
    function DoGetFieldClassTypeToString(pClassType: TClass): string; override;
    function GetAlwaysAlterDataTypeOnChangeProperties: Boolean; override;
    function DoGetColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass; override;
    function GetSQLForCreateDomain: string; override;
    function GetSQLForDropDomain: string; override;
    function SequenceSQL(pSequenceName: string; pSchemaName: string; pRestartWith: Integer; pCommandType: TCommandType): string; override;
    procedure WriteSQLFormatSettings(pSQLFormatSettings: TSQLFormatSettings); override;
    procedure AdjustIndexCommand(lIndexCommand: TCustomCommand); override;
    function GenerateDropIndexCommand(lIndexCommand: TCustomCommand): string; override;
    function DoGetCurrentDateExpression: String; override;
    function DoGetCurrentDateTimeExpression: String; override;
    function DoGetCurrentTimeExpression: String; override;
    function BooleanToDefaultExpression(pBooleanValue: Boolean): Variant; override;
    function FormatLikeExpression(pExpression: string): string; override;
  public
    constructor Create(pAsDefault: Boolean = False; pISQLFormater: ISQLFormatter = nil); override;
    destructor Destroy; override;
    property SelectOptions: TSelectOptions read FSelectOptions write FSelectOptions;
  end;

implementation

uses
  System.StrUtils,
  System.SysUtils,
  System.Math,
  AM.Freedom.SQLCommands.MSSQLFields,
  AM.Freedom.SQLCommands.SequenceCommands,
  AM.Freedom.SQLCommands.Fields,
  AM.Freedom.DefaultsClassRegister,
  AM.Freedom.IDBPersistent,
  AM.Freedom.DBPersistent.IDBConnector,
  AM.Freedom.SQLMappers.SelectClause,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.Helper.CommandType,
  AM.Freedom.ICursor;

{ TMSSQLMapper }

procedure TMSSQLMapper.AdjustIndexCommand(lIndexCommand: TCustomCommand);
var
  lCreateIndex: TCreateIndexCommand;
  lColumn: TIndexColumn;
begin
  if (lIndexCommand.InheritsFrom(TCreateIndexCommand)) then
  begin
    lCreateIndex := TCreateIndexCommand(lIndexCommand);
    if lCreateIndex.Option <> ioClustered then
    begin
      lCreateIndex.Option := ioNone;
    end;
    for lColumn in lCreateIndex.Columns do
    begin
      if (lColumn.NullOption <> noNone) then
      begin
        lColumn.NullOption := noNone;
      end;
    end;
  end;
end;

function TMSSQLMapper.BooleanToDefaultExpression(pBooleanValue: Boolean): Variant;
begin
  Result := ifthen(pBooleanValue, 1, 0);
end;

constructor TMSSQLMapper.Create(pAsDefault: Boolean; pISQLFormater: ISQLFormatter);
begin
  inherited Create(pAsDefault, pISQLFormater);
  FSelectOptions := TSelectOptions.Create;
end;

function TMSSQLMapper.DescriptionText(pDescriptionTextType: TDescriptionTextType): string;
begin
  // case pDescriptionTextType of
  // Column: Result := '';
  // Table: ;
  // Domain: ;
  // end;
end;

destructor TMSSQLMapper.Destroy;
begin
  FSelectOptions.Free;
  inherited;
end;

function TMSSQLMapper.DoGetColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass;
begin
  case pColumnType of
    ctyByte: Result := TByteFieldCommand;
    ctySingle, ctyDouble, ctyCurrency, ctyExtended:
      begin
        Result := TFloatFieldCommand;
      end;
    ctyXML: Result := TXMLFieldCommand;
    else
      Result := nil;
  end;
end;

function TMSSQLMapper.DoGetCurrentDateExpression: String;
begin
  Result := 'GetDate()';
end;

function TMSSQLMapper.DoGetCurrentDateTimeExpression: String;
begin
  Result := DoGetCurrentDateExpression;
end;

function TMSSQLMapper.DoGetCurrentTimeExpression: String;
begin
  Result := DoGetCurrentDateExpression;
end;

function TMSSQLMapper.DoGetFieldClassTypeToString(pClassType: TClass): string;
begin
  Result := '';
  if pClassType = TByteFieldCommand then
  begin
    Result := 'tinyint';
  end
  else if pClassType = TBooleanFieldCommand then
  begin
    Result := 'bit';
  end
  else if pClassType = TBlobFieldCommand then
  begin
    Result := 'varbinary';
  end
  else if pClassType = TMemoFieldCommand then
  begin
    Result := 'text';
  end
  else if pClassType = TDateTimeFieldCommand then
  begin
    Result := 'datetime';
  end
  else if pClassType = TXMLFieldCommand then
  begin
    Result := 'xml';
  end
  else if pClassType = TMSSQLSmallMoneyFieldCommand then
  begin
    Result := 'smallmoney';
  end
  else if pClassType = TMSSQLMoneyFieldCommand then
  begin
    Result := 'money';
  end
  else if (pClassType = TFloatFieldCommand) or (pClassType = TDoubleFieldCommand) then
  begin
    Result := 'float';
  end;
end;

function TMSSQLMapper.FormatLikeExpression(pExpression: string): string;
begin
  Result := pExpression;
  Result := ReplaceText(Result, ' ', '%');
end;

function TMSSQLMapper.GenerateDropIndexCommand(lIndexCommand: TCustomCommand): string;
var
  lDropIndex: TDropIndexCommand;
begin
  Result := '';
  if (lIndexCommand.InheritsFrom(TDropIndexCommand)) then
  begin
    lDropIndex := TDropIndexCommand(lIndexCommand);
    Result := Format('drop index %s on %s%s', [lDropIndex.Name, ifthen(lDropIndex.Schema <> '', lDropIndex.Schema + '.'), lDropIndex.OnTable]);
  end;
end;

function TMSSQLMapper.GetAlterTableSingleCommand: Boolean;
begin
  Result := True;
end;

function TMSSQLMapper.GetAlwaysAlterDataTypeOnChangeProperties: Boolean;
begin
  Result := True;
end;

function TMSSQLMapper.GetKeyWords: TStrings;
begin
  Result := inherited GetKeyWords;
  Result.DelimitedText := Result.DelimitedText +
    ';TOP;INT;TINYINT;OUTPUT;INSERTED;REAL;WITH;NOLOCK;DATEADD;GETDATE;CLUSTERED';
end;

function TMSSQLMapper.GetSQLForCreateDomain: string;
begin
  Result := 'create type %s from %s %s';
end;

function TMSSQLMapper.GetSQLForDropDomain: string;
begin
  Result := 'drop type %s';
end;

function TMSSQLMapper.IncludeDirectives(pFrom: string): string;
begin
  Result := inherited;
  if (FSelectOptions.WithNoLock) and (Result <> '') then
  begin
    Result := Result + ' with (nolock)';
  end;
end;

procedure TMSSQLMapper.IncludeLimitRows(var pSQL: string; const pLimitRows: Integer);
begin
  if Pos('SELECT', AnsiUpperCase(pSQL)) > 0 then
  begin
    Insert(' top ' + IntToStr(pLimitRows), pSQL, Pos('SELECT', AnsiUpperCase(pSQL)) + 6);
  end;
end;

procedure TMSSQLMapper.IncludeReturningClause(var pSQLText: string; const pFieldNames: TList<string>);
var
  lOutput: string;
  lResultOutput: string;
begin
  lResultOutput := '';
  for lOutput in pFieldNames do
  begin
    lResultOutput := lResultOutput + ifthen(lResultOutput <> '', ', ') + 'inserted.' + lOutput;
  end;
  if lResultOutput <> '' then
  begin
    if Pos('VALUES', UpperCase(pSQLText)) > 0 then
    begin
      Insert(' output ' + lResultOutput + ' ', pSQLText, Pos('VALUES', UpperCase(pSQLText)) - 1);
    end
    else
    begin
      Insert(' output ' + lResultOutput + ' ', pSQLText, Pos('SELECT', UpperCase(pSQLText)) - 1);
    end;
  end;
end;

function TMSSQLMapper.OnGetDateFormat: string;
var
  lSelect: TSelectClause;
  lSQLWhere: TSelectClause;
  lCursor: ICursor;
  lDateFormat: String;
begin
  if (FDateFormat = '') then
  begin
    lSelect := TSelectClause.CreateFromTable('master..syslanguages');
    lSelect.Field('dateformat');
    lSQLWhere := TSelectClause.CreateFromTable('master..sysconfigures');
    lSQLWhere.Field('value');
    lSQLWhere.Where(TCriteria.CreateAsEqual(TFieldArgument.Create('comment'), TValueArgument.CreateAsString('default language')));
    lSelect.Where(TCriteria.CreateAsEqual(TFieldArgument.Create('langid'), lSQLWhere));
    lCursor := TDefaultsClassRegister.DefaultPersistent.GetCursorWithSelect(lSelect);
    lDateFormat := lCursor.Values[0];
    lCursor := nil;
    FDateFormat := Format('%s-%s-%s', [ExtractDateFormat(lDateFormat[1]), ExtractDateFormat(lDateFormat[2]),
        ExtractDateFormat(lDateFormat[3])]);
  end;
  Result := FDateFormat;
  //Result := 'yyyy-MM-dd';
  //Result := 'dd-MM-yyyy';
end;

function TMSSQLMapper.ExtractDateFormat(pCharFormat: Char): String;
begin
  if (pCharFormat = 'd') then
  begin
    Result := 'dd';
  end
  else if (pCharFormat = 'm') then
  begin
    Result := 'MM';
  end
  else if (pCharFormat = 'y') then
  begin
    Result := 'yyyy';
  end;
end;

function TMSSQLMapper.SequenceSQL(pSequenceName: string; pSchemaName: string; pRestartWith: Integer; pCommandType: TCommandType): string;
begin
  Result := '';
end;

procedure TMSSQLMapper.WriteSQLFormatSettings(pSQLFormatSettings: TSQLFormatSettings);
begin
  pSQLFormatSettings.OnGetDateFormat := OnGetDateFormat;
  inherited;
end;
{ TSelectOptions }

procedure TSelectOptions.Assign(pSource: TSelectOptions);
begin
  SetWithNoLock(pSource.WithNoLock);
end;

procedure TSelectOptions.DoChange;
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Self);
  end;
end;

procedure TSelectOptions.SetWithNoLock(const Value: Boolean);
begin
  FWithNoLock := Value;
  DoChange;
end;

end.
