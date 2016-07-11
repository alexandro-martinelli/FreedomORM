unit AM.Freedom.FieldCommandFactory;

interface

uses
  AM.Freedom.ObjectMapper.DDLObjects,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLCommands.ScaledFieldCommand,
  AM.Freedom.SQLCommands.SizedFieldCommand,
  AM.Freedom.SQLMappers.ISQLMapper,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.SQLMapper.CustomArgument;

type
  TCreateFieldCommandParams = class
  private
    FOnGetFieldCommandClass: TGetFieldCommandClass;
    FColumn: TDDLColumn;
    FSQLMapper: ISQLMapper;
    FSchema: String;
    FSize: UInt16;
    FScale: UInt8;
    FColumnType: TColumnType;
  public
    property Column: TDDLColumn read FColumn write FColumn;
    property ColumnType: TColumnType read FColumnType write FColumnType;
    property Schema: String read FSchema write FSchema;
    property OnGetFieldCommandClass: TGetFieldCommandClass read FOnGetFieldCommandClass write FOnGetFieldCommandClass;
    property Size: UInt16 read FSize write FSize;
    property Scale: UInt8 read FScale write FScale;
    property SQLMapper: ISQLMapper read FSQLMapper write FSQLMapper;
  end;

  TFieldCommandFactory = class sealed
  strict private
    class function CreateArgumentFromDefaultValue(pCreateFieldCommandParams: TCreateFieldCommandParams): TCustomArgument;
    class function ToFieldCommandClass(pCreateFieldCommandParams: TCreateFieldCommandParams): TFieldCommandClass;
  public
    class function CreateFieldCommandFromDDLColumn(pCreateFieldCommandParams: TCreateFieldCommandParams): TCustomFieldCommand;
    class function CreateFieldCommandFromColumnType(pCreateFieldCommandParams: TCreateFieldCommandParams): TCustomFieldCommand;
  end;

implementation

{ TFieldCommandFactory }

uses
  AM.Freedom.Helper.Variant,
  AM.Freedom.SQLCommands.Fields;

class function TFieldCommandFactory.CreateFieldCommandFromDDLColumn(pCreateFieldCommandParams: TCreateFieldCommandParams): TCustomFieldCommand;
begin
  pCreateFieldCommandParams.ColumnType := pCreateFieldCommandParams.Column.ColumnType;
  pCreateFieldCommandParams.Size := pCreateFieldCommandParams.Column.Size;
  pCreateFieldCommandParams.Scale := pCreateFieldCommandParams.Column.Scale;
  Result := TFieldCommandFactory.CreateFieldCommandFromColumnType(pCreateFieldCommandParams);
  if Assigned(Result) then
  begin
    Result.Name := pCreateFieldCommandParams.Column.Name;
    if (Required in pCreateFieldCommandParams.Column.ColumnOptions) then
    begin
      Result.FieldOptions.Nullable := nNotNull;
    end;
    if (pCreateFieldCommandParams.Schema = '') or (pCreateFieldCommandParams.Column.IdOptions.Schemas.FindSchema(pCreateFieldCommandParams.Schema) <> nil) then
    begin
      Result.FieldOptions.Identity := pCreateFieldCommandParams.Column.IdOptions.IsId and
          (pCreateFieldCommandParams.Column.IdOptions.IdOption = Identity);
    end;
    Result.Domain := pCreateFieldCommandParams.Column.Domain;
    if (pCreateFieldCommandParams.Column.DefaultValueOptions.IsNow or pCreateFieldCommandParams.Column.DefaultValueOptions.Value.IsNotNull) then
    begin
      Result.FieldOptions.Default := CreateArgumentFromDefaultValue(pCreateFieldCommandParams);
    end;
  end;
end;

class function TFieldCommandFactory.ToFieldCommandClass(pCreateFieldCommandParams: TCreateFieldCommandParams): TFieldCommandClass;
begin
  case pCreateFieldCommandParams.ColumnType of
    ctySmallint:
      Result := TSmallintFieldCommand;
    ctyInteger:
      Result := TIntegerFieldCommand;
    ctyInt64:
      Result := TInt64FieldCommand;
    ctyChar:
      Result := TCharFieldCommand;
    ctyString:
      Result := TVarcharFieldCommand;
    ctyDate:
      Result := TDateFieldCommand;
    ctyTime:
      Result := TTimeFieldCommand;
    ctyDateTime:
      Result := TDateTimeFieldCommand;
    ctyBlob:
      Result := TBlobFieldCommand;
    ctyMemo:
      Result := TMemoFieldCommand;
    ctyDouble:
      Result := TDoubleFieldCommand;
    ctyBoolean:
      Result := TBooleanFieldCommand;
    ctySingle, ctyExtended, ctyCurrency, ctyEnumerator, ctyByte, ctyXML:
      Result := pCreateFieldCommandParams.SQLMapper.ColumnTypeToFieldCommandClass(pCreateFieldCommandParams.ColumnType);
    else
      Result := nil;
  end;
end;

class function TFieldCommandFactory.CreateArgumentFromDefaultValue(pCreateFieldCommandParams: TCreateFieldCommandParams): TCustomArgument;
var
  lColumn: TDDLColumn;
begin
  lColumn := pCreateFieldCommandParams.Column;
  if lColumn.DefaultValueOptions.IsNow then
  begin
    Result := TLiteralArgument.Create(pCreateFieldCommandParams.SQLMapper.CurrentDateTimeExpression);
  end
  else
  begin
    Result := TValueArgument.CreateAsVariant(lColumn.DefaultValueOptions.Value);
  end;
end;

class function TFieldCommandFactory.CreateFieldCommandFromColumnType(pCreateFieldCommandParams: TCreateFieldCommandParams): TCustomFieldCommand;
var
  lFieldClass: TFieldCommandClass;
begin
  Result := nil;
  lFieldClass := ToFieldCommandClass(pCreateFieldCommandParams);
  if Assigned(lFieldClass) then
  begin
    Result := lFieldClass.Create;
    if Result.InheritsFrom(TScaledFieldCommand) then
    begin
      TScaledFieldCommand(Result).Size := pCreateFieldCommandParams.Size;
      TScaledFieldCommand(Result).Scale := pCreateFieldCommandParams.Scale;
    end
    else if Result.InheritsFrom(TSizedFieldCommand) then
    begin
      TSizedFieldCommand(Result).Size := pCreateFieldCommandParams.Size;
    end;
  end;
end;

end.
