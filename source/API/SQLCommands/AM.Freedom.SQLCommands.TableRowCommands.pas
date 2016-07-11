unit AM.Freedom.SQLCommands.TableRowCommands;

interface

uses
  System.Classes,
  System.Generics.Collections,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.ObjectMapper,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLMappers.WhereClause,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMappers.SelectClause;

type
  TDeleteCommand = class(TCustomCommand)
  strict private
    FTable: TTableArgument;
    FWhereClause: TWhereClause;
  strict protected
    function GetWhereClause: TWhereClause;
    function GetFromTable: TTableArgument;
    procedure SetFromTable(const pTable: TTableArgument);
    function GetCommandType: TCommandType; override;
  public
    constructor Create(pSchemaName: string = ''); overload; override;
    constructor Create(pName: String; pCriterias: array of TCriteria; pAlias: string = ''; pSchemaName: string = ''); reintroduce; overload;
    destructor Destroy; override;

    function From(const pName: String; const pAlias: String = ''): TDeleteCommand; overload;
    function From<T: class>: TDeleteCommand; overload;
    function Where(const pCriterias: Array of TCriteria): TDeleteCommand; overload;
    function Where(const pCriteria: TCriteria): TDeleteCommand; overload;

    property FromTable: TTableArgument read GetFromTable write SetFromTable;
    property WhereClause: TWhereClause read GetWhereClause;
  end;

  TFieldValueArgument = class sealed
  strict private
    FField: TFieldArgument;
    FValue: TCustomArgument;
    function GetAsVariant: Variant;
    function GetAsStream: TStream;
  private protected
    function GetValue: TCustomArgument; virtual;
    procedure SetValue(const pValue: TCustomArgument); virtual;
    function GetField: TFieldArgument; virtual;
    procedure SetField(const pField: TFieldArgument); virtual;
  public
    destructor Destroy; override;
    class function CreateAsValue<T>(const pName: String; const pValue: T; pTableAlias: String = ''; pBlobType: TColumnType = ctyMemo): TFieldValueArgument;
    class function CreateAsArgument(const pName: String; const pValue: TCustomArgument; pTableAlias: String = ''): TFieldValueArgument;
    class function CreateAsNull(const pName: String; pTableAlias: String = ''): TFieldValueArgument;
    property Field: TFieldArgument read GetField write SetField;
    property Value: TCustomArgument read GetValue write SetValue;
    property AsVariant: Variant read GetAsVariant;
    property AsStream: TStream read GetAsStream;
  end;

  TUpdateCommand = class(TCustomCommand)
  strict private
    FTable: TTableArgument;
    FFieldValue: TObjectList<TFieldValueArgument>;
    FWhereClause: TWhereClause;
  strict protected
    function GetTable: TTableArgument; virtual;
    procedure SetTable(const pTable: TTableArgument); virtual;
    function GetCommandType: TCommandType; override;
  public
    constructor Create(pSchemaName: string = ''); override;
    destructor Destroy; override;

    function FindField(pFieldName: String): TFieldValueArgument;

    function TableName(pTableName: String; pAlias: String = ''): TUpdateCommand;
    function FieldValue<T>(pName: String; pValue: T; pTableAlias: String = ''; pBlobType: TColumnType = ctyMemo): TUpdateCommand;
    function FieldColumnType(pColumnType: TColumnType; pName: String; pValue: Variant; pTableAlias: String = ''): TUpdateCommand;

    function Where(pWhere: TCriteria): TUpdateCommand; overload;

    property Table: TTableArgument read GetTable write SetTable;
    property FieldsValues: TObjectList<TFieldValueArgument> read FFieldValue;
    property WhereClause: TWhereClause read FWhereClause;
  end;

  TInsertCommand = class(TCustomCommand)
  private
    FFields: TObjectList<TFieldArgument>;
    FInto: TTableArgument;
    FSelectClause: TSelectClause;
    FValues: TObjectList<TCustomArgument>;
    FReturningFields: TList<String>;
  strict protected
    procedure SetInto(const pInto: TTableArgument); virtual;
    procedure SetSelectClause(const pSelect: TSelectClause); virtual;
    function GetCommandType: TCommandType; override;
  public
    constructor Create(pSchemaName: string = ''); override;
    destructor Destroy; override;
    function FindField(pFieldName: String): TFieldArgument;
    function IntoTable(pName: String; pAlias: String = ''): TInsertCommand;
    function Field(pName: String; pTableAlias: String = ''): TInsertCommand;
    function Value<T>(pValue: T): TInsertCommand;
    function FieldValue<T>(pName: String; pValue: T; pTableAlias: String = ''): TInsertCommand; overload;
    function FieldValue(pName: String; pLiteralValue: String; pTableAlias: String = ''): TInsertCommand; overload;
    function ColumnType(pColumnType: TColumnType; pValue: Variant): TInsertCommand;
    function FieldColumnType(pColumnType: TColumnType; pName: String; pValue: Variant; pTableAlias: String = ''): TInsertCommand;
    function ReturningField(pName: String): TInsertCommand; overload;
    function Select(pSelect: TSelectClause): TInsertCommand; overload;
    function ValueByFieldName(pFieldName: String): Variant;
    function StreamByFieldName(pFieldName: String): TStream;
    property Into: TTableArgument read FInto write SetInto;
    property Fields: TObjectList<TFieldArgument> read FFields;
    property Values: TObjectList<TCustomArgument> read FValues;
    property SelectClause: TSelectClause read FSelectClause write SetSelectClause;
    property ReturningFields: TList<String> read FReturningFields;
  end;

implementation

uses
  System.SysUtils, System.Variants, AM.Freedom.Helper.Variant;

{ TDeleteCommand }

constructor TDeleteCommand.Create(pName: String; pCriterias: array of TCriteria; pAlias: string; pSchemaName: string);
var
  I: Integer;
begin
  Create(pSchemaName);
  SetFromTable(TTableArgument.Create(pName, pAlias));
  for I := Low(pCriterias) to High(pCriterias) do
  begin
    FWhereClause.ListCriterias.Add(pCriterias[I]);
  end;
end;

constructor TDeleteCommand.Create(pSchemaName: string);
begin
  inherited;
  FWhereClause := TWhereClause.Create;
end;

destructor TDeleteCommand.Destroy;
begin
  FreeAndNil(FWhereClause);
  FreeAndNil(FTable);
  inherited;
end;

function TDeleteCommand.GetCommandType: TCommandType;
begin
  Result  := TCommandType.Drop;
end;

function TDeleteCommand.GetFromTable: TTableArgument;
begin
  Result := FTable;
end;

function TDeleteCommand.GetWhereClause: TWhereClause;
begin
  Result := FWhereClause;
end;

procedure TDeleteCommand.SetFromTable(const pTable: TTableArgument);
begin
  if pTable <> FTable then
  begin
    FreeAndNil(FTable);
  end;
  FTable := pTable;
end;

function TDeleteCommand.From(const pName, pAlias: String): TDeleteCommand;
begin
  Result := Self;
  Result.FromTable := TTableArgument.Create(pName, pAlias);
end;

function TDeleteCommand.From<T>: TDeleteCommand;
var
  lMapper: TObjectMapper;
  lParams: TObjectToMapperParams;
begin
  lParams := TObjectToMapperParams.Create;
  lParams.MetaClassType := T;
  lParams.Options := [];
  lMapper := TObjectToMapper.ObjectToMapper(lParams);
  try
    Result := From(lMapper.Name, lMapper.Alias);
  finally
    TObjectToMapper.UnlockMapper(lMapper.GetHashCode);
  end;
end;

function TDeleteCommand.Where(const pCriterias: array of TCriteria): TDeleteCommand;
var
  I: Integer;
begin
  Result := Self;
  for I := Low(pCriterias) to High(pCriterias) do
  begin
    FWhereClause.AddCriteria(pCriterias[I]);
  end;
end;

function TDeleteCommand.Where(const pCriteria: TCriteria): TDeleteCommand;
begin
  Result := Self;
  FWhereClause.AddCriteria(pCriteria);
end;

{ TUpdateCommand }

constructor TUpdateCommand.Create(pSchemaName: string);
begin
  inherited;
  FFieldValue := TObjectList<TFieldValueArgument>.Create;
  FWhereClause := TWhereClause.Create;
end;

destructor TUpdateCommand.Destroy;
begin
  FreeAndnil(FTable);
  FreeAndnil(FFieldValue);
  FreeAndnil(FWhereClause);
  inherited;
end;

function TUpdateCommand.FieldValue<T>(pName: String; pValue: T; pTableAlias: String; pBlobType: TColumnType): TUpdateCommand;
begin
  Result := Self;
  FFieldValue.Add(TFieldValueArgument.CreateAsValue<T>(pName, pValue, pTableAlias, pBlobType));
end;

function TUpdateCommand.FieldColumnType(pColumnType: TColumnType; pName: String; pValue: Variant; pTableAlias: String): TUpdateCommand;
var
  lFieldValue: TFieldValueArgument;
begin
  lFieldValue := nil;
  if (VarIsNull(pValue)) then
  begin
    lFieldValue := TFieldValueArgument.CreateAsNull(pName, pTableAlias);
  end
  else
  begin
    case pColumnType of
      ctyByte: lFieldValue := TFieldValueArgument.CreateAsValue<Byte>(pName, pValue, pTableAlias);
      ctySmallint: lFieldValue := TFieldValueArgument.CreateAsValue<SmallInt>(pName, pValue, pTableAlias);
      ctyInteger: lFieldValue := TFieldValueArgument.CreateAsValue<Integer>(pName, pValue, pTableAlias);
      ctyInt64: lFieldValue := TFieldValueArgument.CreateAsValue<Int64>(pName, pValue, pTableAlias);
      ctyChar, ctyString: lFieldValue := TFieldValueArgument.CreateAsValue<String>(pName, pValue, pTableAlias);
      ctySingle: lFieldValue := TFieldValueArgument.CreateAsValue<Single>(pName, pValue, pTableAlias);
      ctyDouble: lFieldValue := TFieldValueArgument.CreateAsValue<Double>(pName, pValue, pTableAlias);
      ctyCurrency: lFieldValue := TFieldValueArgument.CreateAsValue<Currency>(pName, pValue, pTableAlias);
      ctyExtended: lFieldValue := TFieldValueArgument.CreateAsValue<Extended>(pName, pValue, pTableAlias);
      ctyDate: lFieldValue := TFieldValueArgument.CreateAsValue<TDate>(pName, pValue, pTableAlias);
      ctyTime: lFieldValue := TFieldValueArgument.CreateAsValue<TTime>(pName, pValue, pTableAlias);
      ctyDateTime: lFieldValue := TFieldValueArgument.CreateAsValue<TDateTime>(pName, pValue, pTableAlias);
      ctyEnumerator:
      begin
        case VarType(pValue) of
          varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord,
              varInt64, varUInt64: lFieldValue := TFieldValueArgument.CreateAsValue<Integer>(pName, pValue, pTableAlias);
          varOleStr, varString, varUString: lFieldValue := TFieldValueArgument.CreateAsValue<String>(pName, pValue, pTableAlias);
        end;
      end;
      else
        begin
          if (not VarIsNull(pValue)) then
          begin
            case VarType(pValue) of
              varSmallInt, varInteger, varShortInt, varByte, varWord: lFieldValue := TFieldValueArgument.CreateAsValue<Integer>(pName, pValue, pTableAlias);
              varLongWord, varInt64, varUInt64: lFieldValue := TFieldValueArgument.CreateAsValue<Int64>(pName, pValue, pTableAlias);
              varBoolean: lFieldValue := TFieldValueArgument.CreateAsValue<Boolean>(pName, pValue, pTableAlias);
              else
                lFieldValue := TFieldValueArgument.CreateAsValue<String>(pName, pValue, pTableAlias);
            end;
          end
          else
          begin
            lFieldValue := TFieldValueArgument.CreateAsNull(pName, pTableAlias);
          end;
        end;
    end;
  end;
  if (Assigned(lFieldValue)) then
  begin
    FFieldValue.Add(lFieldValue);
  end;
  Result := Self;
end;

function TUpdateCommand.FindField(pFieldName: String): TFieldValueArgument;
var
  lField: TFieldValueArgument;
begin
  Result := nil;
  for lField in FFieldValue do
  begin
    if SameText(lField.Field.Name, pFieldName) then
    begin
      Result := lField;
      Break;
    end;
  end;
end;

function TUpdateCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Alter;
end;

function TUpdateCommand.GetTable: TTableArgument;
begin
  Result := FTable;
end;

procedure TUpdateCommand.SetTable(const pTable: TTableArgument);
begin
  if Assigned(FTable) then
  begin
    FreeAndNil(FTable);
  end;
  FTable := pTable;
end;

function TUpdateCommand.TableName(pTableName, pAlias: String): TUpdateCommand;
begin
  Result := Self;
  SetTable(TTableArgument.Create(pTableName, pAlias));
end;

function TUpdateCommand.Where(pWhere: TCriteria): TUpdateCommand;
begin
  Result := Self;
  FWhereClause.AddCriteria(pWhere);
end;

{ TFieldValueArgument }

class function TFieldValueArgument.CreateAsArgument(const pName: String; const pValue: TCustomArgument;
  pTableAlias: String): TFieldValueArgument;
begin
  Result := TFieldValueArgument.Create;
  Result.Field := TFieldArgument.Create(pName, pTableAlias);
  Result.Value := pValue;
end;

class function TFieldValueArgument.CreateAsNull(const pName: String; pTableAlias: String): TFieldValueArgument;
begin
  Result := TFieldValueArgument.Create;
  Result.Field := TFieldArgument.Create(pName, pTableAlias);
  Result.Value := TNullArgument.Create;
end;

class function TFieldValueArgument.CreateAsValue<T>(const pName: String; const pValue: T; pTableAlias: String; pBlobType: TColumnType): TFieldValueArgument;
begin
  Result := TFieldValueArgument.Create;
  Result.Field := TFieldArgument.Create(pName, pTableAlias);
  Result.Value := TValueArgument.CreateAs<T>(pValue, pBlobType);
end;

destructor TFieldValueArgument.Destroy;
begin
  FreeAndNil(FField);
  FreeAndNil(FValue);
  inherited;
end;

function TFieldValueArgument.GetAsStream: TStream;
begin
  Result := nil;
  if (FValue.InheritsFrom(TValueArgument)) then
  begin
    Result := TValueArgument(FValue).AsStream;
  end;
end;

function TFieldValueArgument.GetAsVariant: Variant;
begin
  Result := Null;
  if (FValue.InheritsFrom(TValueArgument)) then
  begin
    Result := TValueArgument(FValue).Value;
  end;
end;

function TFieldValueArgument.GetField: TFieldArgument;
begin
  Result := FField;
end;

function TFieldValueArgument.GetValue: TCustomArgument;
begin
  Result := FValue;
end;

procedure TFieldValueArgument.SetField(const pField: TFieldArgument);
begin
  if FField <> pField then
  begin
    FreeAndNil(FField);
  end;
  FField := pField;
end;

procedure TFieldValueArgument.SetValue(const pValue: TCustomArgument);
begin
  if FValue <> pValue then
  begin
    FreeAndNil(FValue);
  end;
  FValue := pValue;
end;

{ TInsertCommand }

constructor TInsertCommand.Create(pSchemaName: string);
begin
  inherited Create(pSchemaName);
  FFields := TObjectList<TFieldArgument>.Create;
  FValues := TObjectList<TCustomArgument>.Create;
  FReturningFields := TList<String>.Create;
end;

destructor TInsertCommand.Destroy;
begin
  FreeAndNil(FFields);
  FreeAndNil(FValues);
  FreeAndNil(FInto);
  FreeAndNil(FSelectClause);
  FreeAndNil(FReturningFields);
  inherited;
end;

function TInsertCommand.Field(pName, pTableAlias: String): TInsertCommand;
begin
  if FindField(pName) = nil then
  begin
    FFields.Add(TFieldArgument.Create(pName, pTableAlias));
  end;
  Result := Self;
end;

function TInsertCommand.FieldValue(pName, pLiteralValue, pTableAlias: String): TInsertCommand;
begin
  Field(pName, pTableAlias).Values.Add(TLiteralArgument.Create(pLiteralValue));
  Result := Self;
end;

function TInsertCommand.FieldValue<T>(pName: String; pValue: T; pTableAlias: String): TInsertCommand;
begin
  if FindField(pName) = nil then
  begin
    Result := Field(pName, pTableAlias).Value<T>(pValue);
  end;
end;

function TInsertCommand.FieldColumnType(pColumnType: TColumnType; pName: String; pValue: Variant;
  pTableAlias: String): TInsertCommand;
begin
  Field(pName, pTableAlias);
  Result := ColumnType(pColumnType, pValue);
end;

function TInsertCommand.FindField(pFieldName: String): TFieldArgument;
var
  lField: TFieldArgument;
begin
  Result := nil;
  for lField in FFields do
  begin
    if SameText(lField.Name, pFieldName) then
    begin
      Result := lField;
      Break
    end;
  end;
end;

function TInsertCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Add;
end;

function TInsertCommand.IntoTable(pName, pAlias: String): TInsertCommand;
begin
  SetInto(TTableArgument.Create(pName, pAlias));
  Result := Self;
end;

function TInsertCommand.ReturningField(pName: String): TInsertCommand;
begin
  Result := Self;
  if pName <> '' then
  begin
    if not FReturningFields.Contains(pName) then
    begin
      FReturningFields.Add(pName);
    end;
  end;
end;

function TInsertCommand.Select(pSelect: TSelectClause): TInsertCommand;
begin
  SetSelectClause(pSelect);
  Result := Self;
end;

procedure TInsertCommand.SetInto(const pInto: TTableArgument);
begin
  if FInto <> pInto then
  begin
    FreeAndNil(FInto);
  end;
  FInto := pInto;
end;

procedure TInsertCommand.SetSelectClause(const pSelect: TSelectClause);
begin
  if FSelectClause <> pSelect then
  begin
    FreeAndNil(FSelectClause);
  end;
  FSelectClause := pSelect;
end;

function TInsertCommand.StreamByFieldName(pFieldName: String): TStream;
var
  lIndex: Integer;
  lItem: TCustomArgument;
begin
  Result := nil;
  for lIndex := 0 to Fields.Count - 1 do
  begin
    if SameText(Fields.Items[lIndex].Name, pFieldName) then
    begin
      lItem := Values.Items[lIndex];
      if (lItem.InheritsFrom(TValueArgument)) then
      begin
        Result := TValueArgument(Values.Items[lIndex]).AsStream;
      end
      else
      begin
        Result := nil;
      end;
      Break;
    end;
  end;
end;

function TInsertCommand.Value<T>(pValue: T): TInsertCommand;
begin
  FValues.Add(TValueArgument.CreateAs<T>(pValue));
  Result := Self;
end;

function TInsertCommand.ValueByFieldName(pFieldName: String): Variant;
var
  lIndex: Integer;
  lItem: TCustomArgument;
begin
  Result := Null;
  for lIndex := 0 to Fields.Count - 1 do
  begin
    if SameText(Fields.Items[lIndex].Name, pFieldName) then
    begin
      lItem := Values.Items[lIndex];
      if (lItem.InheritsFrom(TValueArgument)) then
      begin
        Result := TValueArgument(Values.Items[lIndex]).Value;
      end
      else if (lItem.InheritsFrom(TLiteralArgument)) then
      begin
        Result := TLiteralArgument(Values.Items[lIndex]).LiteralValue;
      end
      else
      begin
        Result := Null;
      end;
      Break;
    end;
  end;
end;

function TInsertCommand.ColumnType(pColumnType: TColumnType; pValue: Variant): TInsertCommand;
var
  lValueArgument: TCustomArgument;
begin
  if (VarIsNull(pValue)) then
  begin
    lValueArgument := TNullArgument.Create;
  end
  else
  begin
    case pColumnType of
      ctyByte: lValueArgument := TValueArgument.CreateAs<Byte>(pValue);
      ctySmallint: lValueArgument := TValueArgument.CreateAs<SmallInt>(pValue);
      ctyInteger: lValueArgument := TValueArgument.CreateAs<Integer>(pValue);
      ctyInt64: lValueArgument := TValueArgument.CreateAs<Int64>(pValue);
      ctyChar, ctyString: lValueArgument := TValueArgument.CreateAs<String>(pValue);
      ctySingle: lValueArgument := TValueArgument.CreateAs<Single>(pValue);
      ctyDouble: lValueArgument := TValueArgument.CreateAs<Double>(pValue);
      ctyCurrency: lValueArgument := TValueArgument.CreateAs<Currency>(pValue);
      ctyExtended: lValueArgument := TValueArgument.CreateAs<Extended>(pValue);
      ctyDate: lValueArgument := TValueArgument.CreateAs<TDate>(pValue);
      ctyTime: lValueArgument := TValueArgument.CreateAs<TTime>(pValue);
      ctyDateTime: lValueArgument := TValueArgument.CreateAs<TDateTime>(pValue);
      ctyEnumerator: lValueArgument := TValueArgument.CreateAs<Integer>(pValue);
      ctyBlob: lValueArgument := TValueArgument.CreateAs<String>(pValue);
      else
        begin
          case VarType(pValue) of
            varSmallInt, varInteger, varShortInt, varByte, varWord: lValueArgument := TValueArgument.CreateAs<Integer>(pValue);
            varLongWord, varInt64, varUInt64: lValueArgument := TValueArgument.CreateAs<Int64>(pValue);
            varBoolean: lValueArgument := TValueArgument.CreateAsBoolean(pValue);
            else
              lValueArgument := TValueArgument.CreateAs<String>(pValue);
          end;
        end;
    end;
  end;
  FValues.Add(lValueArgument);
  Result := Self;
end;

end.
