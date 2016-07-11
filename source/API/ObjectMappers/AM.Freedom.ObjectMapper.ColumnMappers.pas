unit AM.Freedom.ObjectMapper.ColumnMappers;

interface

uses
  System.Classes,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.EnumerationTypes,
  System.Rtti,
  AM.Freedom.ObjectMapper.ColumnsList;

type
  TBooleanColumnMapper = class(TCustomColumnMapper)
  strict private
    FValueTrue: Variant;
    FValueFalse: Variant;
    FInternalColumnType: TColumnType;
  private
    procedure SetInternalColumnType(const pColumnType: TColumnType);
  protected
    function GetBooleanTValue: TValue; override;
    function DoGetNullableValue(pInitialValue: Variant): Variant; override;
    function DoGetCurrentValue(pFieldValue: Variant): Variant; override;
  public
    constructor Create; override;
    property ValueTrue: Variant read FValueTrue write FValueTrue;
    property ValueFalse: Variant read FValueFalse write FValueFalse;
    property InternalColumnType: TColumnType read FInternalColumnType write SetInternalColumnType;
  end;

  TEnumerationColumnMapper = class(TCustomColumnMapper)
  strict private const
    cUnkowPos = -9999;
  strict private
    FEnumType: TEnumerationType;
    FEnumCharOf: TArray<Char>;
  strict private
    function PosInEnumCharArray(pValue: Variant): Int64;
  protected
    function GetEnumeratiorTValue: TValue; override;
    function DoGetCurrentValue(pFieldValue: Variant): Variant; override;
  public
    constructor Create; override;
    property EnumType: TEnumerationType read FEnumType write FEnumType;
    property EnumCharOf: TArray<Char> read FEnumCharOf write FEnumCharOf;
  end;

  TBlobColumnMapper = class(TCustomColumnMapper)
  private
    FBlobType: TBlobType;
    FStreamColumnValue: TStream;
    procedure SetStreamColumnValue(const Value: TStream);
  public
    constructor Create; override;
    destructor Destroy; override;
    property BlobType: TBlobType read FBlobType write FBlobType default TBlobType.Text;
    property StreamColumnValue: TStream read FStreamColumnValue write SetStreamColumnValue;
  end;

  TReferenceColumnMapper = class abstract(TCustomColumnMapper)
  strict private
    FRefObjectAlias: String;
    FRefColumnName: string;
    FRefColumnType: TColumnType;
    FRefObjectName: String;
    FRefClass: TClass;
    FOriginalRefObjectAlias: String;
  public
    property RefColumnName: string read FRefColumnName write FRefColumnName;
    property RefObjectName: String read FRefObjectName write FRefObjectName;
    property RefObjectAlias: String read FRefObjectAlias write FRefObjectAlias;
    property OriginalRefObjectAlias: String read FOriginalRefObjectAlias write FOriginalRefObjectAlias;
    property RefColumnType: TColumnType read FRefColumnType write FRefColumnType;
    property RefMetaClass: TClass read FRefClass write FRefClass;
  end;

  TJoinedColumnMapper = class(TReferenceColumnMapper)
  strict private
    FJoinKind: TJoinKind;
    FRefResultColumnName: String;
    FDeleteAction: TForeignOption;
    FUpdateAction: TForeignOption;
  protected
    function GetJoinedTValue: TValue; override;
  public
    property JoinKind: TJoinKind read FJoinKind write FJoinKind;
    property RefResultColumnName: String read FRefResultColumnName write FRefResultColumnName;
    property UpdateAction: TForeignOption read FUpdateAction write FUpdateAction;
    property DeleteAction: TForeignOption read FDeleteAction write FDeleteAction;
  end;

  TDetailColumnMapper = class(TReferenceColumnMapper);

  TExtensionColumnMapper = class(TCustomColumnMapper)
  private
    FChildColumns: TColumnsList;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ChildColumns: TColumnsList read FChildColumns;
  end;


implementation

uses
  System.Variants,
  AM.Freedom.Helper.Variant,
  AM.Freedom.Helper.RttiField,
  AM.Freedom.Helper.RttiType;

{ TBlobColumnMapper }

constructor TBlobColumnMapper.Create;
begin
  inherited;
  FBlobType := TBlobType.Text;
  FStreamColumnValue := TMemoryStream.Create;
end;

destructor TBlobColumnMapper.Destroy;
begin
  FStreamColumnValue.Free;
  inherited;
end;

procedure TBlobColumnMapper.SetStreamColumnValue(const Value: TStream);
begin
  if Assigned(FStreamColumnValue) then
  begin
    FStreamColumnValue.Free;
    FStreamColumnValue := nil;
  end;
  FStreamColumnValue := Value;
end;

{ TEnumerationColumnMapper }

constructor TEnumerationColumnMapper.Create;
begin
  inherited;
  SetLength(FEnumCharOf, 0);
end;

function TEnumerationColumnMapper.DoGetCurrentValue(pFieldValue: Variant): Variant;
begin
  Result := pFieldValue.TryToInt;
  if (FEnumType = emChar) then
  begin
    Result := FEnumCharOf[Result.TryToInt];
  end;
end;

function TEnumerationColumnMapper.GetEnumeratiorTValue: TValue;
var
  lPos: Int64;
begin
  case FEnumType of
    emChar: lPos := PosInEnumCharArray(ColumnValue);
    else
      lPos := ColumnValue.TryToInt;
  end;
  if lPos = cUnkowPos then
  begin
    Result := 0;
  end;
  Result := TValue.FromOrdinal(RttiOptions.RttiField.FieldType.Handle, lPos);
end;

function TEnumerationColumnMapper.PosInEnumCharArray(pValue: Variant): Int64;
var
  lIndex: Integer;
begin
  Result := cUnkowPos;
  for lIndex := Low(FEnumCharOf) to High(FEnumCharOf) do
  begin
    if FEnumCharOf[lIndex] = pValue then
    begin
      Result := lIndex;
      Break
    end;
  end;
end;

{ TBooleanColumnMapper }

constructor TBooleanColumnMapper.Create;
begin
  inherited;
  FValueTrue := True;
  FValueFalse := False;
  FInternalColumnType := ctyBoolean;
end;

function TBooleanColumnMapper.DoGetCurrentValue(pFieldValue: Variant): Variant;
begin
  Result := inherited;
  if (InternalColumnType <> ctyBoolean) then
  begin
    if (pFieldValue) then
    begin
      Result := FValueTrue;
    end
    else
    begin
      Result := FValueFalse;
    end;
  end;
end;

function TBooleanColumnMapper.DoGetNullableValue(pInitialValue: Variant): Variant;
begin
  Result := Null;
  if (not VarIsNull(pInitialValue)) then
  begin
    if (pInitialValue) then
    begin
      Result := FValueTrue
    end
    else
    begin
      Result := FValueFalse;
    end;
  end;
end;

function TBooleanColumnMapper.GetBooleanTValue: TValue;
begin
  Result := TValue.From<Boolean>(ColumnValue = FValueTrue);
end;

procedure TBooleanColumnMapper.SetInternalColumnType(const pColumnType: TColumnType);
begin
  FInternalColumnType := pColumnType;
  case FInternalColumnType of
    ctyByte, ctySmallint, ctyInteger, ctyInt64:
      begin
        FValueFalse := 0;
        FValueTrue := 1;
      end;
  end;
end;

{ TJoinedColumnMapper }

function TJoinedColumnMapper.GetJoinedTValue: TValue;
var
  lValue: Variant;
  lColumnType: TColumnType;
begin
  Result := nil;
  if (not RttiOptions.RttiField.FieldType.IsInstance) then
  begin
    lColumnType := RttiOptions.RttiField.FieldType.ToColumnType;
    lValue := RttiOptions.RttiFieldHelper.GetVariantValue(RttiOptions.RttiObject, lColumnType);
    case lColumnType of
      ctyByte:
        Result := TValue.From<Byte>(lValue);
      ctySmallint:
        Result := TValue.From<SmallInt>(lValue);
      ctyInteger:
        Result := TValue.From<Integer>(lValue);
      ctyInt64:
        Result := TValue.From<Int64>(lValue);
      ctyChar, ctyString:
        Result := TValue.From<String>(lValue);
      ctySingle:
        Result := TValue.From<Single>(lValue);
      ctyDouble:
        Result := TValue.From<Double>(lValue);
      ctyCurrency:
        Result := TValue.From<Currency>(lValue);
      ctyExtended:
        Result := TValue.From<Extended>(lValue);
      ctyDate:
        Result := TValue.From<TDate>(lValue);
      ctyTime:
        Result := TValue.From<TTime>(lValue);
      ctyDateTime:
        Result := TValue.From<TDateTime>(VarToDateTime(lValue));
      ctyBoolean:
        Result := TValue.From<Boolean>(lValue);
    end;
  end;
end;

{ TExtensioColumnMapper }

constructor TExtensionColumnMapper.Create;
begin
  inherited;
  FChildColumns := TColumnsList.Create(False);
end;

destructor TExtensionColumnMapper.Destroy;
begin
  FChildColumns.Free;
  inherited;
end;

end.
