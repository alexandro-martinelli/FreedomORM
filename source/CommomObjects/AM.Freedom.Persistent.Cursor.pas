unit AM.Freedom.Persistent.Cursor;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Variants,
  AM.Freedom.EnumerationTypes, AM.Freedom.Helper.Variant,
  AM.Freedom.InterfacedObjects, AM.Freedom.ICursor;

type
  TCursorColumn = class sealed
  private
    FColumnName: String;
    FValueTye: TColumnType;
    FColumnOptions: TColumnOptions;
    FIndex: Smallint;
    FSize: UInt16;
  public
    constructor Create(pColumnName: String; pValueTye: TColumnType; pSize: UInt16; pIndex: Smallint; pColumnOptions: TColumnOptions = []);
    property ColumnName: String read FColumnName write FColumnName;
    property ValueTye: TColumnType read FValueTye write FValueTye;
    property ColumnOptions: TColumnOptions read FColumnOptions write FColumnOptions;
    property Index: Smallint read FIndex write FIndex;
    property Size: UInt16 read FSize write FSize;
  end;

  TColumns = class(TObjectList<TCursorColumn>)
  strict private type
    TColumnComparer = class(TComparer<TCursorColumn>)
    public
      function Compare(const Left, Right: TCursorColumn): Integer; override;
    end;
  public
    constructor Create; reintroduce;
  end;

  TColumnRecord = class
  strict private
    FValue: Variant;
    FStream: TStream;
    procedure SetStream(const Value: TStream);
    function GetAsString: String;
    function GetAsInteger: Integer;
    function GetAsFloat: Double;
    function GetAsCurrency: Currency;
    function GetAsDate: TDate;
    function GetAsDateTime: TDateTime;
    function GetAsTime: TTime;
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDate(const Value: TDate);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: String);
    procedure SetAsTime(const Value: TTime);
  public
    constructor Create(pValue: Variant; pStream: TStream = nil);
    destructor Destroy; override;
    property Value: Variant read FValue write FValue;
    property AsStream: TStream read FStream write SetStream;
    property AsString: String read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDate: TDate read GetAsDate write SetAsDate;
    property AsTime: TTime read GetAsTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
  end;

  TRecord = class(TObjectList<TColumnRecord>)
  private
    FIndex: Integer;
  public
    constructor Create(pArrayof: Array of TColumnRecord; pIndex: Smallint); overload;
    constructor Create(pArrayof: Array of Variant; pIndex: Smallint); overload;
    property Index: Integer read FIndex write FIndex;
  end;

  TRecords = class(TObjectList<TRecord>)
  strict private type
    TRecordComparer = class(TComparer<TRecord>)
    public
      function Compare(const Left, Right: TRecord): Integer; override;
    end;
  public
    constructor Create; reintroduce;
  end;

  TPersistentCursor = class sealed(TFreedomInterfacedObject, ICursor)
  strict private
    FColumns: TColumns;
    FRecords: TRecords;
    FRecNo: UInt16;
    function GetColumnCount: Integer;
    function GetRecordCount: Integer;
    function GetRecordByColumnName(pColumnName: String): TColumnRecord;
    function GetValueByColumnName(pColumnName: String): Variant;
    function GetValueByColumnIndex(pColumnIndex: Integer): Variant;
    function GetStreamByColumnName(pColumnName: string): TStream;
    function GetIsEof: Boolean;
    function GetIsBof: Boolean;
    function GetRecNo: UInt16;
    procedure AdjustRecordCapacity(pRecord: TRecord);
    function GetCurrentRecord: TRecord;
    procedure AssignQuery(pQuery: TObject);
    procedure AssignQueryList(pQuery: TObject);
    function GetColumns(pColumnIndex: Integer): string;
    procedure Open;
  public
    constructor Create;
    destructor Destroy; override;
    function FindColumn(pColumn: TCursorColumn): Boolean;
    function ColumnByName(pColumnName: String): TCursorColumn;
    procedure AddRecord(pRecord: TRecord);
    procedure AddColumn(pColumn: TCursorColumn);
    procedure EndUpdate;
    procedure First;
    procedure Next;
    function IsEmpty: Boolean;
    property Columns: TColumns read FColumns;
    property ColumnCount: Integer read GetColumnCount;
    property RecordCount: Integer read GetRecordCount;
    property RecNo: UInt16 read GetRecNo;
    property Eof: Boolean read GetIsEof;
    property Bof: Boolean read GetIsBof;
    property Values[ColumnName: String]: Variant read GetValueByColumnName; default;
    property Values[ColumnIndex: Integer]: Variant read GetValueByColumnIndex; default;
    property Records[ColumnName: String]: TColumnRecord read GetRecordByColumnName;

    property CurrentRecord: TRecord read GetCurrentRecord;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.Exceptions;

{ TCursor }

procedure TPersistentCursor.AddColumn(pColumn: TCursorColumn);
begin
  if pColumn.ColumnName <> '' then
  begin
    if not Assigned(ColumnByName(pColumn.ColumnName)) then
    begin
      FColumns.Add(pColumn);
      FColumns.Sort;
      pColumn := nil;
    end;
  end;
  if Assigned(pColumn) then
  begin
    pColumn.Free;
  end;
end;

procedure TPersistentCursor.AddRecord(pRecord: TRecord);
begin
  AdjustRecordCapacity(pRecord);
  FRecords.Add(pRecord);
end;

procedure TPersistentCursor.AdjustRecordCapacity(pRecord: TRecord);
begin
  if pRecord.Capacity <> FColumns.Count then
  begin
    pRecord.Capacity := FColumns.Count;
    while pRecord.Count <= FColumns.Count do
    begin
      pRecord.Add(TColumnRecord.Create(Null));
    end;
  end;
end;

procedure TPersistentCursor.AssignQueryList(pQuery: TObject);
begin
  // do nothing at now - just interface implementation
end;

procedure TPersistentCursor.AssignQuery(pQuery: TObject);
begin
  // do nothing at now - just interface implementation
end;

function TPersistentCursor.ColumnByName(pColumnName: String): TCursorColumn;
var
  lItem: TCursorColumn;
begin
  Result := nil;
  for lItem in FColumns do
  begin
    if SameText(lItem.ColumnName, pColumnName) then
    begin
      Result := lItem;
      Break;
    end;
  end;
end;

constructor TPersistentCursor.Create;
begin
  FColumns := TColumns.Create;
  FRecords := TRecords.Create;
end;

destructor TPersistentCursor.Destroy;
begin
  FreeAndNil(FColumns);
  FreeAndNil(FRecords);
  inherited;
end;

procedure TPersistentCursor.EndUpdate;
begin
  FColumns.Sort;
end;

function TPersistentCursor.FindColumn(pColumn: TCursorColumn): Boolean;
begin
  Result := FColumns.IndexOf(pColumn) >= 0;
end;

function TPersistentCursor.GetColumnCount: Integer;
begin
  Result := FColumns.Count;
end;

function TPersistentCursor.GetColumns(pColumnIndex: Integer): string;
begin
  if (pColumnIndex < 0) or (pColumnIndex > FColumns.Count) then
  begin
    raise EInvalidCursorColumnIndex.Create(pColumnIndex);
  end
  else
  begin
    Result := FColumns.Items[pColumnIndex].ColumnName;
  end;
end;

function TPersistentCursor.GetCurrentRecord: TRecord;
begin
  Result := FRecords.Items[FRecNo - 1];
end;

function TPersistentCursor.GetIsBof: Boolean;
begin
  Result := FRecNo = 1;
end;

function TPersistentCursor.GetIsEof: Boolean;
begin
  Result := (FRecNo - 1 = FRecords.Count) or IsEmpty;
end;

function TPersistentCursor.GetRecNo: UInt16;
begin
  Result := FRecNo;
end;

function TPersistentCursor.GetRecordCount: Integer;
begin
  Result := FRecords.Count;
end;

function TPersistentCursor.GetStreamByColumnName(pColumnName: string): TStream;
begin
  Result := GetRecordByColumnName(pColumnName).AsStream;
end;

function TPersistentCursor.GetRecordByColumnName(pColumnName: String): TColumnRecord;
var
  lColumn: TCursorColumn;
begin
  lColumn := ColumnByName(pColumnName);
  if Assigned(lColumn) then
  begin
    Result := FRecords.Items[FRecNo - 1][FColumns.IndexOf(lColumn)];
  end
  else
  begin
    raise EInvalidCursorColumnName.Create(pColumnName);
  end;
end;

function TPersistentCursor.GetValueByColumnIndex(pColumnIndex: Integer): Variant;
begin
  Result := Null;
  if (pColumnIndex >= 0) and (pColumnIndex < FColumns.Count) then
  begin
    Result := FRecords.Items[FRecNo - 1][pColumnIndex].Value;
  end;
end;

function TPersistentCursor.GetValueByColumnName(pColumnName: String): Variant;
var
  lColumn: TCursorColumn;
begin
  Result := Null;
  lColumn := ColumnByName(pColumnName);
  if Assigned(lColumn) then
  begin
    Result := FRecords.Items[FRecNo - 1][FColumns.IndexOf(lColumn)].Value;
  end;
end;

function TPersistentCursor.IsEmpty: Boolean;
begin
  Result := GetRecordCount <= 0;
end;

procedure TPersistentCursor.Next;
begin
  if FRecNo <= FRecords.Count + 1 then
  begin
    Inc(FRecNo);
  end;
end;

procedure TPersistentCursor.Open;
begin
  // do nothing;
end;

procedure TPersistentCursor.First;
begin
  if not IsEmpty then
  begin
    FRecNo := 1;
  end
  else
  begin
    FRecNo := 0;
  end;
end;

{ TCursorColumn }

{ TRecord }

constructor TRecord.Create(pArrayof: array of TColumnRecord; pIndex: Smallint);
var
  lContador: Integer;
begin
  inherited Create;
  for lContador := Low(pArrayof) to High(pArrayof) do
  begin
    Add(pArrayof[lContador]);
  end;
  FIndex := pIndex;
end;

constructor TRecord.Create(pArrayof: array of Variant; pIndex: Smallint);
var
  lContador: Integer;
begin
  inherited Create;
  for lContador := Low(pArrayof) to High(pArrayof) do
  begin
    Add(TColumnRecord.Create(pArrayof[lContador]));
  end;
  FIndex := pIndex;
end;

{ TColumns.TColumnComparer }

function TColumns.TColumnComparer.Compare(const Left, Right: TCursorColumn): Integer;
begin
  Result := Left.Index - Right.Index;
end;

{ TColumns }

constructor TColumns.Create;
begin
  inherited Create(TColumnComparer.Create, True);
end;

{ TRecords.TColumnComparer }

function TRecords.TRecordComparer.Compare(const Left, Right: TRecord): Integer;
begin
  Result := left.Index - Right.Index;
end;

{ TRecords }

constructor TRecords.Create;
begin
  Inherited Create(TRecordComparer.Create, True);
end;

{ TColumnRecord }

constructor TColumnRecord.Create(pValue: Variant; pStream: TStream);
begin
  FValue := pValue;
  FStream := pStream;
end;

destructor TColumnRecord.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TColumnRecord.GetAsCurrency: Currency;
begin
  Result := FValue.TryToCurrency;
end;

function TColumnRecord.GetAsDate: TDate;
begin
  Result := FValue.TryToDate;
end;

function TColumnRecord.GetAsDateTime: TDateTime;
begin
  Result := FValue.TryToDateTime;
end;

function TColumnRecord.GetAsFloat: Double;
begin
  Result := FValue.TryToExtended;
end;

function TColumnRecord.GetAsInteger: Integer;
begin
  Result := FValue.TryToInt;
end;

function TColumnRecord.GetAsString: String;
begin
  if (not VarIsNull(FValue)) then
  begin
    Result := FValue;
  end
  else
  begin
    Result := '';
  end;

end;

function TColumnRecord.GetAsTime: TTime;
begin
  Result := FValue.TryToTime;
end;

procedure TColumnRecord.SetAsCurrency(const Value: Currency);
begin
  FValue := Value;
end;

procedure TColumnRecord.SetAsDate(const Value: TDate);
begin
  FValue := Value;
end;

procedure TColumnRecord.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Value;
end;

procedure TColumnRecord.SetAsFloat(const Value: Double);
begin
  FValue := Value;
end;

procedure TColumnRecord.SetAsInteger(const Value: Integer);
begin
  FValue := Value;
end;

procedure TColumnRecord.SetAsString(const Value: String);
begin
  FValue := Value;
end;

procedure TColumnRecord.SetAsTime(const Value: TTime);
begin
  FValue := Value;
end;

procedure TColumnRecord.SetStream(const Value: TStream);
begin
  if Assigned(FStream) then
  begin
    FreeAndNil(FStream);
  end;
  FStream := Value;
end;

{ TCursorColumn }

constructor TCursorColumn.Create(pColumnName: String; pValueTye: TColumnType; pSize: UInt16; pIndex: Smallint;
  pColumnOptions: TColumnOptions);
begin
  FColumnName := pColumnName;
  FValueTye := pValueTye;
  FSize := pSize;
  FIndex := pIndex;
  FColumnOptions := pColumnOptions;
end;

end.
