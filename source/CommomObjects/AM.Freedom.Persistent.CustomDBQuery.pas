unit AM.Freedom.Persistent.CustomDBQuery;

interface

uses
  System.SysUtils,
  Data.DB,
  AM.Freedom.Exceptions,
  AM.Freedom.DBPersistent.IDBConnector,
  System.Classes;

type
  TCustomDBQuery<T: TDataSet> = class abstract(TInterfacedObject, IDBQuery)
  strict private
    FDataSet: T;
    function GetColumnCount: Integer;
    function GetRecordCount: Integer;
    function GetValueByColumnName(pColumnName: string): Variant;
    function GetValueByColumnIndex(pColumnIndex: Integer): Variant;
    function GetStreamByColumnName(pColumnName: string): TStream;
    function GetIsEof: Boolean;
    function GetIsBof: Boolean;
    function GetRecNo: UInt16;
    function GetColumns(pColumnIndex: Integer): string;
  strict protected
    property DataSet: T read FDataSet write FDataSet;
    procedure AssignQuery(pQuery: TObject); virtual;
    procedure AssignQueryList(pQuery: TObject); virtual;
    procedure Open; virtual;
  public
    destructor Destroy; override;
    procedure First;
    procedure Next;
    function IsEmpty: Boolean;
    property ColumnCount: Integer read GetColumnCount;
    property RecordCount: Integer read GetRecordCount;
    property RecNo: UInt16 read GetRecNo;
    property Eof: Boolean read GetIsEof;
    property Bof: Boolean read GetIsBof;
    property Values[pColumnName: string]: Variant read GetValueByColumnName; default;
    property Values[pColumnIndex: Integer]: Variant read GetValueByColumnIndex; default;
    property Columns[pColumnIndex: Integer]: string read GetColumns;
  end;

implementation

{ TCustomDBQuery<T> }

procedure TCustomDBQuery<T>.AssignQueryList(pQuery: TObject);
begin
  raise EInvalidMethodCallOnClass.Create('AssignQueryList', ClassName);
end;

procedure TCustomDBQuery<T>.AssignQuery(pQuery: TObject);
begin
  FDataSet := T(pQuery);
end;

destructor TCustomDBQuery<T>.Destroy;
begin
  FreeAndNil(FDataSet);
  inherited;
end;

procedure TCustomDBQuery<T>.First;
begin
  FDataSet.First;
end;

function TCustomDBQuery<T>.GetColumnCount: Integer;
begin
  Result := FDataSet.FieldCount;
end;

function TCustomDBQuery<T>.GetColumns(pColumnIndex: Integer): string;
begin
  if (pColumnIndex < 0) or (pColumnIndex > FDataSet.FieldCount) then
  begin
    raise EInvalidCursorColumnIndex.Create(pColumnIndex);
  end
  else
  begin
    Result := FDataSet.Fields[pColumnIndex].FieldName;
  end;
end;

function TCustomDBQuery<T>.GetIsBof: Boolean;
begin
  Result := FDataSet.Bof;
end;

function TCustomDBQuery<T>.GetIsEof: Boolean;
begin
  Result := FDataSet.Eof;
end;

function TCustomDBQuery<T>.GetRecNo: UInt16;
begin
  Result := FDataSet.RecNo
end;

function TCustomDBQuery<T>.GetRecordCount: Integer;
begin
  Result := FDataSet.RecordCount;
end;

function TCustomDBQuery<T>.GetStreamByColumnName(pColumnName: string): TStream;
begin
  case FDataSet.FieldByName(pColumnName).DataType of
    ftBlob, ftGraphic, ftOraBlob, ftOraClob: Result := TMemoryStream.Create;
    ftMemo,  ftFmtMemo, ftWideMemo: Result := TStringStream.Create;
    else
      raise EInvalidBlobFieldTypeToStream.Create(FieldTypeNames[FDataSet.FieldByName(pColumnName).DataType], pColumnName);
  end;
  TBlobField(FDataSet.FieldByName(pColumnName)).SaveToStream(Result);
  Result.Position := 0;
end;

function TCustomDBQuery<T>.GetValueByColumnIndex(pColumnIndex: Integer): Variant;
begin
  Result := FDataSet.Fields[pColumnIndex].Value;
end;

function TCustomDBQuery<T>.GetValueByColumnName(pColumnName: string): Variant;
begin
  Result := FDataSet.FieldByName(pColumnName).Value;
end;

function TCustomDBQuery<T>.IsEmpty: Boolean;
begin
  Result := FDataSet.IsEmpty;
end;

procedure TCustomDBQuery<T>.Next;
begin
  FDataSet.Next;
end;

procedure TCustomDBQuery<T>.Open;
begin
  FDataSet.Open;
end;

end.
