unit AM.Freedom.Persistent.CustomDSCursor;

interface

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.Persistent.IDSConnector;

type
  TCustomDSCursor<T: class> = class abstract(TInterfacedObject, IDSCursor)
  strict private
    FObject: T;
    procedure Open;
  strict protected
    function GetColumnCount: Integer; virtual;
    function GetRecordCount: Integer; virtual;
    function GetValueByColumnName(pColumnName: string): Variant; virtual;
    function GetValueByColumnIndex(pColumnIndex: Integer): Variant; virtual;
    function GetStreamByColumnName(pColumnName: string): TStream; virtual;
    function GetIsEof: Boolean; virtual;
    function GetIsBof: Boolean; virtual;
    function GetRecNo: UInt16; virtual;
    function GetColumns(pColumnIndex: Integer): string; virtual;
    procedure AssignQuery(pObject: TObject);
    procedure DoAssignQuery; virtual;
    procedure AssignQueryList(pObject: TObject);
    property InternalObject: T read FObject write FObject;
  public
    procedure First; virtual;
    procedure Next; virtual;
    function IsEmpty: Boolean; virtual;
    function IsNotEmpty: Boolean;
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

uses
  AM.Freedom.Exceptions;

{ TCustomDSCursor<T> }

function TCustomDSCursor<T>.GetColumnCount: Integer;
begin
  raise EInvalidMethodCallOnClass.Create('GetColumnCount', ClassName);
end;

function TCustomDSCursor<T>.GetRecordCount: Integer;
begin
  raise EInvalidMethodCallOnClass.Create('GetRecordCount', ClassName);
end;

function TCustomDSCursor<T>.GetValueByColumnName(pColumnName: string): Variant;
begin
  raise EInvalidMethodCallOnClass.Create('GetValueByColumnName', ClassName);
end;

function TCustomDSCursor<T>.GetValueByColumnIndex(pColumnIndex: Integer): Variant;
begin
  raise EInvalidMethodCallOnClass.Create('GetValueByColumnIndex', ClassName);
end;

function TCustomDSCursor<T>.GetStreamByColumnName(pColumnName: string): TStream;
begin
  raise EInvalidMethodCallOnClass.Create('GetStreamByColumnName', ClassName);
end;

function TCustomDSCursor<T>.GetIsEof: Boolean;
begin
  raise EInvalidMethodCallOnClass.Create('GetIsEof', ClassName);
end;

function TCustomDSCursor<T>.GetIsBof: Boolean;
begin
  raise EInvalidMethodCallOnClass.Create('GetIsBof', ClassName);
end;

function TCustomDSCursor<T>.GetRecNo: UInt16;
begin
  raise EInvalidMethodCallOnClass.Create('GetRecNo', ClassName);
end;

function TCustomDSCursor<T>.GetColumns(pColumnIndex: Integer): string;
begin
  raise EInvalidMethodCallOnClass.Create('GetColumns', ClassName);
end;

procedure TCustomDSCursor<T>.AssignQuery(pObject: TObject);
begin
  FObject := T(pObject);
  DoAssignQuery;
end;

procedure TCustomDSCursor<T>.AssignQueryList(pObject: TObject);
begin
  FObject := T(pObject);
  DoAssignQuery;
end;

procedure TCustomDSCursor<T>.First;
begin
  raise EInvalidMethodCallOnClass.Create('First', ClassName);
end;

procedure TCustomDSCursor<T>.Next;
begin
  raise EInvalidMethodCallOnClass.Create('Next', ClassName);
end;

function TCustomDSCursor<T>.IsEmpty: Boolean;
begin
  raise EInvalidMethodCallOnClass.Create('IsEmpty', ClassName);
end;

function TCustomDSCursor<T>.IsNotEmpty: Boolean;
begin
  Result := not IsEmpty;
end;

procedure TCustomDSCursor<T>.DoAssignQuery;
begin
  raise EInvalidMethodCallOnClass.Create('DoAssignQuery', ClassName);
end;

procedure TCustomDSCursor<T>.Open;
begin
  // do nothing;
end;

end.
