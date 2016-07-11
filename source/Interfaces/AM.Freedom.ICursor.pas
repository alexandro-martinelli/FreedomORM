unit AM.Freedom.ICursor;

interface

uses
  System.Generics.Collections,
  System.Classes;

type
  ICursor = interface;

  TCursorList = class(TList<ICursor>);

  ICursor = interface
    ['{882BC4C6-7600-4E1B-9B70-42CEC3435525}']
    function GetColumnCount: Integer;
    function GetRecordCount: Integer;
    function GetValueByColumnName(pColumnName: string): Variant;
    function GetValueByColumnIndex(pColumnIndex: Integer): Variant;
    function GetStreamByColumnName(pColumnName: string): TStream;
    function GetIsEof: Boolean;
    function GetIsBof: Boolean;
    function GetRecNo: UInt16;
    procedure AssignQuery(pQuery: TObject);
    procedure AssignQueryList(pQuery: TObject);
    function GetColumns(pColumnIndex: Integer): string;
    procedure Open;
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
    property StreamValue[pColumnName: string]: TStream read GetStreamByColumnName;
    property Columns[pColumnIndex: Integer]: string read GetColumns;
  end;

implementation

end.
