unit AM.Freedom.IDBPersistent;

interface

uses
  AM.Freedom.IPersistent,
  AM.Freedom.ObjectMapper.DDLObjects,
  AM.Freedom.ObjectMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.DBPersistent.IDBConnector,
  AM.Freedom.SQLMappers.ISQLMapper,
  AM.Freedom.SQLMappers.CustomSelect,
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.SQLMapper.CustomArgument;

type
  IDBPersistent = interface(IPersistent)
  ['{EA7F4595-2922-412F-9339-7835A7CF4279}']
    function ExtractDBDDL(pObjectMapper: TObjectMapper; pDDLOptions: TDDLOptions = [Fields, Constraints]): TDDLEntity; overload;
    function ExtractDBDDL(pName: String; pSchemaName: String; pDDLOptions: TDDLOptions = [Fields, Constraints]): TObjectMapper; overload;
    function NewDBStatement: IDBStatement;
    function GetDBConnector: IDBConnector;
    function GetSQLMapper: ISQLMapper;
    function SupportedCommand(pCommand: TCustomCommand): Boolean;
    function CompareDDLs(pSourceDDL, pCompareDDL: TDDLEntity): TCommandList;
    function GetMakeForeignConstraintsWithJoinColumn: Boolean;
    procedure SetMakeForeignConstraintsWithJoinColumn(const Value: Boolean);
    procedure Commit;
    procedure Roolback;
    procedure StartTransaction;
    function InTransaction: Boolean;
    procedure Reconnect;
    property MakeForeignConstraintsWithJoinColumn: Boolean read GetMakeForeignConstraintsWithJoinColumn write SetMakeForeignConstraintsWithJoinColumn;
  end;

implementation

end.
