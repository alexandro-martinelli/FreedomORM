unit AM.Freedom.DBPersistent.IDBConnector;

interface

uses
  System.Generics.Collections,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLMappers.ISQLMapper,
  AM.Freedom.ICursor,
  AM.Freedom.DBPersistent.DBParam;

type
  IDBQuery = interface(ICursor)
    ['{1998D1A7-6724-4714-A307-334D6C7759BB}']
  end;

  IDBStatement = interface
    ['{2FA321D3-1DE1-4851-B4D5-97D4FC2DD9D6}']
    procedure SetCommand(pCommand: TCustomCommand; pSQLMapper: ISQLMapper);
    procedure SetParams(pParams: TDBParams);
    function ExecuteQuery: IDBQuery;
    function ExtractQueryList: TCursorList;
    function Execute: Boolean;
  end;

  IDBConnector = interface
    ['{092DAD09-0212-4BE9-864E-EB24B17A413A}']
    function GetConnectorTypes: TConnectorTypes;
    procedure SetConnectorTypes(const pConnectorTypes: TConnectorTypes);
    function GetUseTransactions: Boolean;
    procedure SetUseTransactions(const pUseTransactions: Boolean);
    function GetDBLogTypes: TDBLogTypes;
    procedure SetDBLogTypes(const pDBLogTypes: TDBLogTypes);

    procedure Commit;
    procedure Roolback;
    procedure StartTransaction;
    function InTransaction: Boolean;
    function NewStatement: IDBStatement;
    procedure Reconnect;
    property ConnectorTypes: TConnectorTypes read GetConnectorTypes;
    property UseTransactions: Boolean read GetUseTransactions write SetUseTransactions;
    property DBLogTypes: TDBLogTypes read GetDBLogTypes write SetDBLogTypes;
  end;

implementation

end.
