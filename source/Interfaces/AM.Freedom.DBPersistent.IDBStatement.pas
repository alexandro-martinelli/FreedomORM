unit AM.Freedom.DBPersistent.IDBStatement;

interface

uses
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.DBPersistent.DBParam,
  AM.Freedom.SQLMappers.ISQLMapper,
  AM.Freedom.DBPersistent.IDBQuery;

type
  IDBStatement = interface
    ['{2FA321D3-1DE1-4851-B4D5-97D4FC2DD9D6}']
    procedure SetCommand(pCommand: TCustomCommand; pSQLMapper: ISQLMapper);
    procedure SetParams(pParams: TDBParams);
    function ExecuteQuery: TPersistentCursor;
//    function ExecuteQuery: IDBQuery;
    function Execute: Boolean;
  end;

implementation

end.
