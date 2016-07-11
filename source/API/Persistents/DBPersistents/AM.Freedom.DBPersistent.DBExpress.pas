unit AM.Freedom.DBPersistent.DBExpress;

interface

uses
  Data.SqlExpr,
  Data.DBXCommon,
  Data.DbxDb2,
  Data.DbxFirebird,
  Data.DbxInformix,
  Data.DBXInterbase,
  Data.DBXJSON,
  Data.DbxMSSQL,
  Data.DbxMySql,
  Data.DBXOdbc,
  Data.DbxOracle,
  Data.DbxSybaseASA,
  Data.DbxSybaseASE,
  Data.DB,
  System.Classes,
  AM.Freedom.Persistent.CustomDBConnector,
  AM.Freedom.DBPersistent.IDBConnector,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Persistent.CustomDBStatement,
  AM.Freedom.Persistent.CustomDBQuery;


type
  TDBXDBQuery = class(TCustomDBQuery<TSQLQuery>);

  TDBXStatement = class(TCustomDBStatement<TSQLQuery>)
  strict protected
    function CreateDataSet: TSQLQuery; override;
    function CreateQuery: IDBQuery; override;

    function Execute: Boolean; override;
    procedure DoSetCommand(pSQLText: String); override;
    procedure DoSetVariantParam(pParamName: String; pParamType: TFieldType; pParamValue: Variant); override;
    procedure DoSetStreamParam(pParamName: String; pParamType: TFieldType; pParamValue: TStream); override;
    procedure SetDataSetConnection; override;
  end;

  TDBXConnector = class(TCustomDBConnector<TSQLConnection>)
  strict private
    FDBXTransaction: TDBXTransaction;
  strict protected
    procedure DoRoolback; override;
    procedure DoStartTransaction; override;
    function DoGetInTransaction: Boolean; override;
    procedure DoCommit; override;
  public
    constructor Create(pConnection: TSQLConnection; pOwnsConnection: Boolean = True; pUseTransactions: Boolean = True); override;
    function NewStatement: IDBStatement; override;
    procedure Reconnect; override;
  end;

implementation

uses
  System.SysUtils;

{ TDBExpressQuery }

function TDBXStatement.CreateDataSet: TSQLQuery;
begin
  Result := TSQLQuery.Create(TDBXConnector(DBConnector).Connection);
end;

function TDBXStatement.CreateQuery: IDBQuery;
begin
  Result := TDBXDBQuery.Create;
  Result.AssignQuery(DataSet);
  DataSet := nil;
end;

procedure TDBXStatement.DoSetCommand(pSQLText: String);
begin
  DataSet.SQL.Text := pSQLText;
end;

procedure TDBXStatement.DoSetStreamParam(pParamName: String; pParamType: TFieldType; pParamValue: TStream);
var
  lParam: TParam;
  lMemo: String;
begin
  lParam := DataSet.ParamByName(pParamName);
  if Assigned(lParam) then
  begin
    lParam.DataType := pParamType;
    if pParamType = ftMemo then
    begin
      lMemo := StreamToString(pParamValue);
      lParam.AsMemo := lMemo;
    end
    else
    begin
      pParamValue.Position := 0;
      lParam.LoadFromStream(pParamValue, pParamType);
    end;
  end;
end;

procedure TDBXStatement.DoSetVariantParam(pParamName: String; pParamType: TFieldType; pParamValue: Variant);
var
  lParam: TParam;
begin
  lParam := DataSet.ParamByName(pParamName);
  if Assigned(lParam) then
  begin
    lParam.DataType := pParamType;
    lParam.Value := pParamValue;
  end;
end;

function TDBXStatement.Execute: Boolean;
begin
  DataSet.ExecSQL;
  Result := True;
end;
procedure TDBXStatement.SetDataSetConnection;
begin
  DataSet.SQLConnection := TDBXConnector(DBConnector).Connection;
end;

{ TDBExpressConnector }

procedure TDBXConnector.DoCommit;
begin
  if InTransaction then
  begin
    Connection.CommitFreeAndNil(FDBXTransaction);
  end;
end;

constructor TDBXConnector.Create(pConnection: TSQLConnection; pOwnsConnection: Boolean; pUseTransactions: Boolean);
begin
  inherited Create(pConnection, pOwnsConnection, pUseTransactions);
  SetConnectorTypes([Firebird, SQLServer, Oracle, MySQL, Interbase, DataSnap]);
end;

function TDBXConnector.DoGetInTransaction: Boolean;
begin
  Result := Assigned(FDBXTransaction);
end;

function TDBXConnector.NewStatement: IDBStatement;
begin
  Result := TDBXStatement.Create(Self);
end;

procedure TDBXConnector.Reconnect;
begin
  if not InTransaction then
  begin
    Connection.Close;
    Connection.Open;
  end;
end;

procedure TDBXConnector.DoRoolback;
begin
  if InTransaction then
  begin
    Connection.RollbackFreeAndNil(FDBXTransaction);
  end;
end;

procedure TDBXConnector.DoStartTransaction;
begin
  if not InTransaction then
  begin
    FDBXTransaction := Connection.BeginTransaction;
  end;
end;

end.
