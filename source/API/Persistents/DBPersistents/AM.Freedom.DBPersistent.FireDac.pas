unit AM.Freedom.DBPersistent.FireDac;

interface

uses
  Data.DB,
  System.Classes,
  FireDac.Stan.Intf,
  FireDac.Stan.Option,
  FireDac.Stan.Error,
  FireDac.UI.Intf,
  FireDac.Stan.Def,
  FireDac.Stan.Pool,
  FireDac.Stan.Async,
  FireDAC.Comp.DataSet,
  FireDac.Comp.Client,
  FireDac.DApt,
  FireDac.Comp.UI,
  FireDac.Phys.TDBXBase,
  FireDAC.Phys,
  {$IFDEF VER260}
  FireDac.Phys.DataSnap,
  {$ELSE}
  IPPeerClient,
  FireDAC.Phys.DSDef,
  FireDAC.Phys.DS,
  {$IFEND}
  FireDac.Phys.MSSQL,
  FireDac.Phys.DB2,
  FireDac.Phys.Oracle,
  FireDac.Phys.SQLite,
  FireDac.Phys.IB,
  FireDac.Phys.PG,
  FireDac.Phys.IBBase,
  FireDac.Phys.FB,
  FireDac.Phys.ADS,
  FireDac.Phys.ASA,
  FireDac.Phys.MySQL,
  FireDac.Phys.MSAcc,
  FireDac.Phys.ODBCBase,
  FireDac.Phys.ODBC,
  {$IFDEF MSWINDOWS}
  FireDac.VCLUI.Wait,
  {$ELSE}
  {$IFDEF FMX}
  FireDac.FMXUI.Wait,
  {$ENDIF}
  {$ENDIF}
  AM.Freedom.Persistent.CustomDBStatement,
  AM.Freedom.Persistent.CustomDBConnector,
  AM.Freedom.DBPersistent.IDBConnector,
  AM.Freedom.Persistent.CustomDBQuery,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ICursor;

type
  TFireDacDBQuery = class(TCustomDBQuery<TFDDataSet>)
  strict protected
    procedure AssignQuery(pQuery: TObject); override;
    procedure AssignQueryList(pQuery: TObject); override;
    procedure Open; override;
  public
    destructor Destroy; override;
  end;

  TFireDacDBStatement = class(TCustomDBStatement<TFDQuery>)
  strict private
    procedure UpdateOptionsIfSelect;
  strict protected
    function Execute: Boolean; override;
    function CreateQuery: IDBQuery; override;
    procedure DoSetCommand(pSQLText: string); override;
    procedure DoSetVariantParam(pParamName: string; pParamType: TFieldType; pParamValue: Variant); override;
    procedure DoSetStreamParam(pParamName: string; pParamType: TFieldType; pParamValue: TStream); override;
    procedure SetDataSetConnection; override;
    function CreateDataSet: TFDQuery; override;
    function CreateQueryList: TCursorList; override;
  end;

  TFireDacDBConnector = class(TCustomDBConnector<TFDConnection>)
  strict private
    FWaitCursor: TFDGUIxWaitCursor;
  strict protected
    procedure DoCommit; override;
    procedure DoRoolback; override;
    procedure DoStartTransaction; override;
    function DoGetInTransaction: Boolean; override;
  public
    constructor Create(pConnection: TFDConnection; pOwnsConnection: Boolean = True; pUseTransactions: Boolean = True); override;
    function NewStatement: IDBStatement; override;
    procedure Reconnect; override;
  end;

implementation

uses
  FireDac.Stan.Param,
  System.StrUtils,
  System.SysUtils,
  AM.Freedom.Exceptions;

{ TFireDacDBStatement }

function TFireDacDBStatement.CreateDataSet: TFDQuery;
begin
  Result := TFDQuery.Create(TFireDacDBConnector(DBConnector).Connection);
end;

function TFireDacDBStatement.CreateQuery: IDBQuery;
begin
  Result := TFireDacDBQuery.Create;
  try
    UpdateOptionsIfSelect;
    Result.AssignQuery(DataSet);
  finally
    DataSet := nil;
  end;
end;

function TFireDacDBStatement.CreateQueryList: TCursorList;
var
  lDBQuery: IDBQuery;
begin
  Result := TCursorList.Create;
  try
    DataSet.FetchOptions.Unidirectional := True;
    DataSet.ObjectView := False;
    DataSet.FetchOptions.LiveWindowParanoic := True;
    DataSet.FetchOptions.LiveWindowFastFirst := True;
    DataSet.FetchOptions.RecordCountMode := cmTotal;
    DataSet.FetchOptions.AutoClose := False;
    DataSet.FetchOptions.RowsetSize := 1000;
    DataSet.Open;
    repeat
      lDBQuery := TFireDacDBQuery.Create;
      lDBQuery.AssignQueryList(DataSet);
      Result.Add(lDBQuery);
    until DataSet.FetchNext <= 0;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TFireDacDBStatement.DoSetCommand(pSQLText: string);
begin
  DataSet.SQL.Text := pSQLText;
end;

procedure TFireDacDBStatement.DoSetStreamParam(pParamName: string; pParamType: TFieldType; pParamValue: TStream);
var
  lParam: TFDParam;
  lMemo: string;
begin
  lParam := DataSet.ParamByName(pParamName);
  if Assigned(lParam) then
  begin
    lParam.DataType := pParamType;
    if (pParamType = ftMemo) or (pParamType = ftWideMemo) then
    begin
      lMemo := StreamToString(pParamValue);
      lParam.AsWideMemo := lMemo;
    end
    else
    begin
      pParamValue.Position := 0;
      lParam.LoadFromStream(pParamValue, pParamType);
    end;
  end;
end;

procedure TFireDacDBStatement.DoSetVariantParam(pParamName: string; pParamType: TFieldType; pParamValue: Variant);
var
  lParam: TFDParam;
begin
  lParam := DataSet.ParamByName(pParamName);
  if Assigned(lParam) then
  begin
    lParam.DataType := pParamType;
    lParam.Value := pParamValue;
  end;
end;

function TFireDacDBStatement.Execute: Boolean;
begin
  DataSet.ExecSQL;
  Result := True;
end;

procedure TFireDacDBStatement.SetDataSetConnection;
begin
  DataSet.Connection := TFireDacDBConnector(DBConnector).Connection;
end;

procedure TFireDacDBStatement.UpdateOptionsIfSelect;
begin
  if (StartsText('SELECT', DataSet.SQL.Text)) then
  begin
    DataSet.FetchOptions.Unidirectional := True;
    DataSet.ObjectView := False;
    DataSet.FetchOptions.LiveWindowParanoic := True;
    DataSet.FetchOptions.LiveWindowFastFirst := True;
    DataSet.FetchOptions.RecordCountMode := cmTotal;
  end;
end;

{ TFireDacDBConnector }

procedure TFireDacDBConnector.DoCommit;
begin
  if InTransaction then
  begin
    Connection.Commit;
  end;
end;

constructor TFireDacDBConnector.Create(pConnection: TFDConnection; pOwnsConnection: Boolean; pUseTransactions: Boolean);
begin
  inherited Create(pConnection, pOwnsConnection, pUseTransactions);
  FWaitCursor := TFDGUIxWaitCursor.Create(pConnection);
  FWaitCursor.ScreenCursor := TFDGUIxScreenCursor.gcrNone;
  SetConnectorTypes([Firebird, SQLServer, Oracle, MySQL, Interbase, DataSnap, PostGree]);
end;

function TFireDacDBConnector.DoGetInTransaction: Boolean;
begin
  Result := False;
  if Connection <> nil then
  begin
    Result := Connection.InTransaction;
  end;
end;

function TFireDacDBConnector.NewStatement: IDBStatement;
begin
  Result := TFireDacDBStatement.Create(Self);
end;

procedure TFireDacDBConnector.Reconnect;
begin
  if Connection <> nil then
  begin
    Connection.Close;
    Connection.Open;
  end;
end;

procedure TFireDacDBConnector.DoRoolback;
begin
  if InTransaction then
  begin
    Connection.Rollback;
  end;
end;

procedure TFireDacDBConnector.DoStartTransaction;
begin
  Roolback;
  if Connection <> nil then
  begin
    Connection.StartTransaction;
  end;
end;

{ TFireDacDBQuery }

procedure TFireDacDBQuery.AssignQueryList(pQuery: TObject);
begin
  DataSet := TFDMemTable.Create(nil);
  DataSet.Data := TFDQuery(pQuery).Data;
end;

destructor TFireDacDBQuery.Destroy;
begin
  DataSet.Close;
  inherited;
end;

procedure TFireDacDBQuery.AssignQuery(pQuery: TObject);
begin
  DataSet := TFDQuery(pQuery);
  DataSet.Open;
end;

procedure TFireDacDBQuery.Open;
begin
  // do nothing
end;

end.
