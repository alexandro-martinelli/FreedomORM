unit AM.Freedom.dmConnection;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.ODBCBase,
  FireDAC.Phys.MSSQL,
  FireDAC.Phys.PG,
  FireDAC.Phys.IBBase,
  FireDAC.Phys.FB,
  Data.DB,
  FireDAC.Comp.Client,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.DBPersistent.FireDac.PropertyNames,
  Vcl.StdCtrls,
  FireDAC.VCLUI.Wait,
  FireDAC.Comp.UI,
  Vcl.Graphics,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  FireDAC.Comp.DataSet,
  AM.Freedom.TableSchemas, FireDAC.Phys.MSSQLDef, FireDAC.Phys.FBDef, FireDAC.Phys.PGDef;

type
  TdmConnection = class(TDataModule)
    FDGUIxWaitCursor: TFDGUIxWaitCursor;
    FDConnection: TFDConnection;
    QryTableSchemas: TFDQuery;
    FDPhysFBDriverLink: TFDPhysFBDriverLink;
    FDPhysMSSQLDriverLink: TFDPhysMSSQLDriverLink;
    FDPhysPgDriverLink: TFDPhysPgDriverLink;
  strict private
    procedure GetParams;
  private
    FConnectionParams: TStrings;
    procedure DoTestConnection(pDataBaseType: TConnectorType; pConnectionParams: TStrings; plblTestConnection: TLabel);
    function DoGetTablesFromDataBaseType(pDataBaseType: TConnectorType; pConnectionParams: TStrings): TTableSchemaList;
    procedure GetQrySQLForTableSchemas(pDataBaseType: TConnectorType);
    procedure DoSetParams(pDataBaseType: TConnectorType; pConnectionParams: TStrings);
  public
    class procedure TestConnection(pDataBaseType: TConnectorType; pConnectionParams: TStrings; plblTestConnection: TLabel);
    class function GetTablesFromDataBaseType(pDataBaseType: TConnectorType; pConnectionParams: TStrings): TTableSchemaList;
  end;

implementation

uses
  Vcl.Forms;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TdmConnection }

procedure TdmConnection.GetParams;
var
  lIndex: Integer;
begin
  for lIndex := 0 to FConnectionParams.Count - 1 do
  begin
    if (FConnectionParams.ValueFromIndex[lIndex] <> ' ') then
    begin
      FDConnection.Params.Values[FConnectionParams.Names[lIndex]] := FConnectionParams.ValueFromIndex[lIndex];
    end;
  end;
end;

procedure TdmConnection.GetQrySQLForTableSchemas(pDataBaseType: TConnectorType);
begin
  QryTableSchemas.Close;
  QryTableSchemas.SQL.Clear;
  case pDataBaseType of
    Firebird: QryTableSchemas.SQL.Text := 'select RL.RDB$RELATION_NAME as TABLE_NAME, '''' as SCHEMA_NAME' +
        ' from RDB$RELATIONS RL where RL.RDB$RELATION_TYPE = 0' +
        ' and coalesce(RL.RDB$SYSTEM_FLAG, 0) = 0' +
        ' order by 1';
    SQLServer: QryTableSchemas.SQL.Text := 'select TAB.TABLE_NAME, TAB.TABLE_SCHEMA' +
        ' from INFORMATION_SCHEMA.TABLES TAB' +
        ' order by TAB.TABLE_SCHEMA, TAB.TABLE_NAME';
    PostGree: QryTableSchemas.SQL.Text := 'select TAB.TABLE_NAME, TAB.TABLE_SCHEMA' +
        ' from INFORMATION_SCHEMA.TABLES TAB' +
        ' where TAB.TABLE_SCHEMA not in (''information_schema'', ''pg_catalog'')' +
        ' order by TAB.TABLE_SCHEMA, TAB.TABLE_NAME';
    Oracle: ;
    MySQL: ;
    Interbase: ;
  end;
end;

class function TdmConnection.GetTablesFromDataBaseType(pDataBaseType: TConnectorType; pConnectionParams: TStrings): TTableSchemaList;
var
  ldmConnection: TdmConnection;
begin
  ldmConnection := TdmConnection.Create(nil);
  try
    Result := ldmConnection.DoGetTablesFromDataBaseType(pDataBaseType, pConnectionParams);
  finally
    ldmConnection.Free;
  end;
end;

class procedure TdmConnection.TestConnection(pDataBaseType: TConnectorType; pConnectionParams: TStrings;
  plblTestConnection: TLabel);
var
  ldmConnection: TdmConnection;
begin
  ldmConnection := TdmConnection.Create(nil);
  try
    ldmConnection.DoTestConnection(pDataBaseType, pConnectionParams, plblTestConnection);
  finally
    ldmConnection.Free;
  end;
end;

function TdmConnection.DoGetTablesFromDataBaseType(pDataBaseType: TConnectorType;
  pConnectionParams: TStrings): TTableSchemaList;
begin
  Result := TTableSchemaList.Create;
  DoSetParams(pDataBaseType, pConnectionParams);
  GetQrySQLForTableSchemas(pDataBaseType);
  QryTableSchemas.Open;
  QryTableSchemas.First;
  while not QryTableSchemas.Eof do begin
    Result.AddTableSchema(QryTableSchemas.Fields[0].AsString,  QryTableSchemas.Fields[1].AsString);
    QryTableSchemas.Next;
  end;
  QryTableSchemas.Close;
end;

procedure TdmConnection.DoSetParams(pDataBaseType: TConnectorType; pConnectionParams: TStrings);
begin
  FConnectionParams := pConnectionParams;
  case pDataBaseType of
    Firebird:
      begin
        FDConnection.DriverName := 'FB';
      end;
    SQLServer:
      begin
        FDConnection.DriverName := 'MSSQL';
      end;
    PostGree:
      begin
        FDConnection.DriverName := 'PG';
      end;
    Oracle:
      begin
        FDConnection.DriverName := 'Ora';
      end;
    MySQL:
      begin
        FDConnection.DriverName := 'MySQL';
      end;
    Interbase:
      begin
        FDConnection.DriverName := 'IB';
      end;
  end;
  GetParams;
end;

procedure TdmConnection.DoTestConnection(pDataBaseType: TConnectorType; pConnectionParams: TStrings;
  plblTestConnection: TLabel);
begin
  DoSetParams(pDataBaseType, pConnectionParams);
  try
    FDConnection.Open;
    plblTestConnection.Caption := 'Connection tested sucessfully!';
    plblTestConnection.Font.Color := clBlue;
  except
    on E:Exception do
    begin
      Application.MessageBox(PWideChar('Connection error!' + sLineBreak + sLineBreak + E.Message), PWideChar('Freedom DB Object'),
          MB_ICONERROR + MB_OK + MB_DEFBUTTON1);
      plblTestConnection.Caption := 'Connection error!';
      plblTestConnection.Font.Color := clRed;
    end;
  end;
  FDConnection.Close;
end;

end.
