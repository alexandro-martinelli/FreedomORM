unit AM.Freedom.Persistent.CustomDBStatement;

interface

uses
  Data.DB,
  System.Classes,
  AM.Freedom.DBPersistent.IDBConnector,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.DBPersistent.DBParam,
  AM.Freedom.Exceptions,
  AM.Freedom.Helper.FieldType,
  AM.Freedom.SQLMappers.ISQLMapper,
  AM.Freedom.ICursor;

type
  TCustomDBStatement<T: TDataSet, constructor> = class abstract(TInterfacedObject, IDBStatement)
  strict private
    FDataSet: T;
    FDBConnector: IDBConnector;
    procedure SetCommand(pCommand: TCustomCommand; pSQLMapper: ISQLMapper);
    procedure SetParams(pParams: TDBParams);
    function ExecuteQuery: IDBQuery;
    function ExtractQueryList: TCursorList;
    function GetTextFromDBObject(pObject: TObject; pSQLMapper: ISQLMapper): string;
    function GetDBConnector: IDBConnector;
    function GetDataSet: T;
    function LogSQLs: Boolean;
    function LogSelect: Boolean;
    function LogDML: Boolean;
    function LogDDL: Boolean;
  strict protected
    property DBConnector: IDBConnector read GetDBConnector;
    function StreamToString(pStream: TStream): string;
    function CreateDataSet: T; virtual;
    function CreateQuery: IDBQuery; virtual;
    function CreateQueryList: TCursorList; virtual;
    function Execute: Boolean; virtual;
    procedure DoSetCommand(pSQLText: string); virtual;
    procedure DoSetVariantParam(pParamName: string; pParamType: TFieldType; pParamValue: Variant); virtual;
    procedure DoSetStreamParam(pParamName: string; pParamType: TFieldType; pParamValue: TStream); virtual;
    procedure SetDataSetConnection; virtual;
  public
    constructor Create(pConnection: IDBConnector);
    destructor Destroy; override;
    property DataSet: T read GetDataSet write FDataSet;
  end;

implementation

uses
  System.Variants,
  System.StrUtils,
  CodeSiteLogging,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.TextGenerator.MasterTextGenerator,
  AM.Freedom.Helper.ColumnType,
  System.SysUtils;

{ TCustomDBStatement<T> }

constructor TCustomDBStatement<T>.Create(pConnection: IDBConnector);
begin
  FDBConnector := pConnection;
end;

function TCustomDBStatement<T>.CreateDataSet: T;
begin
  raise EInvalidMethodCallOnClass.Create('CreateDataSet', ClassName);
end;

function TCustomDBStatement<T>.CreateQuery: IDBQuery;
begin
  raise EInvalidMethodCallOnClass.Create('CreateQuery', ClassName);
end;

function TCustomDBStatement<T>.CreateQueryList: TCursorList;
begin
  raise EInvalidMethodCallOnClass.Create('CreateQueryList', ClassName);
end;

destructor TCustomDBStatement<T>.Destroy;
begin
  if (Assigned(FDataSet)) then
  begin
    FDataSet.Close;
    FDataSet.Free;
  end;
  inherited;
end;

procedure TCustomDBStatement<T>.DoSetCommand(pSQLText: string);
begin
  raise EInvalidMethodCallOnClass.Create('DoSetCommand', ClassName);
end;

procedure TCustomDBStatement<T>.DoSetStreamParam(pParamName: string; pParamType: TFieldType; pParamValue: TStream);
begin
  raise EInvalidMethodCallOnClass.Create('DoSetStreamParam', ClassName);
end;

procedure TCustomDBStatement<T>.DoSetVariantParam(pParamName: string; pParamType: TFieldType; pParamValue: Variant);
begin
  raise EInvalidMethodCallOnClass.Create('DoSetVariantParam', ClassName);
end;

function TCustomDBStatement<T>.Execute: Boolean;
begin
  raise EInvalidMethodCallOnClass.Create('Execute', ClassName);
end;

function TCustomDBStatement<T>.ExecuteQuery: IDBQuery;
begin
  Result := CreateQuery;
  Result.Open;
end;

function TCustomDBStatement<T>.ExtractQueryList: TCursorList;
begin
  Result := CreateQueryList;
end;

function TCustomDBStatement<T>.GetDataSet: T;
begin
  if not Assigned(FDataSet) then
  begin
    FDataSet := CreateDataSet;
    SetDataSetConnection;
  end;
  Result := FDataSet;
end;

function TCustomDBStatement<T>.GetDBConnector: IDBConnector;
begin
  Result := FDBConnector;
end;

function TCustomDBStatement<T>.GetTextFromDBObject(pObject: TObject; pSQLMapper: ISQLMapper): string;
var
  lTextGenerator: TMasterTextGenerator;
begin
  lTextGenerator := TMasterTextGenerator.Create(pSQLMapper);
  try
    Result := lTextGenerator.GenerateText(pObject);
  finally
    lTextGenerator.Free;
  end;
end;

function TCustomDBStatement<T>.LogDDL: Boolean;
begin
    Result := ltDDL in DBConnector.DBLogTypes;
end;

function TCustomDBStatement<T>.LogDML: Boolean;
begin
  Result := ltDML in DBConnector.DBLogTypes;
end;

function TCustomDBStatement<T>.LogSelect: Boolean;
begin
  Result := ltSelect in DBConnector.DBLogTypes;
end;

function TCustomDBStatement<T>.LogSQLs: Boolean;
begin
  Result := DBConnector.DBLogTypes <> [];
end;

procedure TCustomDBStatement<T>.SetCommand(pCommand: TCustomCommand; pSQLMapper: ISQLMapper);
var
  lSQLText: string;
begin
  lSQLText := GetTextFromDBObject(pCommand, pSQLMapper);
  if (LogSQLs) then
  begin
    if StartsText('SELECT', UpperCase(lSQLText)) then
    begin
      if (LogSelect) then
      begin
        CodeSite.Send(lSQLText);
      end;
    end
    else
    begin
      if (StartsText('INSERT', UpperCase(lSQLText)) or StartsText('UPDATE', UpperCase(lSQLText)) or
         StartsText('DELETE', UpperCase(lSQLText))) then
      begin
        if (LogDML) then
        begin
          CodeSite.Send(lSQLText);
        end;
      end
      else if (LogDDL) then
      begin
        CodeSite.Send(lSQLText);
      end;
    end;
  end;
  DoSetCommand(lSQLText);
end;

procedure TCustomDBStatement<T>.SetDataSetConnection;
begin
  raise EInvalidMethodCallOnClass.Create('SetDataSetConnection', ClassName);
end;

procedure TCustomDBStatement<T>.SetParams(pParams: TDBParams);
var
  lParam: TDBParam;
begin
  for lParam in pParams do
  begin
    if lParam.StreamParamValue <> nil then
    begin
      DoSetStreamParam(lParam.ParamName, lParam.ParamType, lParam.StreamParamValue);
    end
    else
    begin
      DoSetVariantParam(lParam.ParamName, lParam.ParamType, lParam.ParamValue)
    end;
  end;
end;

function TCustomDBStatement<T>.StreamToString(pStream: TStream): string;
var
  lStrings: TStringList;
begin
  lStrings := TStringList.Create;
  try
    pStream.Position := 0;
    lStrings.LoadFromStream(pStream);
    Result := lStrings.Text;
  finally
    lStrings.Free;
  end;
end;

end.
