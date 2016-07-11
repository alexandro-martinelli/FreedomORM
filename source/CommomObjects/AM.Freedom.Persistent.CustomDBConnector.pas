unit AM.Freedom.Persistent.CustomDBConnector;

interface

uses
  AM.Freedom.DBPersistent.IDBConnector,
  AM.Freedom.Exceptions,
  AM.Freedom.DefaultsClassRegister,
  System.Classes,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.InterfacedObjects;

type
  TCustomDBConnector<T: TComponent> = class abstract(TInterfacedObject, IDBConnector)
  strict private type
      TDBConnectorNotification = class(TComponent)
      private
        FDBConnector: TCustomDBConnector<T>;
      protected
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      public
        constructor Create(AOwner: TComponent; pConnector: TCustomDBConnector<T>); reintroduce;
      end;
  strict private
    FConnection: T;
    FOwnsConnection: Boolean;
    FConnectorTypes: TConnectorTypes;
    FDBConnectorNotification: TDBConnectorNotification;
    FUseTransactions: Boolean;
    FDBLogTypes: TDBLogTypes;
    function GetUseTransactions: Boolean;
    procedure SetUseTransactions(const pUseTransactions: Boolean);
    function GetConnectorTypes: TConnectorTypes;
    function GetDBLogTypes: TDBLogTypes;
    procedure SetDBLogTypes(const pDBLogTypes: TDBLogTypes);
  strict protected
    procedure SetConnectorTypes(const pConnectorTypes: TConnectorTypes);
    procedure DoCommit; virtual;
    procedure DoRoolback; virtual;
    procedure DoStartTransaction; virtual;
    function DoGetInTransaction: Boolean; virtual;
  public
    constructor Create(pConnection: T; pOwnsConnection: Boolean = True; pUseTransactions: Boolean = True); virtual;
    destructor Destroy; override;
    procedure Commit;
    procedure Roolback;
    procedure StartTransaction;
    function InTransaction: Boolean; virtual;
    function NewStatement: IDBStatement; virtual;
    procedure Reconnect; virtual;
    function Connection: T;
    property ConnectorTypes: TConnectorTypes read GetConnectorTypes;
    property UseTransactions: Boolean read GetUseTransactions write SetUseTransactions;
    property DBLogTypes: TDBLogTypes read GetDBLogTypes write SetDBLogTypes;
  end;

implementation

{ TCustomDBConnector<T> }

uses
  AM.Freedom.Persistent.CustomDBStatement;

procedure TCustomDBConnector<T>.Commit;
begin
  if (UseTransactions) then
  begin
    DoCommit;
  end;
end;

constructor TCustomDBConnector<T>.Create(pConnection: T; pOwnsConnection: Boolean; pUseTransactions: Boolean);
begin
  FConnection := pConnection;
  FOwnsConnection := pOwnsConnection;
  FDBConnectorNotification := TDBConnectorNotification.Create(nil, Self);
  FConnection.FreeNotification(FDBConnectorNotification);
  FUseTransactions := pUseTransactions;
end;

destructor TCustomDBConnector<T>.Destroy;
begin
  if FOwnsConnection and Assigned(FConnection) then
  begin
    FConnection.Free;
  end;
  FConnection := nil;
  FDBConnectorNotification.Free;
  inherited;
end;

procedure TCustomDBConnector<T>.DoCommit;
begin
  raise EInvalidMethodCallOnClass.Create('DoCommit', ClassName);
end;

function TCustomDBConnector<T>.DoGetInTransaction: Boolean;
begin
  raise EInvalidMethodCallOnClass.Create('DoGetInTransaction', ClassName);
end;

procedure TCustomDBConnector<T>.DoRoolback;
begin
  raise EInvalidMethodCallOnClass.Create('DoRoolback', ClassName);
end;

procedure TCustomDBConnector<T>.DoStartTransaction;
begin
  raise EInvalidMethodCallOnClass.Create('DoStartTransaction', ClassName);
end;

function TCustomDBConnector<T>.Connection: T;
begin
  Result := FConnection;
end;

function TCustomDBConnector<T>.GetConnectorTypes: TConnectorTypes;
begin
  Result := FConnectorTypes;
end;

function TCustomDBConnector<T>.GetDBLogTypes: TDBLogTypes;
begin
  Result := FDBLogTypes;
end;

function TCustomDBConnector<T>.GetUseTransactions: Boolean;
begin
  Result := FUseTransactions;
end;

function TCustomDBConnector<T>.InTransaction: Boolean;
begin
  if (UseTransactions) then
  begin
    Result := DoGetInTransaction;
  end
  else
  begin
    Result := False;
  end;
end;

function TCustomDBConnector<T>.NewStatement: IDBStatement;
begin
  raise EInvalidMethodCallOnClass.Create('NewQuery', ClassName);
end;

procedure TCustomDBConnector<T>.Reconnect;
begin
  raise EInvalidMethodCallOnClass.Create('Reconnect', ClassName);
end;

procedure TCustomDBConnector<T>.Roolback;
begin
  if (UseTransactions) then
  begin
    DoRoolback;
  end;
end;

procedure TCustomDBConnector<T>.SetConnectorTypes(const pConnectorTypes: TConnectorTypes);
begin
  FConnectorTypes := pConnectorTypes;
end;

procedure TCustomDBConnector<T>.SetDBLogTypes(const pDBLogTypes: TDBLogTypes);
begin
  FDBLogTypes := pDBLogTypes;
end;

procedure TCustomDBConnector<T>.SetUseTransactions(const pUseTransactions: Boolean);
begin
  FUseTransactions := pUseTransactions;
end;

procedure TCustomDBConnector<T>.StartTransaction;
begin
  if (UseTransactions) then
  begin
    DoStartTransaction;
  end;
end;

{ TCustomDBConnector<T>.TDBConnectorNotification }

constructor TCustomDBConnector<T>.TDBConnectorNotification.Create(AOwner: TComponent; pConnector: TCustomDBConnector<T>);
begin
  inherited Create(AOwner);
  FDBConnector := pConnector;
end;

procedure TCustomDBConnector<T>.TDBConnectorNotification.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = TComponent(FDBConnector.FConnection)) and (Operation = opRemove) then
  begin
    TComponent(FDBConnector.FConnection).RemoveFreeNotification(Self);
    FDBConnector.FConnection := nil;
  end;
end;

end.
