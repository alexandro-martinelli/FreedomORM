unit AM.Freedom.InterfacedObjects;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  AM.Freedom.INotificator;

type
  TFreedomInterfacedObject = class(TObject, IInterface, INotificator)
  strict private
    FNotifiers: TList<INotificator>;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateFreeNotifiers;
  protected
    procedure RegisterFreeNotification(pNotificator: INotificator); virtual;
    procedure SendFreeNotification; virtual;
    procedure ReceiveFreeNotification(pNotificator: INotificator); virtual;
    procedure RemoveNotificator(pNotificator: INotificator); virtual;
  public
    destructor Destroy; override;
  end;

  TInterfacedObjectList<T: class> = class(TObjectList<T>, IInterface, INotificator)
  strict private
    FNotifiers: TList<INotificator>;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateFreeNotifiers;
  protected
    procedure RegisterFreeNotification(pNotificator: INotificator); virtual;
    procedure SendFreeNotification; virtual;
    procedure ReceiveFreeNotification(pNotificator: INotificator); virtual;
    procedure RemoveNotificator(pNotificator: INotificator); virtual;
  public
    destructor Destroy; override;
  end;

implementation

uses
  AM.Freedom.Exceptions;

{ TFreedomInterfacedObject }

procedure TFreedomInterfacedObject.CreateFreeNotifiers;
begin
  if (not Assigned(FNotifiers)) then
  begin
    FNotifiers := TList<INotificator>.Create;
  end;
end;

destructor TFreedomInterfacedObject.Destroy;
begin
  SendFreeNotification;
  FreeAndNil(FNotifiers);
  inherited;
end;

procedure TFreedomInterfacedObject.ReceiveFreeNotification(pNotificator: INotificator);
begin
  CreateFreeNotifiers;
  if (FNotifiers.Contains(pNotificator)) then
  begin
    FNotifiers.Extract(pNotificator);
  end;
end;

function TFreedomInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TFreedomInterfacedObject.RegisterFreeNotification(pNotificator: INotificator);
begin
  CreateFreeNotifiers;
  if (not FNotifiers.Contains(pNotificator)) then
  begin
    FNotifiers.Add(pNotificator);
  end;
end;

procedure TFreedomInterfacedObject.SendFreeNotification;
var
  lNotificator: INotificator;
begin
  CreateFreeNotifiers;
  for lNotificator in FNotifiers do
  begin
    lNotificator.ReceiveFreeNotification(Self);
  end;
end;

function TFreedomInterfacedObject._AddRef: Integer;
begin
  Result := 0;
end;

function TFreedomInterfacedObject._Release: Integer;
begin
  Result := 0;
end;

procedure TFreedomInterfacedObject.RemoveNotificator(pNotificator: INotificator);
begin
  if (FNotifiers.Contains(pNotificator)) then
  begin
    FNotifiers.Extract(pNotificator);
  end;
end;

{ TInterfacedObjectList<T> }

procedure TInterfacedObjectList<T>.CreateFreeNotifiers;
begin
  if (not Assigned(FNotifiers)) then
  begin
    FNotifiers := TList<INotificator>.Create;
  end;
end;

procedure TInterfacedObjectList<T>.ReceiveFreeNotification(pNotificator: INotificator);
var
  lObject: T;
  lNotificator: INotificator;
  lIndex: Integer;
begin
  for lIndex := Count - 1  downto 0 do
  begin
    lObject := Items[lIndex];
    if (Supports(lObject, INotificator, lNotificator)) then
    begin
      if (lNotificator = pNotificator) then
      begin
        Extract(lObject);
        Break;
      end;
    end;
  end;
  if Assigned(FNotifiers) and (FNotifiers.Contains(pNotificator)) then
  begin
    FNotifiers.Extract(pNotificator);
  end;
end;

destructor TInterfacedObjectList<T>.Destroy;
begin
  SendFreeNotification;
  try
    FreeAndNil(FNotifiers);
  except
  end;
  inherited;
end;

function TInterfacedObjectList<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TInterfacedObjectList<T>.RegisterFreeNotification(pNotificator: INotificator);
begin
  if (pNotificator <> (Self as INotificator)) then
  begin
    CreateFreeNotifiers;
    if (not FNotifiers.Contains(pNotificator)) then
    begin
      FNotifiers.Add(pNotificator);
    end;
  end;
end;

procedure TInterfacedObjectList<T>.SendFreeNotification;
var
  lNotificator: INotificator;
begin
  CreateFreeNotifiers;
  while FNotifiers.Count > 0 do
  begin
    lNotificator := FNotifiers.Extract(FNotifiers.Items[0]);
    lNotificator.ReceiveFreeNotification(Self);
  end;
end;

function TInterfacedObjectList<T>._AddRef: Integer;
begin
  Result := 0;
end;

function TInterfacedObjectList<T>._Release: Integer;
begin
  Result := 0;
end;

procedure TInterfacedObjectList<T>.RemoveNotificator(pNotificator: INotificator);
begin
  if (FNotifiers.Contains(pNotificator)) then
  begin
    FNotifiers.Extract(pNotificator);
  end;
end;

end.
