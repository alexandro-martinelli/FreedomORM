unit AM.Freedom.CustomFreedomObject;

interface

uses
  System.Generics.Collections,
  AM.Freedom.IFreedomObject,
  AM.Freedom.ObjectMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria,
  AM.Freedom.IPersistent,
  AM.Freedom.InterfacedObjects,
  AM.Freedom.Attributes,
  AM.Freedom.Consts,
  AM.Freedom.ObjectMapper.ColumnValueItem,
  AM.Freedom.ObjectMapper.LiveRefresh,
  System.SysUtils,
  AM.Freedom.Helper.RttiField,
  AM.Freedom.LiveRefreshObject,
  AM.Freedom.JSONFreedomObject,
  AM.Freedom.JSONAttributes;

type
  TCustomFreedomObjectClass = class of TCustomFreedomObject;

  TCustomFreedomObject = class(TJSONFreedomObject, IFreedomObject)
  strict private
    [NoMapping]
    [JSONUnparsed]
    class var FDefaultsEnabled: Boolean;
  strict private
    [NoMapping]
    [JSONListOnlyItens]
    FColumnValueList: TColumnValueList;
    [NoMapping]
    FObjectState: TObjectState;
    [NoMapping]
    FOldObjectState: TObjectState;
    {$IFDEF USEPOOL}
    [Column(TConsts.cLastUpdateTimeColumn)]
    {$IFEND}
    FLastUpdateTime: TDateTime;
    [NoMapping]
    FHasDetails: Boolean;
    [NoMapping]
    FIsWriteable: Boolean;
    [NoMapping]
    [JSONUnparsed]
    FLiveRefreshs: TLiveRefreshs;
    function GetObjectState: TObjectState;
    function GetOldObjectState: TObjectState;
    {$IFDEF USEPOOL}
    function GetLastUpdateTime: TDateTime;
    {$IFEND}
    procedure Initialize;
    procedure RevertState;
    procedure AssignInitialValues;
    procedure AssignLiveRefreshs(pObjectMapper: TObjectMapper);
    function OnGetLiveRefreshColumnValue(pLazyObject: TObject): TLiveRefreshObject;
  strict protected
    procedure DoFindWithID(pID: Variant); virtual;
    function GetColumnValueList: TColumnValueList;
    procedure DoAssignInitialValues; virtual;
    procedure SetObjectState(const pState: TObjectState);
    procedure SetLastUpdateTime(const pDateTime: TDateTime);
    procedure SetObjectStateWithoutChangeOld(const pState: TObjectState);
    procedure SetOldObjectState(const pState: TObjectState);
    procedure DoSearch(pGroupCriteria: TGroupCriteria); virtual;
    procedure SetColumnValueList(pColumnValueList: TColumnValueList);
    procedure Persist(pObjectState: TObjectState; pPersistent: IPersistent = nil); virtual;
    procedure AssignNonCreatedObjects(pObjectMapper: TObjectMapper); virtual;
    function HasDetails: Boolean;
    function IsWriteable: Boolean;
    property ColumnValueList: TColumnValueList read FColumnValueList;
  public
    destructor Destroy; override;
    class procedure DisableDefaults;
    class procedure EnableDefaults;

    procedure AfterConstruction; override;
    [NoMapping]
    property ObjectState: TObjectState read GetObjectState;
    [NoMapping]
    property OldObjectState: TObjectState read GetOldObjectState;
    {$IFDEF USEPOOL}
    [NoMapping]
    property LastUpdateTime: TDateTime read GetLastUpdateTime;
    {$IFEND}
  end;

implementation

uses
  System.StrUtils,
  System.Variants,
  System.Rtti,
  AM.Freedom.MasterMethodControl,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.ObjectMapper.MapperToObject,
  AM.Freedom.Exceptions,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.ILazy, 
  AM.Freedom.ObjectMapper.ColumnMappers;

{ TCustomEntity }

procedure TCustomFreedomObject.Initialize;
var
  lMapper: TObjectMapper;
  function ExtractMapperFromSelf: TObjectMapper;
  var
    lParams: TObjectToMapperParams;
  begin
    lParams := TObjectToMapperParams.Create;
    lParams.ObjectInstance := Self;
    Result := TObjectToMapper.ObjectToMapper(lParams);
  end;
begin
  FObjectState := Unknown;
  lMapper := ExtractMapperFromSelf;
  try
    FHasDetails := lMapper.Columns.DetailColumns.Count > 0;
    FIsWriteable := lMapper.IsWriteable;
//    AssignNonCreatedObjects(lMapper);
    if (FDefaultsEnabled) then
    begin
      TMapperToObject.WriteDefaultValues(Self, lMapper);
    end;
    TMapperToObject.WriteBooleanValuesToNullables(lMapper);
    if lMapper.Methods.Count > 0 then
      TMasterMethodControl.AddAndRegister(Self, lMapper.Methods.CreateNew);
    AssignLiveRefreshs(lMapper);
  finally
    TObjectToMapper.UnlockMapper(lMapper.GetHashCode);
  end;
end;

function TCustomFreedomObject.IsWriteable: Boolean;
begin
 Result := FIsWriteable;
end;

function TCustomFreedomObject.OnGetLiveRefreshColumnValue(pLazyObject: TObject): TLiveRefreshObject;
var
  lLiveRefresh: TLiveRefresh;
  lParams: TObjectToMapperParams;
  lMapper: TObjectMapper;
  lColumn: TCustomColumnMapper;
begin
  Result := nil;
  lLiveRefresh := FLiveRefreshs.FindLiveRefresh(pLazyObject);
  if (Assigned(lLiveRefresh)) then
  begin
    Result := TLiveRefreshObject.Create;
    Result.RefColumnName := lLiveRefresh.RefColumnName;
    Result.ColumnType := lLiveRefresh.ColumnType;
    lParams := TObjectToMapperParams.Create;
    lParams.ObjectInstance := Self;
    lMapper := TObjectToMapper.ObjectToMapper(lParams);
    try
      lColumn := lMapper.Columns.FindColumn(lLiveRefresh.Column, lLiveRefresh.ColumnType);
      if (Assigned(lColumn)) then
      begin
        Result.ColumnName := lColumn.Name;
        Result.Value := lColumn.RttiOptions.RttiFieldHelper.GetVariantValue(lColumn.RttiOptions.RttiObject, lLiveRefresh.ColumnType);
      end;
    finally
      TObjectToMapper.UnlockMapper(lMapper.GetHashCode);
    end;
  end;
end;

procedure TCustomFreedomObject.Persist(pObjectState: TObjectState; pPersistent: IPersistent);
begin
  raise EInvalidMethodCallOnClass.Create('Persist', ClassName);
end;

procedure TCustomFreedomObject.AfterConstruction;
begin
  inherited;
  FLiveRefreshs := TLiveRefreshs.Create;
  Initialize;
end;

procedure TCustomFreedomObject.AssignInitialValues;
begin
  if (Assigned(FColumnValueList)) then
  begin
    DoAssignInitialValues;
  end;
end;

procedure TCustomFreedomObject.AssignLiveRefreshs(pObjectMapper: TObjectMapper);
var
  lColumn, lJoinColumn: TCustomColumnMapper;
  lObject: TObject;
  lILazy: ILazy;
begin
  if (not Assigned(FLiveRefreshs)) then
  begin
    FLiveRefreshs := TLiveRefreshs.Create;
  end;
  for lColumn in pObjectMapper.Columns.SimpleColumns do
  begin
    for lJoinColumn in pObjectMapper.Columns.JoinColumns do
    begin
      if SameText(lJoinColumn.Name, lColumn.Name) then
      begin
        if (lJoinColumn.LazyOptions.IsLazy) and (lJoinColumn.LazyOptions.LazyType = TLazyType.Simple) then
        begin
          lObject := lJoinColumn.RttiOptions.RttiField.GetValue(lJoinColumn.RttiOptions.RttiObject).AsObject;
          if (Assigned(lObject)) then
          begin
            Supports(lObject, ILazy, lILazy);
            lILazy.SetOnGetLiveRefreshColumnValue(OnGetLiveRefreshColumnValue);
            FLiveRefreshs.AddLiveRefresh(lObject, lColumn.Name, lColumn.ColumnType, TJoinedColumnMapper(lJoinColumn).RefColumnName);
          end;
        end;
      end;
    end;
  end;
end;

procedure TCustomFreedomObject.AssignNonCreatedObjects(pObjectMapper: TObjectMapper);
begin
  // override when necessary
end;

destructor TCustomFreedomObject.Destroy;
begin
  TMasterMethodControl.UnregisterObject(Self);
  FLiveRefreshs.Free;
  FreeAndNil(FColumnValueList);
  inherited;
end;

class procedure TCustomFreedomObject.DisableDefaults;
begin
  FDefaultsEnabled := False;
end;

procedure TCustomFreedomObject.DoAssignInitialValues;
begin
  raise EInvalidMethodCallOnClass.Create('DoAssignInitialValues', ClassName);
end;

procedure TCustomFreedomObject.DoFindWithID(pID: Variant);
begin
  raise EInvalidMethodCallOnClass.Create('DoFindWithID', ClassName);
end;

procedure TCustomFreedomObject.DoSearch(pGroupCriteria: TGroupCriteria);
begin
  raise EInvalidMethodCallOnClass.Create('DoSearch', ClassName);
end;

class procedure TCustomFreedomObject.EnableDefaults;
begin
  FDefaultsEnabled := True;
end;

function TCustomFreedomObject.GetColumnValueList: TColumnValueList;
begin
  if (not Assigned(FColumnValueList)) then
  begin
    FColumnValueList := TColumnValueList.Create;
  end;
  Result := FColumnValueList;
end;

{$IFDEF USEPOOL}
function TCustomFreedomObject.GetLastUpdateTime: TDateTime;
begin
  Result := FLastUpdateTime;
end;
{$IFEND}

function TCustomFreedomObject.GetObjectState: TObjectState;
begin
  Result := FObjectState;
end;

function TCustomFreedomObject.GetOldObjectState: TObjectState;
begin
  Result := FOldObjectState;
end;

function TCustomFreedomObject.HasDetails: Boolean;
begin
  Result := FHasDetails;
end;

procedure TCustomFreedomObject.RevertState;
begin
  FObjectState := FOldObjectState;
  FOldObjectState := Unknown;
end;

procedure TCustomFreedomObject.SetObjectState(const pState: TObjectState);
begin
  FOldObjectState := FObjectState;
  FObjectState := pState;
end;

procedure TCustomFreedomObject.SetObjectStateWithoutChangeOld(const pState: TObjectState);
begin
  FObjectState := pState;
end;

procedure TCustomFreedomObject.SetOldObjectState(const pState: TObjectState);
begin
  FOldObjectState := pState;
end;

procedure TCustomFreedomObject.SetColumnValueList(pColumnValueList: TColumnValueList);
begin
  if (Assigned(FColumnValueList)) then
  begin
    FColumnValueList.Free;
  end;
  FColumnValueList := pColumnValueList;
end;

procedure TCustomFreedomObject.SetLastUpdateTime(const pDateTime: TDateTime);
begin
  FLastUpdateTime := pDateTime;
end;

end.
