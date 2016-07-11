unit AM.Freedom.ObjectMapper;

interface

uses
  System.Rtti,
  AM.Freedom.ObjectMapper.CustomMapper,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.ObjectMapper.TriggerMapper,
  AM.Freedom.ObjectMapper.MethodMapper,
  AM.Freedom.ObjectMapper.ConstraintMapper,
  AM.Freedom.ObjectMapper.Schemas,
  AM.Freedom.ObjectMapper.LiveRefresh,
  AM.Freedom.GroupCriteria,
  AM.Freedom.ObjectMapper.ColumnsList,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.ObjectMapper.ColumnValueItem;

type
  TRttiObjectOptions = class sealed
  private
    FRttiType: TRttiType;
    FRttiObject: TObject;
  public
    destructor Destroy; override;
    procedure Assign(pSource: TRttiObjectOptions);
    procedure Clear;
    function CreateNew: TRttiObjectOptions;
    property RttiType: TRttiType read FRttiType write FRttiType;
    property RttiObject: TObject read FRttiObject write FRttiObject;
  end;

  TObjectMapper = class sealed(TCustomMapper)
  strict private
    FColumns: TColumnsList;
    FTriggers: TTriggersMapper;
    FMethods: TMethodMapperList;
    FIsAutoMapping: Boolean;
    FUnmapped: Boolean;
    FIsList: Boolean;
    FMetaclassType: TClass;
    FRttiOptions: TRttiObjectOptions;
    FUniques: TUniqueMappers;
    FPrimarys: TPrimaryMappers;
    FForeigns: TForeignMappers;
    FSchemas: TSchemas;
    FCurrentSchema: string;
    FLiveRefreshDone: Boolean;
    FLiveRefreshs: TLiveRefreshs;
    FObjectCursorClass: TClass;
    function GetIsEmpty: Boolean;
    function GetInitialValues: TColumnValueList;
    function GetHasDetails: Boolean;
    procedure SetRttiOptions(const Value: TRttiObjectOptions);
    procedure SetCurrentSchema(const Value: string);
    function GetIsWriteable: Boolean;
    procedure SetObjectCursorClass(const Value: TClass);
  public
    constructor Create(pOwnsObjects: Boolean = True);
    destructor Destroy; override;
    property Alias;
    property CurrentSchema: string read FCurrentSchema write SetCurrentSchema;
    property IsAutoMapping: Boolean read FIsAutoMapping write FIsAutoMapping;
    property MetaClassType: TClass read FMetaclassType write FMetaclassType;
    property RttiOptions: TRttiObjectOptions read FRttiOptions write SetRttiOptions;
    property ObjectCursorClass: TClass read FObjectCursorClass write SetObjectCursorClass;
    property Unmapped: Boolean read FUnmapped write FUnmapped;
    property IsList: Boolean read FIsList write FIsList;
    property Columns: TColumnsList read FColumns;
    property Triggers: TTriggersMapper read FTriggers;
    property Methods: TMethodMapperList read FMethods;
    property Uniques: TUniqueMappers read FUniques;
    property Primarys: TPrimaryMappers read FPrimarys;
    property Foreigns: TForeignMappers read FForeigns;
    property IsEmpty: Boolean read GetIsEmpty;
    property HasDetails: Boolean read GetHasDetails;
    property Schemas: TSchemas read FSchemas;
    property InitialValues: TColumnValueList read GetInitialValues;
    property IsWriteable: Boolean read GetIsWriteable;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.IFreedomObject,
  AM.Freedom.IFreedomObjectList,
  AM.Freedom.ILazy,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.Helper.RttiField,
  AM.Freedom.ObjectMapper.CustomObjectCursor;

{ TObjectMapper }

constructor TObjectMapper.Create(pOwnsObjects: Boolean);
begin
  FIsAutoMapping := False;
  FUnmapped := True;
  FIsList := False;
  FLiveRefreshDone := False;

  FColumns := TColumnsList.Create(pOwnsObjects);
  FTriggers := TTriggersMapper.Create(pOwnsObjects);
  FMethods := TMethodMapperList.Create(pOwnsObjects);
  FRttiOptions := TRttiObjectOptions.Create;
  FUniques := TUniqueMappers.Create(pOwnsObjects);
  FPrimarys := TPrimaryMappers.Create(pOwnsObjects);
  FForeigns := TForeignMappers.Create(pOwnsObjects);
  FSchemas := TSchemas.Create;
  FLiveRefreshs := TLiveRefreshs.Create;

end;

destructor TObjectMapper.Destroy;
begin
  FColumns.Free;
  FTriggers.Free;
  FMethods.Free;
  FRttiOptions.Free;
  FUniques.Free;
  FPrimarys.Free;
  FForeigns.Free;
  FSchemas.Free;
  FLiveRefreshs.Free;
  inherited;
end;

function TObjectMapper.GetHasDetails: Boolean;
begin
  Result := FColumns.DetailColumns.Count > 0;
end;

function TObjectMapper.GetInitialValues: TColumnValueList;
var
  lIFreedomObject: IFreedomObject;
begin
  Result := nil;
  if Assigned(FRttiOptions.RttiObject) and Supports(FRttiOptions.RttiObject, IFreedomObject, lIFreedomObject) then
  begin
    Result := lIFreedomObject.GetColumnValueList;
  end;
end;

function TObjectMapper.GetIsEmpty: Boolean;
var
  lColumn: TCustomColumnMapper;
begin
  Result := True;
  for lColumn in FColumns.SimpleColumns do
  begin
    if not lColumn.ColumnType.IsOrdinal and not lColumn.ColumnType.IsObject then
    begin
      Result := lColumn.IsNull;
      if not Result then
      begin
        Break;
      end;
    end;
  end;
end;

function TObjectMapper.GetIsWriteable: Boolean;
begin
  Result := (FObjectCursorClass = nil);
end;

procedure TObjectMapper.SetCurrentSchema(const Value: string);
begin
  FCurrentSchema := Value;
  FColumns.CurrentSchema := Value;
end;

procedure TObjectMapper.SetObjectCursorClass(const Value: TClass);
begin
  FObjectCursorClass := Value;
  if (Value <> nil) then
  begin
    Name := '';
    Alias := '';
    FIsAutoMapping := False;
  end;
end;

procedure TObjectMapper.SetRttiOptions(const Value: TRttiObjectOptions);
begin
  if FRttiOptions <> Value then
  begin
    FreeAndNil(FRttiOptions);
  end;
  FRttiOptions := Value;
end;
{ TRttiObjectOptions }

procedure TRttiObjectOptions.Assign(pSource: TRttiObjectOptions);
begin
  FRttiType := pSource.RttiType;
  FRttiObject := pSource.RttiObject;
end;

procedure TRttiObjectOptions.Clear;
begin
  FRttiType := nil;
  FRttiObject := nil;
end;

function TRttiObjectOptions.CreateNew: TRttiObjectOptions;
begin
  Result := TRttiObjectOptions.Create;
  Result.Assign(Self);
end;

destructor TRttiObjectOptions.Destroy;
begin
  Clear;
  inherited;
end;

end.
