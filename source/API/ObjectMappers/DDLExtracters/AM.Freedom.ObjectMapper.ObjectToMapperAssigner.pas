unit AM.Freedom.ObjectMapper.ObjectToMapperAssigner;

interface

uses
  System.SysUtils,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.ObjectMapper;

type
  TObjectToMapperAssigner = class sealed
  strict private
    FObjectMapper: TObjectMapper;
    FObjectInstance: TObject;
    procedure VerifyLazyLoad(pColumn: TCustomColumnMapper);
    procedure AssignSimpleColumns;
    procedure AssignObjectColumns;
    procedure AssignJoinColumns;
    procedure AssignDetailColumns;
    procedure AssignBlobColumns;
    procedure AssignExtensionColumns;
    procedure AssignChildColumns(pColumn: TExtensionColumnMapper; pObjectInstance: TObject);
  public
    procedure AssignObjectToMapper(pObjectMapper: TObjectMapper; pObjectInstance: TObject);
  end;

implementation

uses
  AM.Freedom.Helper.RttiField,
  AM.Freedom.Helper.RttiProperty,
  AM.Freedom.ILazy;

{ TObjectToMapperAssigner }

procedure TObjectToMapperAssigner.AssignBlobColumns;
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in FObjectMapper.Columns.BlobColumns do
  begin
    lColumn.RttiOptions.RttiObject := FObjectInstance;
  end;
end;

procedure TObjectToMapperAssigner.AssignChildColumns(pColumn: TExtensionColumnMapper; pObjectInstance: TObject);
var
  lObject: TObject;
  lChildColumn: TCustomColumnMapper;
begin
  lObject := nil;
  if (Assigned(pObjectInstance)) then
  begin
    if pColumn.RttiOptions.IsField then
    begin
      lObject := pColumn.RttiOptions.RttiFieldHelper.GetObjectValue(pObjectInstance);
    end
    else
    begin
      lObject := pColumn.RttiOptions.RttiProperty.GetObjectValue(pObjectInstance);
    end;
  end;
  for lChildColumn in TExtensionColumnMapper(pColumn).ChildColumns do
  begin
    lChildColumn.RttiOptions.RttiObject := lObject;
    if (Assigned(lObject)) then
    begin
      VerifyLazyLoad(lChildColumn);
    end
    else
    begin
      lChildColumn.LazyOptions.IsLazyLoaded := False;
    end;
    if (lChildColumn.IsExtension) then
    begin
      AssignChildColumns(TExtensionColumnMapper(lChildColumn), lObject)
    end;
  end;
end;

procedure TObjectToMapperAssigner.AssignDetailColumns;
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in FObjectMapper.Columns.DetailColumns do
  begin
    lColumn.RttiOptions.RttiObject := FObjectInstance;
    if (lColumn.ParentColumn = nil) then
    begin
      VerifyLazyLoad(lColumn);
    end;
  end;
end;

procedure TObjectToMapperAssigner.AssignExtensionColumns;
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in FObjectMapper.Columns.ExtensionColumns do
  begin
    if (lColumn.ParentColumn = nil) then
    begin
      AssignChildColumns(TExtensionColumnMapper(lColumn), FObjectInstance);
    end;
  end;
end;

procedure TObjectToMapperAssigner.AssignJoinColumns;
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in FObjectMapper.Columns.JoinColumns do
  begin
    lColumn.RttiOptions.RttiObject := FObjectInstance;
    if (lColumn.ParentColumn = nil) then
    begin
      VerifyLazyLoad(lColumn);
    end;
  end;
end;

procedure TObjectToMapperAssigner.AssignObjectColumns;
begin
  AssignJoinColumns;
  AssignDetailColumns;
  AssignBlobColumns;
end;

procedure TObjectToMapperAssigner.AssignObjectToMapper(pObjectMapper: TObjectMapper; pObjectInstance: TObject);
begin
  FObjectMapper := pObjectMapper;
  FObjectInstance := pObjectInstance;
  FObjectMapper.RttiOptions.RttiObject := pObjectInstance;
  AssignSimpleColumns;
  AssignObjectColumns;
  AssignExtensionColumns;
end;

procedure TObjectToMapperAssigner.AssignSimpleColumns;
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in FObjectMapper.Columns.SimpleColumns do
  begin
    lColumn.RttiOptions.RttiObject := FObjectInstance;
  end;
  for lColumn in FObjectMapper.Columns.OrdinalColumns do
  begin
    lColumn.RttiOptions.RttiObject := FObjectInstance;
  end;
end;

procedure TObjectToMapperAssigner.VerifyLazyLoad(pColumn: TCustomColumnMapper);
var
  lObject: TObject;
  lLazy: ILazy;
begin
  pColumn.LazyOptions.IsLazyLoaded := False;
  if (pColumn.LazyOptions.IsLazy) then
  begin
    lObject := pColumn.RttiOptions.RttiFieldHelper.GetObjectValue(pColumn.RttiOptions.RttiObject);
    if Supports(lObject, ILazy, lLazy) then
    begin
      pColumn.LazyOptions.IsLazyLoaded := lLazy.Loaded;
    end;
  end;
end;


end.