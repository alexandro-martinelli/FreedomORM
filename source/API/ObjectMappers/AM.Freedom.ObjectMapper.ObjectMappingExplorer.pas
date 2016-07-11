unit AM.Freedom.ObjectMapper.ObjectMappingExplorer;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  AM.Freedom.ObjectMapper,
  AM.Freedom.EnumerationTypes;

type
  TMapperHash = Integer;

  TObjectMappingItem = class
  private
    FMetaClassType: TClass;
    FObjectMapper: TObjectMapper;
    FMapperHash: TMapperHash;
    FLocked: Boolean;
    FOptions: TObjectToMapperOptions;
    function GetUnlocked: Boolean;
  public
    destructor Destroy; override;
    property MetaClassType: TClass read FMetaClassType write FMetaClassType;
    property ObjectMapper: TObjectMapper read FObjectMapper write FObjectMapper;
    property MapperHash: TMapperHash read FMapperHash write FMapperHash;
    property Locked: Boolean read FLocked write FLocked;
    property Unlocked: Boolean read GetUnlocked;
    property Options: TObjectToMapperOptions read FOptions write FOptions;
  end;

  TObjectMappingItemList = class(TObjectList<TObjectMappingItem>)
  strict private
    function IsSameItemMapping(pItem: TObjectMappingItem; pMetaClassType: TClass; pMapperHash: TMapperHash; pOptions: TObjectToMapperOptions): Boolean;
  public
    function FindMappingByMetaClassType(pMetaClassType: TClass; pMapperHash: TMapperHash; pOptions: TObjectToMapperOptions): TObjectMappingItem;
    function FindMappingByMapperHash(pMapperHash: TMapperHash): TObjectMappingItem;
    function AddMapping(pMetaClassType: TClass; pObjectMapper: TObjectMapper; pOptions: TObjectToMapperOptions): TMapperHash;
  end;

  TObjectMappingExplorer = class sealed
  strict private
    class var FObjectMappingExplorerList: TObjectMappingItemList;
  private
    class procedure CreateObjectMappingExplorerList;
    class procedure DestroyObjectMappingExplorerList;
  public
    class function FindMappingByMetaClassType(pMetaClassType: TClass; pMapperHash: TMapperHash; pOptions: TObjectToMapperOptions): TObjectMapper;
    class procedure UnlockMapper(pMapperHash: TMapperHash);
    class procedure RemoveIncompleteMapper(pMapperHash: TMapperHash);
    class procedure LockMapper(pObjectMapper: TObjectMapper);
    class function AddMapping(pMetaClassType: TClass; pObjectMapper: TObjectMapper; pOptions: TObjectToMapperOptions): TMapperHash;
  end;

implementation

{ TObjectMappingItemList }

function TObjectMappingItemList.AddMapping(pMetaClassType: TClass; pObjectMapper: TObjectMapper; pOptions: TObjectToMapperOptions): TMapperHash;
var
  lObjectMappingItem: TObjectMappingItem;
begin
  lObjectMappingItem := TObjectMappingItem.Create;
  lObjectMappingItem.MetaClassType := pMetaClassType;
  lObjectMappingItem.ObjectMapper := pObjectMapper;
  lObjectMappingItem.MapperHash := pObjectMapper.GetHashCode;
  lObjectMappingItem.Locked := True;
  lObjectMappingItem.Options := pOptions;
  Result := lObjectMappingItem.MapperHash;
  Add(lObjectMappingItem);
end;

function TObjectMappingItemList.FindMappingByMapperHash(pMapperHash: TMapperHash): TObjectMappingItem;
var
  lItem: TObjectMappingItem;
begin
  Result := nil;
  for lItem in Self do
  begin
    if (lItem.MapperHash = pMapperHash) then
    begin
      Result := lItem;
      Break;
    end;
  end;
end;

function TObjectMappingItemList.FindMappingByMetaClassType(pMetaClassType: TClass; pMapperHash: Integer; pOptions: TObjectToMapperOptions): TObjectMappingItem;
var
  lItem: TObjectMappingItem;
begin
  Result := nil;
  for lItem in Self do
  begin
    if IsSameItemMapping(lItem, pMetaClassType, pMapperHash, pOptions) then
    begin
      Result := lItem;
      Break;
    end;
  end;
end;

function TObjectMappingItemList.IsSameItemMapping(pItem: TObjectMappingItem; pMetaClassType: TClass; pMapperHash: TMapperHash; pOptions: TObjectToMapperOptions): Boolean;
begin
  Result := SameText(pItem.MetaClassType.ClassName, pMetaClassType.ClassName);
  if (Result) then
  begin
    if (pMapperHash > 0) then
    begin
      Result := pItem.MapperHash = pMapperHash;
    end
    else
    begin
      Result := (pOptions = pItem.Options) and pItem.Unlocked;
    end;
  end;
end;
{ TObjectMappingExplorer }

class function TObjectMappingExplorer.AddMapping(pMetaClassType: TClass; pObjectMapper: TObjectMapper; pOptions: TObjectToMapperOptions): TMapperHash;
begin
  Result := FObjectMappingExplorerList.AddMapping(pMetaClassType, pObjectMapper, pOptions);
end;

class procedure TObjectMappingExplorer.CreateObjectMappingExplorerList;
begin
  if (not Assigned(FObjectMappingExplorerList)) then
  begin
    FObjectMappingExplorerList := TObjectMappingItemList.Create;
  end;
end;

class procedure TObjectMappingExplorer.DestroyObjectMappingExplorerList;
begin
  FObjectMappingExplorerList.Free;
end;

class function TObjectMappingExplorer.FindMappingByMetaClassType(pMetaClassType: TClass; pMapperHash: TMapperHash; pOptions: TObjectToMapperOptions): TObjectMapper;
var
  lItem: TObjectMappingItem;
begin
  CreateObjectMappingExplorerList;
  Result := nil;
  lItem := FObjectMappingExplorerList.FindMappingByMetaClassType(pMetaClassType, pMapperHash, pOptions);
  if (Assigned(lItem)) then
  begin
    Result := lItem.ObjectMapper;
  end;
end;

class procedure TObjectMappingExplorer.LockMapper(pObjectMapper: TObjectMapper);
var
  lItem: TObjectMappingItem;
begin
  lItem := FObjectMappingExplorerList.FindMappingByMapperHash(pObjectMapper.GetHashCode);
  if (Assigned(lItem)) then
  begin
    lItem.Locked := True;
  end;
end;

class procedure TObjectMappingExplorer.RemoveIncompleteMapper(pMapperHash: TMapperHash);
var
  lItem: TObjectMappingItem;
begin
  lItem := FObjectMappingExplorerList.FindMappingByMapperHash(pMapperHash);
  if (Assigned(lItem)) then
  begin
    FObjectMappingExplorerList.Remove(lItem);
  end;
end;

class procedure TObjectMappingExplorer.UnlockMapper(pMapperHash: TMapperHash);
var
  lItem: TObjectMappingItem;
begin
  lItem := FObjectMappingExplorerList.FindMappingByMapperHash(pMapperHash);
  if (Assigned(lItem)) then
  begin
    lItem.Locked := False;
    lItem.ObjectMapper.CurrentSchema := '';
  end;
end;
{ TObjectMappingItem }

destructor TObjectMappingItem.Destroy;
begin
  FObjectMapper.Free;
  inherited;
end;

function TObjectMappingItem.GetUnlocked: Boolean;
begin
  Result := not FLocked;
end;

initialization

finalization
  TObjectMappingExplorer.DestroyObjectMappingExplorerList;

end.
