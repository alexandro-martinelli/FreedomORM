unit AM.Freedom.FreedomObjectCloner;

interface

uses
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  System.SysUtils,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.ObjectMapper,
  AM.Freedom.ILazy,
  AM.Freedom.SQLMapper.CustomArgument;

type
  TFreedomObjectCloner = class sealed
  strict private
    FSourceColumn: TCustomColumnMapper;
    FDestinyColumn: TCustomColumnMapper;
    FDestinyMapper: TObjectMapper;
    FSourceMapper: TObjectMapper;
    FDetails: Boolean;
    FClearId: Boolean;
    procedure CopyFrom(pSourceObject, pDestinyObject: TObject; pIgnoreColumnNames: array of String;
        pDetails: Boolean = False; pClearId: Boolean = False);
    function CanCopyColumn(pIgnoreColumnNames: array of String): Boolean;
    function ExtractDetailObject(pColumn: TCustomColumnMapper; pInstanceObject: TObject): TObject;
    function ExtractObjectMapperFrom(pObject: TObject): TObjectMapper;
    procedure FinalizeObjectMapperFrom(pMapperHash: Integer);
    procedure CopySimpleValues;
    procedure CopyBlobValues;
    procedure CopyJoinValues;
    procedure DoWriteJoinValue(pColumnValue: Variant);
    function CreateCriteriaForReferenceColumnMapper(pColumnValue: Variant): TCriteria;
    procedure InternalWriteList(pObject: TObject; pColumnValue: Variant);
    procedure InternalWriteObject(pObject: TObject; pColumnValue: Variant);
    procedure SetLazySearch(pColumnValue: Variant);
    function CanSetLazySearchForColumn: Boolean;
    function GetInterfaceLazyFromColumn(pColumn: TCustomColumnMapper): ILazy;
    procedure DoSetLazySearch(pLazy: ILazy; pColumnValue: Variant);
    function CreateArgumentForReferenceColumn(pColumnValue: Variant): TCustomArgument;
    procedure CopyDetailValues(pIgnoreColumnNames: array of String; pSourceObject, pDestinyObject: TObject);
  public
    class procedure MakeCopy(pSourceObject, pDestinyObject: TObject; pIgnoreColumnNames: array of String;
        pDetails: Boolean = False; pClearId: Boolean = False);
  end;


implementation

uses
  AM.Freedom.FreedomObject,
  AM.Freedom.FreedomObjectList,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  System.StrUtils,
  System.Classes,
  System.Variants,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Helper.Variant,
  AM.Freedom.Exceptions,
  AM.Freedom.IFreedomObject,
  AM.Freedom.IFreedomObjectList,
  AM.Freedom.GroupCriteria,
  AM.Freedom.SQLMappers.Arguments,
  System.Rtti,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.Helper.RttiType;

function TFreedomObjectCloner.ExtractObjectMapperFrom(pObject: TObject): TObjectMapper;
var
  lParams: TObjectToMapperParams;
begin
  lParams := TObjectToMapperParams.Create;
  lParams.ObjectInstance := pObject;
  Result := TObjectToMapper.ObjectToMapper(lParams);
end;

procedure TFreedomObjectCloner.FinalizeObjectMapperFrom(pMapperHash: Integer);
begin
  TObjectToMapper.UnlockMapper(pMapperHash);
end;

function TFreedomObjectCloner.CanCopyColumn(pIgnoreColumnNames: array of String): Boolean;
begin
  Result := Assigned(FDestinyColumn);
  if (Result) then
  begin
  Result := not (FSourceColumn.IdOptions.IsId and FClearId);
    if (Result) then
    begin
      Result := IndexText(FSourceColumn.Name, pIgnoreColumnNames) < 0;
    end;
  end;
end;

function TFreedomObjectCloner.ExtractDetailObject(pColumn: TCustomColumnMapper; pInstanceObject: TObject): TObject;
var
  lObject: TObject;
  lLazyList: ILazy;
begin
  Result := nil;
  lObject := pColumn.RttiOptions.RttiFieldHelper.GetObjectValue(pInstanceObject);
  if pColumn.LazyOptions.IsLazy then
  begin
    if Supports(lObject, ILazy, lLazyList) then
    begin
      if not pColumn.LazyOptions.IsLazyLoaded then
      begin
        lLazyList.Load;
      end;
      Result := lLazyList.GetObject;
    end
    else
    begin
      Result := lObject;
    end;
  end;
end;

procedure TFreedomObjectCloner.CopyFrom(pSourceObject, pDestinyObject: TObject; pIgnoreColumnNames: Array of String; pDetails, pClearId: Boolean);
var
  lColumn: TCustomColumnMapper;
begin
  if (Assigned(pDestinyObject)) and Assigned(pSourceObject) and (pSourceObject.ClassType = pDestinyObject.ClassType) then
  begin
    FDetails := pDetails;
    FClearId := pClearId;
    FDestinyMapper := ExtractObjectMapperFrom(pDestinyObject);
    FSourceMapper := ExtractObjectMapperFrom(pSourceObject);
    try
      for lColumn in FSourceMapper.Columns do
      begin
        FSourceColumn := lColumn;
        FDestinyColumn := FDestinyMapper.Columns.FindColumnByFieldOrPropName(FSourceColumn.RttiOptions.RttiField.Name, FSourceColumn.ColumnType);
        if CanCopyColumn(pIgnoreColumnNames) then
        begin
          if FSourceColumn.ColumnType.IsSimpleType or FSourceColumn.ColumnType.IsOrdinal then
          begin
            CopySimpleValues;
          end
          else if (FDestinyColumn.ColumnType.IsBlob) then
          begin
            CopyBlobValues;
          end
          else if (FDestinyColumn.ColumnType = ctyJoin) then
          begin
            CopyJoinValues;
          end;
        end;
      end;
      if (pDetails) then
      begin
        CopyDetailValues(pIgnoreColumnNames, pSourceObject, pDestinyObject);
      end;
    finally
      FinalizeObjectMapperFrom(FDestinyMapper.GetHashCode);
      FinalizeObjectMapperFrom(FSourceMapper.GetHashCode);
    end;
  end;
end;

procedure TFreedomObjectCloner.CopyDetailValues(pIgnoreColumnNames: array of String; pSourceObject, pDestinyObject: TObject);
var
  lSourceDetailObject: TFreedomObjectList<TFreedomObject>;
  lDestinyDetailObject: TFreedomObjectList<TFreedomObject>;
  lDestinyObject: TFreedomObject;
  lColumn: TCustomColumnMapper;
  lSourceObject: TFreedomObject;
begin
  for lColumn in FSourceMapper.Columns.DetailColumns do
  begin
    FSourceColumn := lColumn;
    FDestinyColumn := FDestinyMapper.Columns.FindColumnByFieldOrPropName(FSourceColumn.RttiOptions.RttiField.Name, FSourceColumn.ColumnType);
    lSourceDetailObject := TFreedomObjectList<TFreedomObject>(ExtractDetailObject(FSourceColumn, pSourceObject));
    lDestinyDetailObject := TFreedomObjectList<TFreedomObject>(ExtractDetailObject(FDestinyColumn, pDestinyObject));
    for lSourceObject in lSourceDetailObject do
    begin
      lDestinyObject :=  TFreedomObject(TFreedomObjectClass(FDestinyColumn.LazyOptions.LazyClassType).Create);
      lDestinyObject.CopyFrom(lSourceObject, pIgnoreColumnNames, FDetails, FClearId);
      lDestinyDetailObject.Insert(lDestinyObject);
    end;
  end;
end;

procedure TFreedomObjectCloner.CopyJoinValues;
var
  lColumnValue: Variant;
  lObject: TObject;
  lMapper: TObjectMapper;
  lColumn: TCustomColumnMapper;
  lLazy: ILazy;
  lColumnType: TColumnType;
begin
  if not FSourceColumn.LazyOptions.IsLazy then
  begin
    if (not FSourceColumn.RttiOptions.RttiField.FieldType.IsInstance) then
    begin
      lColumnType := TReferenceColumnMapper(FSourceColumn).RefColumnType;
      if (TJoinedColumnMapper(FSourceColumn).RefResultColumnName <> '') then
      begin
        lColumnType := FSourceColumn.RttiOptions.RttiField.FieldType.ToColumnType;
      end;
      lColumnValue := FSourceColumn.RttiOptions.RttiFieldHelper.GetVariantValue(FSourceColumn.RttiOptions.RttiObject, lColumnType);
      if (lColumnValue.IsNotNull) then
      begin
        FDestinyColumn.RttiOptions.RttiFieldHelper.SetVariantValue(FDestinyColumn.RttiOptions.RttiObject, lColumnValue);
      end;
    end
    else
    begin
      lObject := FSourceColumn.RttiOptions.RttiFieldHelper.GetObjectValue(FSourceColumn.RttiOptions.RttiObject);
      lMapper := ExtractObjectMapperFrom(lObject);
      lColumn := lMapper.Columns.FindColumn(TReferenceColumnMapper(FSourceColumn).RefColumnName, TReferenceColumnMapper(FSourceColumn).RefColumnType);
      lColumnValue := lColumn.RttiOptions.RttiFieldHelper.GetVariantValue(lObject, lCOlumn.ColumnType);
      if (lColumnValue.IsNotNull) then
      begin
        DoWriteJoinValue(lColumnValue);
      end;
    end;
  end
  else
  begin
    lLazy := GetInterfaceLazyFromColumn(FSourceColumn);
    lColumnValue := lLazy.GetLazyID;
    if (lColumnValue.IsNotNull) then
    begin
      SetLazySearch(lColumnValue);
    end;
  end;
end;

procedure TFreedomObjectCloner.DoWriteJoinValue(pColumnValue: Variant);
var
  lClass: TClass;
  lObject: TObject;
begin
  lClass := FDestinyColumn.RttiOptions.RttiField.FieldType.AsInstance.MetaClassType;
  lObject := FDestinyColumn.RttiOptions.RttiFieldHelper.GetObjectValue(FDestinyColumn.RttiOptions.RttiObject);
  if (Assigned(lObject)) then
  begin
    if Supports(lClass, IFreedomObjectList) then
    begin
      InternalWriteList(lObject, pColumnValue);
    end
    else if Supports(lClass, IFreedomObject) then
    begin
      InternalWriteObject(lObject, pColumnValue);
    end;
  end
  else
  begin
    raise EFieldValueCannotBeAssignedOnClass.Create(FDestinyColumn.RttiOptions.RttiField.Name, lClass.ClassName,
        FDestinyColumn.RttiOptions.RttiObject.ClassName);
  end;
end;

procedure TFreedomObjectCloner.InternalWriteList(pObject: TObject; pColumnValue: Variant);
var
  lFreedomList: IFreedomObjectList;
  lGroupCriteria: TGroupCriteria;
begin
  if Assigned(pObject) and pObject.GetInterface(IFreedomObjectList, lFreedomList) then
  begin
    lGroupCriteria := TGroupCriteria.Create;
    lGroupCriteria.AddCriteria(CreateCriteriaForReferenceColumnMapper(pColumnValue));
    lFreedomList.DoSearch(lGroupCriteria);
    FreeAndNil(lGroupCriteria);
  end;
end;

procedure TFreedomObjectCloner.InternalWriteObject(pObject: TObject; pColumnValue: Variant);
var
  lFreedomObject: IFreedomObject;
  lGroupCriteria: TGroupCriteria;
begin
  if Assigned(pObject) and pObject.GetInterface(IFreedomObject, lFreedomObject) then
  begin
    lGroupCriteria := TGroupCriteria.Create;
    lGroupCriteria.AddCriteria(CreateCriteriaForReferenceColumnMapper(pColumnValue));
    lFreedomObject.DoSearch(lGroupCriteria);
    FreeAndNil(lGroupCriteria);
  end;
end;

function TFreedomObjectCloner.CreateCriteriaForReferenceColumnMapper(pColumnValue: Variant): TCriteria;
var
  lAlias: string;
begin
  lAlias := TReferenceColumnMapper(FDestinyColumn).OriginalRefObjectAlias;
  Result := TCriteria.CreateAsEqual(TFieldArgument.Create(TReferenceColumnMapper(FDestinyColumn).RefColumnName, lAlias),
    CreateArgumentForReferenceColumn(pColumnValue));
end;

function TFreedomObjectCloner.CreateArgumentForReferenceColumn(pColumnValue: Variant): TCustomArgument;
begin
  case TReferenceColumnMapper(FDestinyColumn).RefColumnType of
    ctyByte, ctySmallint:
      Result := TValueArgument.CreateAsSmallint(pColumnValue);
    ctyInteger:
      Result := TValueArgument.CreateAsInteger(pColumnValue);
    ctyInt64:
      Result := TValueArgument.CreateAsInt64(pColumnValue);
    ctyChar, ctyString:
      Result := TValueArgument.CreateAsString(pColumnValue);
    ctySingle, ctyDouble:
      Result := TValueArgument.CreateAsDouble(pColumnValue);
    ctyCurrency:
      Result := TValueArgument.CreateAsCurrency(pColumnValue);
    ctyExtended:
      Result := TValueArgument.CreateAsExtended(pColumnValue);
    ctyDate:
      Result := TValueArgument.CreateAsDate(pColumnValue);
    ctyTime:
      Result := TValueArgument.CreateAsTime(pColumnValue);
    ctyDateTime:
      Result := TValueArgument.CreateAsDateTime(pColumnValue);
  else
    raise EInvalidReferencedColumnColumnType.Create(FDestinyColumn.Name, TReferenceColumnMapper(FDestinyColumn).RefColumnType.ToString,
        FDestinyColumn.RttiOptions.RttiObject.ClassName);
  end;
end;

procedure TFreedomObjectCloner.CopyBlobValues;
var
  lDestinyStream, lSourceStream: TStream;
  lDestinyStrings, lSourceStrings: TStrings;
begin
  if FDestinyColumn.RttiOptions.RttiField.FieldType.AsInstance.MetaclassType.InheritsFrom(TStrings) then
  begin
    lDestinyStrings := TStrings(FDestinyColumn.RttiOptions.RttiFieldHelper.GetObjectValue(FDestinyColumn.RttiOptions.RttiObject));
    lSourceStrings := TStrings(FSourceColumn.RttiOptions.RttiFieldHelper.GetObjectValue(FSourceColumn.RttiOptions.RttiObject));
    lDestinyStrings.Text := lSourceStrings.Text;
  end
  else
  begin
    lDestinyStream := TStream(FDestinyColumn.RttiOptions.RttiFieldHelper.GetObjectValue(FDestinyColumn.RttiOptions.RttiObject));
    lSourceStream := TStream(FSourceColumn.RttiOptions.RttiFieldHelper.GetObjectValue(FSourceColumn.RttiOptions.RttiObject));
    lSourceStream.Position := 0;
    lDestinyStream.CopyFrom(lSourceStream, 0);
  end;
end;

procedure TFreedomObjectCloner.CopySimpleValues;
var
  lColumnValue: Variant;
begin
  if FDestinyColumn.IsNullable then
  begin
    lColumnValue := FSourceColumn.RttiOptions.RttiFieldHelper.GetNullableVariantValue(FSourceColumn.RttiOptions.RttiObject);
    FDestinyColumn.RttiOptions.RttiFieldHelper.SetNullableVariantValue(FDestinyColumn.RttiOptions.RttiObject, lColumnValue);
  end
  else
  begin
    FDestinyColumn.RttiOptions.RttiField.SetValue(FDestinyColumn.RttiOptions.RttiObject,
        FSourceColumn.RttiOptions.RttiField.GetValue(FSourceColumn.RttiOptions.RttiObject));
  end;
end;

procedure TFreedomObjectCloner.SetLazySearch(pColumnValue: Variant);
var
  lLazy: ILazy;
begin
  if CanSetLazySearchForColumn then
  begin
    lLazy := GetInterfaceLazyFromColumn(FDestinyColumn);
    if Assigned(lLazy) then
    begin
      DoSetLazySearch(lLazy, pColumnValue);
    end;
  end;
end;

function TFreedomObjectCloner.CanSetLazySearchForColumn: Boolean;
begin
  Result := (FDestinyColumn.RttiOptions.RttiObject <> nil) and (FDestinyColumn.ColumnType in [ctyJoin, ctyDetail]) and
      (TReferenceColumnMapper(FDestinyColumn).RefColumnName <> '');
end;

function TFreedomObjectCloner.GetInterfaceLazyFromColumn(pColumn: TCustomColumnMapper): ILazy;
var
  lObject: TObject;
  lFieldClassName: string;
begin
  Result := nil;
  lObject := pColumn.RttiOptions.RttiField.GetValue(pColumn.RttiOptions.RttiObject).AsObject;
  if Assigned(lObject) then
  begin
    lObject.GetInterface(ILazy, Result);
  end
  else
  begin
    case pColumn.LazyOptions.LazyType of
      Simple: lFieldClassName := 'TLazy';
      List: lFieldClassName := 'TLazyList';
    end;
    raise EFieldValueCannotBeAssignedOnClass.Create(pColumn.RttiOptions.RttiField.Name, lFieldClassName,
        pColumn.RttiOptions.RttiObject.ClassName);
  end;
end;

procedure TFreedomObjectCloner.DoSetLazySearch(pLazy: ILazy; pColumnValue: Variant);
var
  lLazySearch: TGroupCriteria;
begin
  lLazySearch := TGroupCriteria.Create;
  lLazySearch.AddCriteria(CreateCriteriaForReferenceColumnMapper(pColumnValue));
  pLazy.SetLazySearch(lLazySearch);
  pLazy.SetLazyID(pColumnValue);
end;

class procedure TFreedomObjectCloner.MakeCopy(pSourceObject, pDestinyObject: TObject;
  pIgnoreColumnNames: array of String; pDetails, pClearId: Boolean);
var
  lCloner: TFreedomObjectCloner;
begin
  lCloner := TFreedomObjectCloner.Create;
  try
    lCloner.CopyFrom(pSourceObject, pDestinyObject, pIgnoreColumnNames, pDetails, pClearId);
  finally
    lCloner.Free;
  end;
end;

end.