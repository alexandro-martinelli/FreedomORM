unit AM.Freedom.ObjectMapper.MapperToObject;

interface

uses
  System.Rtti,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.IPersistent,
  AM.Freedom.ObjectMapper,
  AM.Freedom.IFreedomObject,
  AM.Freedom.IFreedomObjectList,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.ILazy, AM.Freedom.INullable, AM.Freedom.Nullable;

type
  TMapperToObject = class
  strict private
    class var FObjectInstance: TObject;
    class var FObjectMapper: TObjectMapper;
    class var FPersistent: IPersistent;
    class var FObjectState: TObjectState;
    class var FColumn: TCustomColumnMapper;
    class procedure WriteValuesForSimpleColumns;
    class procedure WriteValuesForOrdinalColumns;
    class procedure WriteValuesForNullableColumns;
    class procedure WriteInternalObjects;
    class procedure WriteJoins;
    class procedure DoWriteJoinColumn;
    class procedure DoWriteJoins;
    class procedure InternalWriteEntity(pObject: TObject);
    class procedure DoInternalWriteEntityIfObjectAssigned(pFreedomObject: IFreedomObject; pObject: TObject);
    class procedure DoInternalWriteEntityIfUnknownObjectState(pFreedomObject: IFreedomObject);
    class procedure DoInternalWriteEntityIfKnownObjectState(pFreedomObject: IFreedomObject);
    class procedure WriteList;
    class procedure DoWriteList;
    class procedure InternalWriteList(pObject: TObject);
    class procedure DoInternalWriteListIfUnknownObjectState(pFreedomList: IFreedomObjectList);
    class procedure DoInternalWriteListIfKnownObjectState(pFreedomList: IFreedomObjectList);
    class function CanPersistList(pFreedomList: IFreedomObjectList): Boolean;
    class procedure WriteBlobs;
    class procedure DoWriteBlobs;
    class procedure SetLazySearch;
    class function CanSetLazySearchForColumn: Boolean;
    class function GetInterfaceLazyFromColumn: ILazy;
    class procedure DoSetLazySearch(pLazy: ILazy);
    class function CreateCriteriaForReferenceColumnMapper: TCriteria;
    class function CreateArgumentForReferenceColumn: TCustomArgument;
    class procedure FinalizeInternalFields;
  public
    class procedure WriteObject(pObjectMapper: TObjectMapper; pPersistent: IPersistent; pObjectState: TObjectState);
    class procedure WriteDefaultValues(pObjectInstance: TObject; pMapper: TObjectMapper);
    class procedure WriteBooleanValuesToNullables(pMapper: TObjectMapper);
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  AM.Freedom.GroupCriteria,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.Helper.Variant,
  AM.Freedom.Exceptions,
  AM.Freedom.Helper.RttiField,
  AM.Freedom.SQLMappers.Arguments;

{ TObjectWriter }

class procedure TMapperToObject.WriteObject(pObjectMapper: TObjectMapper; pPersistent: IPersistent; pObjectState: TObjectState);
begin
  FObjectInstance := pObjectMapper.RttiOptions.RttiObject;
  FObjectMapper := pObjectMapper;
  FPersistent := pPersistent;
  FObjectState := pObjectState;
  WriteValuesForSimpleColumns;
  WriteValuesForOrdinalColumns;
  WriteValuesForNullableColumns;
  WriteInternalObjects;
  FinalizeInternalFields;
end;

class procedure TMapperToObject.WriteDefaultValues(pObjectInstance: TObject; pMapper: TObjectMapper);
var
  lColumn: TCustomColumnMapper;
  lValue: TValue;
  lNow: TDateTime;
begin
  for lColumn in pMapper.Columns do
  begin
    if lColumn.DefaultValueOptions.Value.IsNotNull and not lColumn.ColumnType.IsObject then
    begin
      if lColumn.DefaultValueOptions.IsNow and (lColumn.ColumnType.IsDateTime) then
      begin
        lNow := Now;
        lColumn.RttiOptions.RttiField.SetValue(pObjectInstance, TValue.From<TDateTime>(lNow));
        lColumn.ColumnValue := lNow;
      end
      else
      begin
        if lColumn.ColumnType.IsOrdinal then
        begin
          if lColumn.ColumnType = ctyBoolean then
          begin
            if (not lColumn.IsNullable) then
            begin
              lValue := TValue.From<Boolean>(lColumn.DefaultValueOptions.Value);
            end
            else
            begin
              case lColumn.DefaultValueOptions.Value.TryToBoolean of
                True:
                  if (TBooleanColumnMapper(lColumn).InternalColumnType = ctyString) then
                  begin
                   lValue := TValue.From<String>(TBooleanColumnMapper(lColumn).ValueTrue);
                  end
                  else
                  begin
                    lValue := TValue.From<Integer>(TBooleanColumnMapper(lColumn).ValueTrue);
                  end;
                False:
                  if (TBooleanColumnMapper(lColumn).InternalColumnType = ctyString) then
                  begin
                   lValue := TValue.From<String>(TBooleanColumnMapper(lColumn).ValueFalse);
                  end
                  else
                  begin
                    lValue := TValue.From<Integer>(TBooleanColumnMapper(lColumn).ValueFalse);
                  end;
              end;
            end;
          end
          else
          begin
            lValue := TValue.FromOrdinal(lColumn.RttiOptions.RttiField.FieldType.Handle, Int64(lColumn.DefaultValueOptions.Value));
          end;
          if (not lColumn.IsNullable) then
          begin
            lColumn.RttiOptions.RttiField.SetValue(pObjectInstance, lValue);
          end
          else
          begin
            lColumn.RttiOptions.RttiFieldHelper.SetNullableVariantValue(pObjectInstance, lValue.AsVariant);
          end;
          lColumn.ColumnValue := lValue.AsVariant;
        end
        else
        begin
          if (not lColumn.IsNullable) then
          begin
            lColumn.RttiOptions.RttiField.SetValue(pObjectInstance, TValue.FromVariant(lColumn.DefaultValueOptions.Value));
          end
          else
          begin
            lColumn.RttiOptions.RttiFieldHelper.SetNullableVariantValue(pObjectInstance, lColumn.DefaultValueOptions.Value);
          end;
          lColumn.ColumnValue := lColumn.DefaultValueOptions.Value;
        end;
      end;
    end;
  end;
end;

class procedure TMapperToObject.WriteValuesForNullableColumns;
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in FObjectMapper.Columns.NullableColumns do
  begin
    lColumn.RttiOptions.RttiFieldHelper.SetNullableVariantValue(FObjectInstance, lColumn.ColumnValue);
  end;
end;

class procedure TMapperToObject.WriteValuesForOrdinalColumns;
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in FObjectMapper.Columns.OrdinalColumns do
  begin
    if lColumn.ColumnValue.IsNotNull then
    begin
      if (not lColumn.IsNullable) then
      begin
        lColumn.RttiOptions.RttiField.SetValue(FObjectInstance, lColumn.AsTValue);
      end;
    end;
  end;
end;

class procedure TMapperToObject.WriteValuesForSimpleColumns;
var
  lColumn: TCustomColumnMapper;
  lObjectInstance: TObject;
begin
  for lColumn in FObjectMapper.Columns.SimpleColumns do
  begin
    if lColumn.ColumnValue.IsNotNull then
    begin
      lObjectInstance := FObjectInstance;
      if (lColumn.RttiOptions.RttiObject <> nil) and (lObjectInstance <> lColumn.RttiOptions.RttiObject) then
      begin
        lObjectInstance := lColumn.RttiOptions.RttiObject;
      end;
      if (not lColumn.IsNullable) then
      begin
        lColumn.RttiOptions.RttiField.SetValue(lObjectInstance, lColumn.AsTValue);
      end;
    end;
  end;
end;

class function TMapperToObject.CanPersistList(pFreedomList: IFreedomObjectList): Boolean;
begin
  Result := not pFreedomList.IsEmpty;
  if Result then
  begin
    case FObjectState of
      Clean:
        if (TColumnOption.NoUpdate in FColumn.ColumnOptions) then
          Result := False;
      Inserted:
        if (TColumnOption.NoInsert in FColumn.ColumnOptions) then
          Result := False;
      Deleted:
        if (TColumnOption.NoDelete in FColumn.ColumnOptions) then
          Result := False;
    end;
  end;
end;

class function TMapperToObject.CanSetLazySearchForColumn: Boolean;
begin
  Result := Assigned(FObjectInstance) and (FColumn.ColumnType in [ctyJoin, ctyDetail])
    and (TReferenceColumnMapper(FColumn).RefColumnName <> EmptyStr)
    and not FColumn.ColumnValue.IsNull;

end;

class function TMapperToObject.CreateArgumentForReferenceColumn: TCustomArgument;
begin
  case TReferenceColumnMapper(FColumn).RefColumnType of
    ctyByte, ctySmallint:
      Result := TValueArgument.CreateAsSmallint(FColumn.ColumnValue);
    ctyInteger:
      Result := TValueArgument.CreateAsInteger(FColumn.ColumnValue);
    ctyInt64:
      Result := TValueArgument.CreateAsInt64(FColumn.ColumnValue);
    ctyChar, ctyString:
      Result := TValueArgument.CreateAsString(FColumn.ColumnValue);
    ctySingle, ctyDouble:
      Result := TValueArgument.CreateAsDouble(FColumn.ColumnValue);
    ctyCurrency:
      Result := TValueArgument.CreateAsCurrency(FColumn.ColumnValue);
    ctyExtended:
      Result := TValueArgument.CreateAsExtended(FColumn.ColumnValue);
    ctyDate:
      Result := TValueArgument.CreateAsDate(FColumn.ColumnValue);
    ctyTime:
      Result := TValueArgument.CreateAsTime(FColumn.ColumnValue);
    ctyDateTime:
      Result := TValueArgument.CreateAsDateTime(FColumn.ColumnValue);
  else
    raise EInvalidReferencedColumnColumnType.Create(FColumn.Name, TReferenceColumnMapper(FColumn).RefColumnType.ToString,
          FObjectMapper.MetaClassType.ClassName);
  end;
end;

class function TMapperToObject.CreateCriteriaForReferenceColumnMapper: TCriteria;
var
  lAlias: string;
begin
  lAlias := TReferenceColumnMapper(FColumn).RefObjectAlias;
//  if FColumn.DuplicatedAlias then
//    lAlias := FObjectMapper.Alias;
  Result := TCriteria.CreateAsEqual(TFieldArgument.Create(TReferenceColumnMapper(FColumn).RefColumnName,
      lAlias), CreateArgumentForReferenceColumn);
end;

class procedure TMapperToObject.DoInternalWriteEntityIfKnownObjectState(pFreedomObject: IFreedomObject);
begin
  case FObjectState of
    Clean:
      if not(TColumnOption.NoUpdate in FColumn.ColumnOptions) then
      begin
        pFreedomObject.Persist(Clean, FPersistent);
      end;
    Inserted:
      if not(TColumnOption.NoInsert in FColumn.ColumnOptions) then
      begin
        pFreedomObject.Persist(Inserted, FPersistent);
      end;
    Deleted:
      if not(TColumnOption.NoDelete in FColumn.ColumnOptions) then
      begin
        pFreedomObject.Persist(Deleted, FPersistent);
      end;
  end;
end;

class procedure TMapperToObject.DoInternalWriteEntityIfObjectAssigned(pFreedomObject: IFreedomObject; pObject: TObject);
begin
  if FObjectState = TObjectState.Unknown then
  begin
    DoInternalWriteEntityIfUnknownObjectState(pFreedomObject);
  end
  else
  begin
    DoInternalWriteEntityIfKnownObjectState(pFreedomObject);
  end;
end;

class procedure TMapperToObject.DoInternalWriteEntityIfUnknownObjectState(pFreedomObject: IFreedomObject);
var
  lGroupCriteria: TGroupCriteria;
begin
  lGroupCriteria := TGroupCriteria.Create;
  lGroupCriteria.AddCriteria(CreateCriteriaForReferenceColumnMapper);
  pFreedomObject.DoSearch(lGroupCriteria);
  FreeAndNil(lGroupCriteria);
end;

class procedure TMapperToObject.DoInternalWriteListIfKnownObjectState(pFreedomList: IFreedomObjectList);
begin
  if CanPersistList(pFreedomList) then
  begin
    pFreedomList.PersistObjects;
  end;
end;

class procedure TMapperToObject.DoInternalWriteListIfUnknownObjectState(pFreedomList: IFreedomObjectList);
var
  lGroupCriteria: TGroupCriteria;
begin
  if not VarIsNull(FColumn.ColumnValue) then
  begin
    lGroupCriteria := TGroupCriteria.Create;
    lGroupCriteria.AddCriteria(CreateCriteriaForReferenceColumnMapper);
    pFreedomList.DoSearch(lGroupCriteria);
    FreeAndNil(lGroupCriteria);
  end;
end;

class procedure TMapperToObject.DoSetLazySearch(pLazy: ILazy);
var
  lLazySearch: TGroupCriteria;
begin
  lLazySearch := TGroupCriteria.Create;
  lLazySearch.AddCriteria(CreateCriteriaForReferenceColumnMapper);
  pLazy.SetLazySearch(lLazySearch);
  pLazy.SetLazyID(FColumn.ColumnValue);
end;

class procedure TMapperToObject.DoWriteBlobs;
var
  lObject: TObject;
begin
  if not FColumn.LazyOptions.IsLazy then
  begin
    lObject := FColumn.RttiOptions.RttiFieldHelper.GetObjectValue(FObjectInstance);
    if Assigned(lObject) then
    begin
      if lObject.InheritsFrom(TStrings) then
      begin
        TBlobColumnMapper(FColumn).StreamColumnValue.Position := 0;
        TStrings(lObject).LoadFromStream(TBlobColumnMapper(FColumn).StreamColumnValue);
      end
      else
      begin
        TStream(lObject).CopyFrom(TBlobColumnMapper(FColumn).StreamColumnValue, 0);
      end;
    end;
  end
  else
  begin
    SetLazySearch;
  end;
end;

class procedure TMapperToObject.DoWriteJoinColumn;
begin
  if not FColumn.LazyOptions.IsLazy then
  begin
    if FColumn.RttiOptions.RttiField.FieldType.IsInstance then
    begin
      DoWriteJoins;
    end else
    begin
      FColumn.RttiOptions.RttiField.SetValue(FColumn.RttiOptions.RttiObject, TValue.FromVariant(FColumn.ColumnValue));
    end;
  end
  else
  begin
    SetLazySearch;
  end;
end;

class procedure TMapperToObject.DoWriteJoins;
var
  AClass: TClass;
  lObject: TObject;
begin
  AClass := FColumn.RttiOptions.RttiField.FieldType.AsInstance.MetaclassType;
  lObject := FColumn.RttiOptions.RttiFieldHelper.GetObjectValue(FObjectInstance);
  if Supports(AClass, IFreedomObjectList) then
  begin
    InternalWriteList(lObject);
  end
  else if Supports(AClass, IFreedomObject) then
  begin
    InternalWriteEntity(lObject);
  end;
end;

class procedure TMapperToObject.DoWriteList;
begin
  if not FColumn.LazyOptions.IsLazy then
  begin
    InternalWriteList(FColumn.RttiOptions.RttiFieldHelper.GetObjectValue(FObjectInstance));
  end
  else
  begin
    SetLazySearch;
  end;
end;

class procedure TMapperToObject.FinalizeInternalFields;
begin
  FObjectInstance := nil;
  FObjectMapper := nil;
  FPersistent := nil;
  FColumn := nil;
end;

class function TMapperToObject.GetInterfaceLazyFromColumn: ILazy;
var
  lObject: TObject;
begin
  Result := nil;
  lObject := FColumn.RttiOptions.RttiFieldHelper.GetObjectValue(FObjectInstance);
  if Assigned(lObject) then
  begin
    Supports(lObject, ILazy, Result);
  end;
end;

class procedure TMapperToObject.InternalWriteEntity(pObject: TObject);
var
  lFreedomObject: IFreedomObject;
begin
  if Assigned(pObject) and Supports(pObject, IFreedomObject, lFreedomObject) then
  begin
    DoInternalWriteEntityIfObjectAssigned(lFreedomObject, pObject);
  end;
end;

class procedure TMapperToObject.InternalWriteList(pObject: TObject);
var
  lFreedomList: IFreedomObjectList;
begin
  if Assigned(pObject) and Supports(pObject, IFreedomObjectList, lFreedomList) then
  begin
    if FObjectState = TObjectState.Unknown then
    begin
      DoInternalWriteListIfUnknownObjectState(lFreedomList);
    end
    else
    begin
      DoInternalWriteListIfKnownObjectState(lFreedomList);
    end;
  end;
end;

class procedure TMapperToObject.SetLazySearch;
var
  lLazy: ILazy;
begin
  if CanSetLazySearchForColumn then
  begin
    lLazy := GetInterfaceLazyFromColumn;
    if Assigned(lLazy) then
    begin
      DoSetLazySearch(lLazy);
    end;
  end;
end;

class procedure TMapperToObject.WriteBlobs;
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in FObjectMapper.Columns.BlobColumns do
  begin
    FColumn := lColumn;
    DoWriteBlobs;
  end;
end;

class procedure TMapperToObject.WriteBooleanValuesToNullables(pMapper: TObjectMapper);
var
  lColumn: TCustomColumnMapper;
  lINullable: INullable;
  lObject: TObject;
begin
  for lColumn in pMapper.Columns.NullableColumns do
  begin
    if (lColumn.InheritsFrom(TBooleanColumnMapper) and lColumn.IsNullable) then
    begin
      lObject := lColumn.RttiOptions.RttiFieldHelper.GetObjectValue(lColumn.RttiOptions.RttiObject);
      Supports(lObject, INullable, lINullable);
      lINullable.BooleanValueOptions.ValueTrue := TBooleanColumnMapper(lColumn).ValueTrue;
      lINullable.BooleanValueOptions.ValueFalse := TBooleanColumnMapper(lColumn).ValueFalse;
      lINullable.BooleanValueOptions.ValueType := TBooleanColumnMapper(lColumn).InternalColumnType;
    end;
  end;
end;

class procedure TMapperToObject.WriteInternalObjects;
begin
  WriteBlobs;
  WriteJoins;
  WriteList;
end;

class procedure TMapperToObject.WriteJoins;
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in FObjectMapper.Columns.JoinColumns do
  begin
    FColumn := lColumn;
    DoWriteJoinColumn;
  end;
end;

class procedure TMapperToObject.WriteList;
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in FObjectMapper.Columns.DetailColumns do
  begin
    FColumn := lColumn;
    DoWriteList;
  end;
end;

end.
