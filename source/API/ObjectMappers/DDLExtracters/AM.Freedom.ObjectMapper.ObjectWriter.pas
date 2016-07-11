unit AM.Freedom.ObjectMapper.ObjectWriter;

interface

uses
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.ObjectMapper,
  AM.Freedom.Helper.RttiField,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.ICursor,
  AM.Freedom.ObjectMapper.ColumnOptions,
  AM.Freedom.ObjectMapper.ColumnValueItem,
  AM.Freedom.ILazy,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.IFreedomObjectList,
  AM.Freedom.IFreedomObject;

type
  TObjectWriter = class sealed
  strict private
    FCursor: ICursor;
    FMapper: TObjectMapper;
    FSchema: string;
    FColumnValueList: TColumnValueList;
    FColumn: TCustomColumnMapper;
    FColumnValueItem: TColumnValueItem;
    procedure Read;
    procedure WriteBlobValue;
    procedure WriteJoins;
    procedure SetLazySearch;
    function CanSetLazySearchForColumn: Boolean;
    function GetInterfaceLazyFromColumn: ILazy;
    procedure DoSetLazySearch(pLazy: ILazy);
    function CreateCriteriaForReferenceColumnMapper: TCriteria;
    function CreateArgumentForReferenceColumn: TCustomArgument;
    procedure WriteJoinColumnValue;
    procedure DoWriteJoinValue;
    procedure InternalWriteList(pObject: TObject);
    procedure InternalWriteObject(pObject: TObject);
    function DoRoundValue(pValue: Extended; pColumn: TCustomColumnMapper): Variant;
    procedure WriteValue;
  public
    class procedure PersistentCursorToObject(pCursor: ICursor; pObjectMapper: TObjectMapper; pSchema: string;
        pColumnValueList: TColumnValueList);
    class procedure PersistentRecordToObject(pCursor: ICursor; pObjectMapper: TObjectMapper; pSchema: string;
        pColumnValueList: TColumnValueList);
    procedure ReadRecord;
    property Cursor: ICursor read FCursor write FCursor;
    property Mapper: TObjectMapper read FMapper write FMapper;
    property Schema: string read FSchema write FSchema;
    property ColumnValueList: TColumnValueList read FColumnValueList write FColumnValueList;
  end;

implementation

uses
  System.Variants,
  System.SysUtils,
  System.Classes,
  AM.Freedom.DefaultsClassRegister,
  AM.Freedom.CustomNullable,
  AM.Freedom.Helper.Variant,
  AM.Freedom.NullableCompare,
  AM.Freedom.RoundObject,
  AM.Freedom.CustomRounder,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.Exceptions,
  AM.Freedom.GroupCriteria,
  System.Rtti;
{ TCursorReader }

function TObjectWriter.CanSetLazySearchForColumn: Boolean;
begin
  Result := (FColumn.RttiOptions.RttiObject <> nil) and (FColumn.ColumnType in [ctyJoin, ctyDetail]) and
      (TReferenceColumnMapper(FColumn).RefColumnName <> '') and not FColumnValueItem.ColumnValue.IsNull;
end;

function TObjectWriter.CreateArgumentForReferenceColumn: TCustomArgument;
begin
  case TReferenceColumnMapper(FColumn).RefColumnType of
    ctyByte, ctySmallint:
      Result := TValueArgument.CreateAsSmallint(FColumnValueItem.ColumnValue);
    ctyInteger:
      Result := TValueArgument.CreateAsInteger(FColumnValueItem.ColumnValue);
    ctyInt64:
      Result := TValueArgument.CreateAsInt64(FColumnValueItem.ColumnValue);
    ctyChar, ctyString:
      Result := TValueArgument.CreateAsString(FColumnValueItem.ColumnValue);
    ctySingle, ctyDouble:
      Result := TValueArgument.CreateAsDouble(FColumnValueItem.ColumnValue);
    ctyCurrency:
      Result := TValueArgument.CreateAsCurrency(FColumnValueItem.ColumnValue);
    ctyExtended:
      Result := TValueArgument.CreateAsExtended(FColumnValueItem.ColumnValue);
    ctyDate:
      Result := TValueArgument.CreateAsDate(FColumnValueItem.ColumnValue);
    ctyTime:
      Result := TValueArgument.CreateAsTime(FColumnValueItem.ColumnValue);
    ctyDateTime:
      Result := TValueArgument.CreateAsDateTime(FColumnValueItem.ColumnValue);
  else
    raise EInvalidReferencedColumnColumnType.Create(FColumn.Name, TReferenceColumnMapper(FColumn).RefColumnType.ToString,
        FMapper.MetaClassType.ClassName);
  end;
end;

function TObjectWriter.CreateCriteriaForReferenceColumnMapper: TCriteria;
var
  lAlias: string;
begin
  lAlias := TReferenceColumnMapper(FColumn).OriginalRefObjectAlias;
  Result := TCriteria.CreateAsEqual(TFieldArgument.Create(TReferenceColumnMapper(FColumn).RefColumnName, lAlias), CreateArgumentForReferenceColumn);
end;

function TObjectWriter.DoRoundValue(pValue: Extended; pColumn: TCustomColumnMapper): Variant;
var
  lRoundObject: TRoundObject;
  lRounder: TCustomRounder;
begin
  lRoundObject := TRoundObject.Create;
  try
    lRoundObject.RoundValue := pValue;
    lRoundObject.RoundDecimals := pColumn.RoundOptions.RoundDecimals;
    lRoundObject.RoundDecimalsMode := pColumn.RoundOptions.RoundDecimalsMode;
    lRoundObject.ColumnName := pColumn.Name;
    lRoundObject.ObjectClass := pColumn.RttiOptions.RttiObject.ClassType;
    if (pColumn.RoundOptions.CanBeModified) then
    begin
      TDefaultsClassRegister.DefaultPersistent.ModifyRoundObject(lRoundObject);
    end;
    try
      if (lRoundObject.Handled) then
      begin
        Result := lRoundObject.RoundValue;
      end
      else
      begin
        lRounder := TDefaultsClassRegister.DefaultRounderClass.Create;
        try
          Result := lRounder.RoundValue(lRoundObject);
        finally
          lRounder.Free;
        end;
      end;
    except
    end;
  finally
    FreeAndNil(lRoundObject);
  end;
end;

procedure TObjectWriter.DoSetLazySearch(pLazy: ILazy);
var
  lLazySearch: TGroupCriteria;
begin
  lLazySearch := TGroupCriteria.Create;
  lLazySearch.AddCriteria(CreateCriteriaForReferenceColumnMapper);
  pLazy.SetLazySearch(lLazySearch);
  pLazy.SetLazyID(FColumnValueItem.ColumnValue);
end;

procedure TObjectWriter.DoWriteJoinValue;
var
  AClass: TClass;
  lObject: TObject;
begin
  AClass := FColumn.RttiOptions.RttiField.FieldType.AsInstance.MetaClassType;
  lObject := FColumn.RttiOptions.RttiFieldHelper.GetObjectValue(FColumn.RttiOptions.RttiObject);
  if (Assigned(lObject)) then
  begin
    if Supports(AClass, IFreedomObjectList) then
    begin
      InternalWriteList(lObject);
    end
    else if Supports(AClass, IFreedomObject) then
    begin
      InternalWriteObject(lObject);
    end;
  end
  else
  begin
    raise EFieldValueCannotBeAssignedOnClass.Create(FColumn.RttiOptions.RttiField.Name, AClass.ClassName,
        FColumn.RttiOptions.RttiObject.ClassName);
  end;
end;

function TObjectWriter.GetInterfaceLazyFromColumn: ILazy;
var
  lObject: TObject;
  lFieldClassName: string;
begin
  Result := nil;
  lObject := FColumn.RttiOptions.RttiField.GetValue(FColumn.RttiOptions.RttiObject).AsObject;
  if Assigned(lObject) then
  begin
    lObject.GetInterface(ILazy, Result);
  end
  else
  begin
    case FColumn.LazyOptions.LazyType of
      Simple: lFieldClassName := 'TLazy';
      List: lFieldClassName := 'TLazyList';
    end;
    raise EFieldValueCannotBeAssignedOnClass.Create(FColumn.RttiOptions.RttiField.Name, lFieldClassName,
        FColumn.RttiOptions.RttiObject.ClassName);
  end;
end;

procedure TObjectWriter.InternalWriteList(pObject: TObject);
var
  lFreedomList: IFreedomObjectList;
  lGroupCriteria: TGroupCriteria;
begin
  if Assigned(pObject) and pObject.GetInterface(IFreedomObjectList, lFreedomList) then
  begin
    if not VarIsNull(FColumnValueItem.ColumnValue) then
    begin
      lGroupCriteria := TGroupCriteria.Create;
      lGroupCriteria.AddCriteria(CreateCriteriaForReferenceColumnMapper);
      lFreedomList.DoSearch(lGroupCriteria);
      FreeAndNil(lGroupCriteria);
    end;
  end;
end;

procedure TObjectWriter.InternalWriteObject(pObject: TObject);
var
  lFreedomObject: IFreedomObject;
  lGroupCriteria: TGroupCriteria;
begin
  if Assigned(pObject) and pObject.GetInterface(IFreedomObject, lFreedomObject) then
  begin
    lGroupCriteria := TGroupCriteria.Create;
    lGroupCriteria.AddCriteria(CreateCriteriaForReferenceColumnMapper);
    lFreedomObject.DoSearch(lGroupCriteria);
    FreeAndNil(lGroupCriteria);
  end;
end;

class procedure TObjectWriter.PersistentCursorToObject(pCursor: ICursor; pObjectMapper: TObjectMapper; pSchema: string;
    pColumnValueList: TColumnValueList);
var
  lCursorReader: TObjectWriter;
begin
  lCursorReader := TObjectWriter.Create;
  try
    lCursorReader.Mapper := pObjectMapper;
    lCursorReader.Cursor := pCursor;
    lCursorReader.Schema := pSchema;
    lCursorReader.ColumnValueList := pColumnValueList;
    lCursorReader.Read;
  finally
    lCursorReader.Free;
    pCursor := nil;
  end;
end;

class procedure TObjectWriter.PersistentRecordToObject(pCursor: ICursor; pObjectMapper: TObjectMapper; pSchema: string;
    pColumnValueList: TColumnValueList);
var
  lCursorReader: TObjectWriter;
begin
  lCursorReader := TObjectWriter.Create;
  try
    lCursorReader.Mapper := pObjectMapper;
    lCursorReader.Cursor := pCursor;
    lCursorReader.Schema := pSchema;
    lCursorReader.ColumnValueList := pColumnValueList;
    lCursorReader.ReadRecord;
  finally
    lCursorReader.Free;
  end;
end;

procedure TObjectWriter.Read;
begin
  FCursor.First;
  if not FCursor.IsEmpty then
  begin
    ReadRecord;
  end;
end;

procedure TObjectWriter.ReadRecord;
var
  lColumn: TCustomColumnMapper;
  lColumnValue: Variant;
begin
  if not FMapper.IsList then
  begin
    FColumnValueList.Clear;
    for lColumn in FMapper.Columns do
    begin
      lColumnValue := Null;
      FColumn := lColumn;
      if (not lColumn.IsExtension) and ((FSchema = '') or (lColumn.Schemas.FindSchema(FSchema) <> nil)) then
      begin
        if lColumn.ColumnType.IsBlob then
        begin
          FColumnValueItem := FColumnValueList.AddValue(lColumn.Name, Null, lCOlumn.ColumnType, FCursor.StreamValue[lColumn.Name]);
          WriteBlobValue;
        end
        else
        begin
          if lColumn.ColumnType in [ctyJoin, ctyDetail] then
          begin
            WriteJoins;
          end
          else
          begin
            lColumnValue := FCursor.Values[lColumn.Name];
            if (lColumn.ColumnType.IsFloat) and lColumn.RoundOptions.IsRounded and not VarIsNull(lColumnValue) then
            begin
              lColumnValue := DoRoundValue(lColumnValue, lColumn);
            end;
            FColumnValueItem := FColumnValueList.AddValue(lColumn.Name, lColumnValue, lColumn.ColumnType);
            WriteValue;
          end;
        end;
      end;
    end;
  end;
end;

procedure TObjectWriter.SetLazySearch;
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

procedure TObjectWriter.WriteBlobValue;
var
  lObject: TObject;
begin
  if not FColumn.LazyOptions.IsLazy then
  begin
    lObject := FColumn.RttiOptions.RttiField.GetValue(FColumn.RttiOptions.RttiObject).AsObject;
    if Assigned(lObject) then
    begin
      if lObject.InheritsFrom(TStrings) then
      begin
        FColumnValueItem.StreamColumnValue.Position := 0;
        TStrings(lObject).LoadFromStream(FColumnValueItem.StreamColumnValue);
      end
      else
      begin
        FColumnValueItem.StreamColumnValue.Position := 0;
        TStream(lObject).CopyFrom(FColumnValueItem.StreamColumnValue, 0);
      end;
    end
    else
    begin
      raise EFieldValueCannotBeAssignedOnClass.Create(FColumn.RttiOptions.RttiField.Name,
        FColumn.RttiOptions.RttiField.FieldType.AsInstance.MetaclassType.ClassName,
        FColumn.RttiOptions.RttiObject.ClassName);
    end;
  end
  else
  begin
    SetLazySearch;
  end;
end;

procedure TObjectWriter.WriteJoinColumnValue;
begin
  if (FColumnValueItem.ColumnValue.IsNotNull) then
  begin
    if not FColumn.LazyOptions.IsLazy then
    begin
      if FColumn.RttiOptions.RttiField.FieldType.IsInstance then
      begin
        DoWriteJoinValue;
      end
      else
      begin
        if (FColumnValueItem.ColumnValue.IsNotNull) then
        begin
          FColumn.RttiOptions.RttiFieldHelper.SetVariantValue(FColumn.RttiOptions.RttiObject, FColumnValueItem.ColumnValue);
        end;
      end;
    end
    else
    begin
      SetLazySearch;
    end;
  end;
end;

procedure TObjectWriter.WriteJoins;
var
  lName: string;
begin
  if FColumn.RttiOptions.RttiField.FieldType.IsInstance then
  begin
    lName := FColumn.Name;
  end
  else
  begin
    lName := TJoinedColumnMapper(FColumn).RefResultColumnName;
    if (FColumn.Alias <> '') then
    begin
      lName := FColumn.Alias;
    end;
  end;
  FColumnValueItem := FColumnValueList.AddValue(lName, FCursor.Values[lName], FColumn.ColumnType);
  WriteJoinColumnValue;
end;

procedure TObjectWriter.WriteValue;
begin
  if (FColumnValueItem.ColumnValue.IsNotNull) then
  begin
    if (FColumn.IsNullable) then
    begin
      FColumn.RttiOptions.RttiFieldHelper.SetNullableVariantValue(FColumn.RttiOptions.RttiObject, FColumnValueItem.ColumnValue);
    end
    else
    begin
      FColumn.ColumnValue := FColumnValueItem.ColumnValue;
      FColumn.RttiOptions.RttiField.SetValue(FColumn.RttiOptions.RttiObject, FColumn.AsTValue);
    end;
  end;
end;

end.
