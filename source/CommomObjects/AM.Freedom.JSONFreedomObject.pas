unit AM.Freedom.JSONFreedomObject;

interface

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.InterfacedObjects,
  Data.DBXJSONReflect,
  System.JSON;

type
  TJSONFreedomObjectClass = class of TJSONFreedomObject;

  TJSONFreedomObject = class(TFreedomInterfacedObject)
  strict private
    class procedure DoGetConverters(pJSONMarshal: TJSONMarshal);
    class procedure DoBeforeMarshal(pJSONMarshal: TJSONMarshal);
    class procedure DoAfterMarshal(pJSONMarshal: TJSONMarshal);
    class procedure DoGetReverters(pJSONUnMarshal: TJSONUnMarshal);

    class function GetClassFromTypeName(pTypeName: String): TClass;
    class procedure DoBeforeUnMarshal(pJSONUnMarshal: TJSONUnMarshal);
    class procedure DoAfterUnMarshal(pObject: TObject);
    class function ToByteArray(pStream: TStream): TBytes;
    class function ByteArrayToString(pBytes: TBytes): String;
    class function ByteArrayFromString(pString: String): TBytes;
  strict protected
    class procedure GetConverters(pJSONMarshal: TJSONMarshal); virtual;
    class procedure BeforeMarshal(pJSONMarshal: TJSONMarshal); virtual;
    class procedure AfterMarshal(pJSONMarshal: TJSONMarshal); virtual;
    class procedure GetReverters(pJSONUnMarshal: TJSONUnMarshal); virtual;
    class procedure BeforeUnMarshal(pJSONUnMarshal: TJSONUnMarshal); virtual;
    class procedure AfterUnMarshal(pObject: TObject); virtual;
    class function StringToByteArrayString(pString: String): String;
    class function StringFromByteArrayString(pString: String): String;
  public
    class function ToJSON(pObject: TJSONFreedomObject): TJSONValue; overload;
    function ToJSON: TJSONValue; overload;
    class function FromJSON(pJSONValue: TJSONValue): TObject; overload;
    class function FromJSON<T: class>(pJSONValue: TJSONValue): T; overload;
  end;

implementation

uses
  System.Rtti,
  System.Variants,
  System.StrUtils,
  AM.Freedom.ObjectMapper.ColumnValueItem,
  AM.Freedom.ILazy,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Lazy,
  AM.Freedom.LazyList,
  AM.Freedom.FreedomObject,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.GroupCriteria,
  AM.Freedom.FreedomObjectList,
  AM.Freedom.IFreedomObjectList, AM.Freedom.Helper.Variant;

{ TBaseFreedomObject }

class procedure TJSONFreedomObject.AfterMarshal(pJSONMarshal: TJSONMarshal);
begin
  // override when necessary
end;

class procedure TJSONFreedomObject.AfterUnMarshal(pObject: TObject);
begin
  // override when necessary
end;

class procedure TJSONFreedomObject.BeforeMarshal(pJSONMarshal: TJSONMarshal);
begin
  // override when necessary
end;

class procedure TJSONFreedomObject.BeforeUnMarshal(pJSONUnMarshal: TJSONUnMarshal);
begin
  // override when necessary
end;

class function TJSONFreedomObject.ByteArrayFromString(pString: String): TBytes;
var
  lStrings: TStrings;
  lIndex: Integer;
begin
  lStrings := TStringList.Create;
  try
    ExtractStrings([',', ';'], [' '], PWideChar(pString), lStrings);
    SetLength(Result, lStrings.Count);
    for lIndex := 0 to lStrings.Count - 1 do
    begin
      Result[lIndex] := StrToInt(lStrings.Strings[lIndex]);
    end;
  finally
    lStrings.Free;
  end;
end;

class function TJSONFreedomObject.ByteArrayToString(pBytes: TBytes): String;
var
  lIndex: Integer;
begin
  Result := '';
  if (Length(pBytes) > 0) then
  begin
    for lIndex := Low(pBytes) to High(pBytes) do
    begin
      Result := Result + ifthen(Result <> '', ';') + pBytes[lIndex].ToString;
    end;
  end;
end;

class procedure TJSONFreedomObject.DoAfterMarshal(pJSONMarshal: TJSONMarshal);
begin
  AfterMarshal(pJSONMarshal);
end;

class procedure TJSONFreedomObject.DoAfterUnMarshal(pObject: TObject);
begin
  AfterUnMarshal(pObject);
end;

class procedure TJSONFreedomObject.DoBeforeMarshal(pJSONMarshal: TJSONMarshal);
begin
  BeforeMarshal(pJSONMarshal);
end;

class procedure TJSONFreedomObject.DoBeforeUnMarshal(pJSONUnMarshal: TJSONUnMarshal);
begin
  BeforeUnMarshal(pJSONUnMarshal);
end;

class procedure TJSONFreedomObject.DoGetConverters(pJSONMarshal: TJSONMarshal);
var
  lRttiType: TRttiType;
  lRttiField: TRTTIField;
  lRttiContext: TRTTIContext;
  lVarConverter: TStringsConverter;
  lStreamConverter: TStringsConverter;
  lStringsConverter: TStringsConverter;
  lColumnValueListConverter: TStringsConverter;
  lLazyConverter: TStringsConverter;
  lJSONFreedomObjectConverter: TStringsConverter;
  lGroupCriteriaConverter: TStringConverter;
  lFreedomObjectListConverter: TStringsConverter;

begin
  {$REGION 'Variant Converter'}
  lVarConverter := function(Data: TObject; Field: string): TListOfStrings
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
    lValue: Variant;
  begin
    SetLength(Result, 2);
    Result[0] := EmptyStr;
    Result[1] := EmptyStr;
    lType := lContext.GetType(Data.ClassType);
    lField := lType.GetField(Field);
    if Assigned(lField) then
    begin
      lValue := lField.GetValue(Data).AsVariant;
      Result[0] := VarTypeAsText(VarType(lValue));
      case lValue.VariantType of
        ctyDateTime: Result[0] := 'DateTime';
        ctyTime: Result[0] := 'Time';
        ctyDate: Result[0] := 'Date';
      end;
      Result[1] := VarToStr(lValue);
    end;
  end;
  {$ENDREGION}
  {$REGION 'Stream converter'}
  lStreamConverter := function(Data: TObject; Field: string): TListOfStrings
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
    lStream: TStream;
    lBytes: TBytes;
  begin
    SetLength(Result, 2);
    Result[0] := EmptyStr;
    Result[1] := EmptyStr;
    lType := lContext.GetType(Data.ClassType);
    lField := lType.GetField(Field);
    if Assigned(lField) then
    begin
      Result[0] := lField.FieldType.AsInstance.MetaclassType.QualifiedClassName;
      lStream := TStream(lField.GetValue(Data).AsObject);
      if (Assigned(lStream)) then
      begin
        Result[0] := lStream.QualifiedClassName;
        lBytes := TJSONFreedomObject.ToByteArray(lStream);
        Result[1] := TJSONFreedomObject.ByteArrayToString(lBytes);
      end;
    end;
  end;
  {$ENDREGION}
  {$REGION 'Strings Converter'}
  lStringsConverter := function(Data: TObject; Field: string): TListOfStrings
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
    lStream: TStream;
    lStrings: TStrings;
    lBytes: TBytes;
  begin
    SetLength(Result, 2);
    Result[0] := EmptyStr;
    Result[1] := EmptyStr;
    lType := lContext.GetType(Data.ClassType);
    lField := lType.GetField(Field);
    if Assigned(lField) then
    begin
      Result[0] := lField.FieldType.AsInstance.MetaclassType.QualifiedClassName;
      lStrings := TStrings(lField.GetValue(Data).AsObject);
      if (Assigned(lStrings)) then
      begin
        Result[0] := lStrings.QualifiedClassName;
        lStream := TStringStream.Create;
        try
          lStrings.SaveToStream(lStream);
          lBytes := TJSONFreedomObject.ToByteArray(lStream);
          Result[1] := TJSONFreedomObject.ByteArrayToString(lBytes);
        finally
          lStream.Free;
        end;
      end;
    end;
  end;
  {$ENDREGION}
  {$REGION 'ColumnValueList Converter'}
  lColumnValueListConverter := function(Data: TObject; Field: string): TListOfStrings
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
    lColumnValueList: TColumnValueList;
    lIndex: Integer;
    lJSONValue: TJSONValue;
  begin
    lType := lContext.GetType(Data.ClassType);
    lField := lType.GetField(Field);
    if Assigned(lField) then
    begin
      lColumnValueList := TColumnValueList(lField.GetValue(Data).AsObject);
      SetLength(Result, 0);
      if (Assigned(lColumnValueList)) then
      begin
        SetLength(Result, lColumnValueList.Count);
        for lIndex := 0 to lColumnValueList.Count - 1 do
        begin
          lJSONValue := lColumnValueList.Items[lIndex].ToJSON;
          try
            Result[lIndex] := StringToByteArrayString(lJSONValue.ToString);
          finally
            lJSONValue.Free;
          end;
        end;
      end;
    end;
  end;
  {$ENDREGION}
  {$REGION 'Lazy Converter'}
  lLazyConverter :=  function(Data: TObject; Field: string): TListOfStrings
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
    lObject: TObject;
    lLazy: ILazy;
  begin
    SetLength(Result, 2);
    Result[0] := '';
    Result[1] := '';
    lType := lContext.GetType(Data.ClassType);
    lField := lType.GetField(Field);
    if Assigned(lField) then
    begin
      Result[0] := lField.FieldType.AsInstance.MetaclassType.QualifiedClassName;
      lObject := lField.GetValue(Data).AsObject;
      if (Assigned(lObject) and Supports(lObject, ILazy, lLazy)) then
      begin
        Result[0] := lObject.QualifiedClassName;
        Result[1] := StringToByteArrayString(TJSONFreedomObject(lObject).ToJSON.ToString);
      end;
    end;
  end;
  {$ENDREGION}
  {$REGION 'JSONFreedomObject Converter'}
  lJSONFreedomObjectConverter := function(Data: TObject; Field: string): TListOfStrings
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
    lObject: TObject;
  begin
    SetLength(Result, 2);
    lType := lContext.GetType(Data.ClassType);
    lField := lType.GetField(Field);
    if Assigned(lField) then
    begin
      lObject := lField.GetValue(Data).AsObject;
      if (Assigned(lObject)) then
      begin
        Result[0] := lObject.QualifiedClassName;
        Result[1] := StringToByteArrayString(TJSONFreedomObject(lObject).ToJSON.ToString);
      end;
    end;
  end;
  {$ENDREGION}
  {$REGION 'GroupCriteria Converter'}
  lGroupCriteriaConverter := function(Data: TObject; Field: string): String
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
    lObject: TObject;
  begin
    Result := '';
    lType := lContext.GetType(Data.ClassType);
    lField := lType.GetField(Field);
    if Assigned(lField) then
    begin
      lObject := lField.GetValue(Data).AsObject;
      if (Assigned(lObject)) then
      begin
        Result := StringToByteArrayString(TGroupCriteria(lObject).ToJSON.ToString);
      end;
    end;
  end;
  {$ENDREGION}
  {$REGION 'Freedom Object List Converter'}
  lFreedomObjectListConverter := function(Data: TObject; Field: string): TListOfStrings
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
    lObject: TObject;
    lObjectList: TFreedomObjectList<TFreedomObject>;
  begin
    SetLength(Result, 2);
    lType := lContext.GetType(Data.ClassType);
    lField := lType.GetField(Field);
    if Assigned(lField) then
    begin
      lObject := lField.GetValue(Data).AsObject;
      if (Assigned(lObject)) then
      begin
        lObjectList := TFreedomObjectList<TFreedomObject>(lObject);
        Result[0] := lObjectList.QualifiedClassName;
//        Result[1] := StringToByteArrayString(lObjectList.ToJSON.ToString);
      end;
    end;
  end;
  {$ENDREGION}

  lRttiType := lRttiContext.GetType(Self);
  for lRttiField in lRttiType.GetFields do
  begin
    if Assigned(lRttiField.FieldType) then
    begin
      if (lRttiField.FieldType.TypeKind = TTypeKind.tkVariant) then
      begin
        pJSONMarshal.RegisterConverter(Self, lRttiField.Name, lVarConverter);
      end
      else if lRttiField.FieldType.IsInstance then
      begin
        if lRttiField.FieldType.AsInstance.MetaclassType.InheritsFrom(TStream) then
        begin
          pJSONMarshal.RegisterConverter(Self, lRttiField.Name, lStreamConverter);
        end
        else if lRttiField.FieldType.AsInstance.MetaclassType.InheritsFrom(TStrings) then
        begin
          pJSONMarshal.RegisterConverter(Self, lRttiField.Name, lStringsConverter);
        end
        else if lRttiField.FieldType.AsInstance.MetaclassType.InheritsFrom(TColumnValueList) then
        begin
          pJSONMarshal.RegisterConverter(Self, lRttiField.Name, lColumnValueListConverter);
        end
        else if Supports(lRttiField.FieldType.AsInstance.MetaclassType, ILazy) then
        begin
          pJSONMarshal.RegisterConverter(Self, lRttiField.Name, lLazyConverter);
        end
        else if lRttiField.FieldType.AsInstance.MetaclassType.InheritsFrom(TGroupCriteria) then
        begin
          pJSONMarshal.RegisterConverter(Self, lRttiField.Name, lGroupCriteriaConverter);
        end
        else if Supports(lRttiField.FieldType.AsInstance.MetaclassType, IFreedomObjectList) then
        begin
          pJSONMarshal.RegisterConverter(Self, lRttiField.Name, lFreedomObjectListConverter);
        end
        else if lRttiField.FieldType.AsInstance.MetaclassType.InheritsFrom(TJSONFreedomObject) then
        begin
          pJSONMarshal.RegisterConverter(Self, lRttiField.Name, lJSONFreedomObjectConverter);
        end;
      end;
    end;
  end;
  GetConverters(pJSONMarshal);
end;

class procedure TJSONFreedomObject.DoGetReverters(pJSONUnMarshal: TJSONUnMarshal);
var
  lRttiType: TRttiType;
  lRttiField: TRTTIField;
  lRTTIContext: TRTTIContext;
  lVarReverter: TStringsReverter;
  lStringsReverter: TStringsReverter;
  lStreamReverter: TStringsReverter;
  lColumnValueListReverter: TStringsReverter;
  lLazyReverter: TStringsReverter;
  lJSONFreedomObjectReverter: TStringsReverter;
  lGroupCriteriaReverter: TStringReverter;
  lFreedomObjectListReverter: TStringsReverter;
begin

  {$REGION 'Variant Reverveter'}
  lVarReverter := procedure(Data: TObject; Field: string; Args: TListOfStrings)
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
  begin
    lType := lContext.GetType(Data.ClassType);
    lField := lType.GetField(Field);
    if Assigned(lField) then
    begin
      if (Args[0] = 'Smallint') or (Args[0] = 'Integer') or  (Args[0] = 'Byte') or
         (Args[0] = 'ShortInt') or (Args[0] = 'Word') or (Args[0] = 'LongWord') or
         (Args[0] = 'Int64') or (Args[0] = 'UInt64') then
      begin
        lField.SetValue(Data, StrToInt(Args[1]));
      end
      else if (Args[0] = 'Single') or (Args[0] = 'Double') or (Args[0] = 'Currency') then
      begin
        lField.SetValue(Data, StrToFloat(Args[1]));
      end
      else if (Args[0] = 'Boolean') then
      begin
        if (AnsiUpperCase(Args[1]) = 'TRUE') or
           (AnsiUpperCase(Args[1]) = 'T') or (AnsiUpperCase(Args[1]) = 'S') or
           (AnsiUpperCase(Args[1]) = '1') then
        begin
          lField.SetValue(Data, 1);
        end
        else
        begin
          lField.SetValue(Data, 0);
        end;
      end
      else if (Args[0] = 'Date') then
      begin
        lField.SetValue(Data, StrToDateDef(Args[1], 0));
      end
      else if (Args[0] = 'Time') then
      begin
        lField.SetValue(Data, StrToTimeDef(Args[1], 0));
      end
      else if (Args[0] = 'DateTime') then
      begin
        lField.SetValue(Data, StrToDateTimeDef(Args[1], 0));
      end
      else if (Args[0] <> 'Empty') and (Args[0] <> 'Unassigned') and (Args[0] <> 'Null') then
      begin
        lField.SetValue(Data, TValue.FromVariant(Args[1]));
      end;
    end;
  end;
  {$ENDREGION}
  {$REGION 'Stream Reverter'}
  lStreamReverter := procedure(Data: TObject; Field: string; Args: TListOfStrings)
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
    lStream: TStream;
    lBytes: TBytes;
  begin
    if (Args[1] <> '') then
    begin
      lType := lContext.GetType(Data.ClassType);
      lField := lType.GetField(Field);
      if Assigned(lField) then
      begin
        lStream := TStream(TJSONFreedomObject.GetClassFromTypeName(Args[0]).NewInstance);
        lBytes := TJSONFreedomObject.ByteArrayFromString(Args[1]);
        lStream.Write(lBytes, Length(lBytes));
        lStream.Position := 0;
        lField.SetValue(Data, TValue.From<TStream>(lStream));
      end;
    end;
  end;
  {$ENDREGION}
  {$REGION 'Strings Reverter'}
  lStringsReverter := procedure(Data: TObject; Field: string; Args: TListOfStrings)
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
    lStream: TStream;
    lBytes: TBytes;
    lStrings: TStrings;
  begin
    if (Args[1] <> '') then
    begin
      lType := lContext.GetType(Data.ClassType);
      lField := lType.GetField(Field);
      if Assigned(lField) then
      begin
        lStrings := TStrings(TJSONFreedomObject.GetClassFromTypeName(Args[0]).NewInstance);
        lStream := TStringStream.Create;
        try
          lBytes := TJSONFreedomObject.ByteArrayFromString(Args[1]);
          lStream.Write(lBytes, Length(lBytes));
          lStream.Position := 0;
          lStrings.Text := Trim(TStringStream(lStream).DataString);
        finally
          lStream.Free;
        end;
        lField.SetValue(Data, TValue.From<TStrings>(lStrings));
      end;
    end;
  end;
  {$ENDREGION}
  {$REGION 'ColumnValueList Reverter'}
  lColumnValueListReverter := procedure(Data: TObject; Field: string; Args: TListOfStrings)
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
    lColumnValueList: TColumnValueList;
    lIndex: Integer;
    lJSONValue: TJSONValue;
    lColumnValueItem: TColumnValueItem;
  begin
    if (Args[1] <> '') then
    begin
      lType := lContext.GetType(Data.ClassType);
      lField := lType.GetField(Field);
      if Assigned(lField) then
      begin
        lColumnValueList := TColumnValueList.Create;
        for lIndex := Low(Args) to High(Args) do
        begin
          lJSONValue := TJSONObject.ParseJSONValue(Args[lIndex]);
          try
            lColumnValueItem := TColumnValueItem.FromJSON<TColumnValueItem>(lJSONValue);
            lColumnValueList.Add(lColumnValueItem)
          finally
            lJSONValue.Free;
          end;
        end;
        lField.SetValue(Data, TValue.From<TColumnValueList>(lColumnValueList));
      end;
    end;
  end;
  {$ENDREGION}
  {$REGION 'Lazy Reverter'}
  lLazyReverter :=  procedure(Data: TObject; Field: string; Args: TListOfStrings)
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
    lObject: TObject;
    lJSONValue: TJSONValue;
  begin
    if (Args[1] <> '') then
    begin
      lType := lContext.GetType(Data.ClassType);
      lField := lType.GetField(Field);
      if Assigned(lField) then
      begin
        lJSONValue := TJSONObject.ParseJSONValue(StringFromByteArrayString(Args[1]));
        try
          lObject := TJSONFreedomObjectClass(GetClassFromTypeName(Args[0])).FromJSON(lJSONValue);
        finally
          lJSONValue.Free;
        end;
        lField.SetValue(Data, TValue.From<TObject>(lObject));
      end;
    end;
  end;
  {$ENDREGION}
  {$REGION 'JSONFreedomObject Reverter'}
  lJSONFreedomObjectReverter := procedure(Data: TObject; Field: string; Args: TListOfStrings)
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
    lObject: TObject;
    lJSONValue: TJSONValue;
  begin
    if (Args[1] <> '') then
    begin
      lType := lContext.GetType(Data.ClassType);
      lField := lType.GetField(Field);
      if Assigned(lField) then
      begin
        lJSONValue := TJSONObject.ParseJSONValue(StringFromByteArrayString(Args[1]));
        try
          lObject := TJSONFreedomObjectClass(GetClassFromTypeName(Args[0])).FromJSON(lJSONValue);
        finally
          lJSONValue.Free;
        end;
        lField.SetValue(Data, TValue.From<TObject>(lObject));
      end;
    end;
  end;
  {$ENDREGION}
  {$REGION 'GroupCriteria Reverter'}
  lGroupCriteriaReverter := procedure(Data: TObject; Field: string; Arg: String)
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
    lJSONValue: TJSONValue;
    lGroupCriteria: TGroupCriteria;
  begin
    if (Arg <> '') then
    begin
      lType := lContext.GetType(Data.ClassType);
      for lField in lType.GetFields do
      begin
        if lField.Name = Field then
        begin
          lJSONValue := TJSONObject.ParseJSONValue(StringFromByteArrayString(Arg));
          lGroupCriteria := nil;
          if (lJSONValue <> nil) then
          begin
            try
              lGroupCriteria := TGroupCriteria.Create;
              lGroupCriteria.FromJSON(TJSONArray(lJSONValue));
            finally
              lJSONValue.Free;
            end;
          end;
          lField.SetValue(Data, TValue.From<TGroupCriteria>(lGroupCriteria));
        end;
        Break;
      end;
    end;
  end;
 {$ENDREGION}
  {$REGION 'Freedom Object List Reverter'}
  lFreedomObjectListReverter := procedure(Data: TObject; Field: string; Args: TListOfStrings)
  var
    lType: TRttiType;
    lField: TRTTIField;
    lContext: TRTTIContext;
    lJSONValue: TJSONValue;
    lObjectList: TFreedomObjectList<TFreedomObject>;
//    lObjectListClass: TFreedomObjectListClass;
  begin
    if (Args[1] <> '') then
    begin
      lType := lContext.GetType(Data.ClassType);
      for lField in lType.GetFields do
      begin
        if lField.Name = Field then
        begin
          lJSONValue := TJSONObject.ParseJSONValue(StringFromByteArrayString(Args[1]));
          try
            lObjectList := nil;
            if (lJSONValue <> nil) then
            begin
//              lObjectListClass := TFreedomObjectListClass(GetClassFromTypeName(Args[0]));
//              lObjectList := lObjectListClass.Create;
              lObjectList := TFreedomObjectList<TFreedomObject>(GetClassFromTypeName(Args[0]).Create);

//              lObjectList.FromJSON(TJSONArray(lJSONValue));
            end;
            lField.SetValue(Data, TValue.From<TObject>(lObjectList));
          finally
            FreeAndNil(lJSONValue);
          end;
          Break;
        end;
      end;
    end;
  end;
  {$ENDREGION}



  lRttiType := lRTTIContext.GetType(Self);
  for lRttiField in lRttiType.GetFields do
  begin
    if Assigned(lRttiField.FieldType) then
    begin
      if lRttiField.FieldType.TypeKind = TTypeKind.tkVariant then
      begin
        pJSONUnMarshal.RegisterReverter(Self, lRttiField.Name, lVarReverter);
      end
      else if lRttiField.FieldType.IsInstance then
      begin
        if lRttiField.FieldType.AsInstance.MetaclassType.InheritsFrom(TStream) then
        begin
          pJSONUnMarshal.RegisterReverter(Self, lRttiField.Name, lStreamReverter);
        end
        else if lRttiField.FieldType.AsInstance.MetaclassType.InheritsFrom(TStrings) then
        begin
          pJSONUnMarshal.RegisterReverter(Self, lRttiField.Name, lStringsReverter);
        end
        else if lRttiField.FieldType.AsInstance.MetaclassType.InheritsFrom(TColumnValueList) then
        begin
          pJSONUnMarshal.RegisterReverter(Self, lRttiField.Name, lColumnValueListReverter);
        end
        else if Supports(lRttiField.FieldType.AsInstance.MetaclassType, ILazy) then
        begin
          pJSONUnMarshal.RegisterReverter(Self, lRttiField.Name, lLazyReverter);
        end
        else if lRttiField.FieldType.AsInstance.MetaclassType.InheritsFrom(TGroupCriteria) then
        begin
          pJSONUnMarshal.RegisterReverter(Self, lRttiField.Name, lGroupCriteriaReverter);
        end
        else if Supports(lRttiField.FieldType.AsInstance.MetaclassType, IFreedomObjectList) then
        begin
          pJSONUnMarshal.RegisterReverter(Self, lRttiField.Name, lFreedomObjectListReverter);
        end
        else if lRttiField.FieldType.AsInstance.MetaclassType.InheritsFrom(TJSONFreedomObject) then
        begin
          pJSONUnMarshal.RegisterReverter(Self, lRttiField.Name, lJSONFreedomObjectReverter);
        end;
      end;
    end;
  end;
  GetReverters(pJSONUnMarshal);
end;

class function TJSONFreedomObject.FromJSON(pJSONValue: TJSONValue): TObject;
var
  lJSONUnMarshal: TJSONUnMarshal;
begin
  Result := nil;
  if not (pJSONValue is TJSONNull) then
  begin
    lJSONUnMarshal := TJSONUnMarshal.Create;
    try
      DoGetReverters(lJSONUnMarshal);
      DoBeforeUnMarshal(lJSONUnMarshal);
      Result := lJSONUnMarshal.Unmarshal(pJSONValue);
      DoAfterUnMarshal(Result);
    finally
      lJSONUnMarshal.Free;
    end;
  end;
end;

class function TJSONFreedomObject.FromJSON<T>(pJSONValue: TJSONValue): T;
begin
  Result := T(FromJSON(pJSONValue));
end;

class function TJSONFreedomObject.GetClassFromTypeName(pTypeName: String): TClass;
var
  lContext: TRttiContext;
  lType: TRttiType;
begin
  Result := nil;
  lType := lContext.FindType(pTypeName);
  if (Assigned(lType)) then
  begin
    Result := lType.AsInstance.MetaclassType;
  end;
end;

class procedure TJSONFreedomObject.GetConverters(pJSONMarshal: TJSONMarshal);
begin
  // override when necessary
end;

class procedure TJSONFreedomObject.GetReverters(pJSONUnMarshal: TJSONUnMarshal);
begin
  // override when necessary
end;

class function TJSONFreedomObject.ToByteArray(pStream: TStream): TBytes;
begin
  pStream.Position := 0;
  SetLength(Result, pStream.Size);
  pStream.Read(Result, pStream.Size);
end;

function TJSONFreedomObject.ToJSON: TJSONValue;
begin
  Result := Self.ToJSON(Self);
end;

class function TJSONFreedomObject.ToJSON(pObject: TJSONFreedomObject): TJSONValue;
var
  lJSONMarshal: TJSONMarshal;
begin
  if Assigned(pObject) then
  begin
    lJSONMarshal := TJSONMarshal.Create(TJSONConverter.Create);
    try
      DoGetConverters(lJSONMarshal);
      DoBeforeMarshal(lJSONMarshal);
      Result := lJSONMarshal.Marshal(pObject);
      DoAfterMarshal(lJSONMarshal);
    finally
      FreeAndNil(lJSONMarshal);
    end;
  end
  else
    Result := TJSONNull.Create;
end;

class function TJSONFreedomObject.StringToByteArrayString(pString: String): String;
var
  lStream: TStringStream;
  lStrings: TStrings;
  lBytes: TBytes;
begin
  lStream := TStringStream.Create;
  lStrings := TStringList.Create;
  try
    lStrings.Text := pString;
    lStrings.SaveToStream(lStream);
    lBytes := TJSONFreedomObject.ToByteArray(lStream);
    Result := TJSONFreedomObject.ByteArrayToString(lBytes);
  finally
    lStrings.Free;
    lStream.Free;
  end;
end;

class function TJSONFreedomObject.StringFromByteArrayString(pString: String): String;
var
  lStream: TStringStream;
  lBytes: TBytes;
begin
  lStream := TStringStream.Create;
  try
    lBytes := TJSONFreedomObject.ByteArrayFromString(pString);
    lStream.Write(lBytes, Length(lBytes));
    lStream.Position := 0;
    Result := Trim(TStringStream(lStream).DataString);
  finally
    lStream.Free;
  end;
end;

end.

