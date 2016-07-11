unit AM.Freedom.JSONConverter;

interface

uses
  System.Rtti,
  System.SysUtils,
  AM.Freedom.JSONObjects;

type
  TFieldJSONConverter = class
  private
    FObject: TObject;
    FField: TRttiField;
    function ConvertField: TJSONPair;
    function ConvertFromFieldValue(pValue: TValue): TJSONValue;
  public
    function Convert(pObject: TObject; pField: TRttiField): TJSONPair;
  end;

  TClassJSONConverterClass = class of TClassJSONConverter;

  TClassJSONConverter = class
  private
    FContext: TRttiContext;
    FType: TRttiType;
    FObject: TObject;
    FJSONObject: TJSONObject;
    procedure InitializeRtti;
    procedure AssignSignatureHeader;
    procedure AssignSignatureFields;
    function FieldCanBeConverted(pField: TRttiField): Boolean;
  public
    function Convert(pObject: TObject): TJSONValue; virtual;
  end;

  TJSONConverter = class
  public
    function Convert(pObject: TObject): String;
    class function MakeJSON(pObject: TObject): String;
  end;

implementation

uses
  System.TypInfo,
  System.Variants,
  System.StrUtils,
  System.Generics.Collections,
  AM.Freedom.JSONAttributes,
  AM.Freedom.JSONConsts,
  AM.Freedom.JSONUtils,
  AM.Freedom.JSONValueFactory,
  AM.Freedom.JSONClassConverterRegister;

{ TFieldJSONConverter }

function TFieldJSONConverter.Convert(pObject: TObject; pField: TRttiField): TJSONPair;
begin
  FObject := pObject;
  FField := pField;
  Result := ConvertField;
end;

function TFieldJSONConverter.ConvertFromFieldValue(pValue: TValue): TJSONValue;
var
  lClassConverter: TClassJSONConverter;
  lTypeData: PTypeData;
  lTypeName: String;
  lObject: TObject;
  lDate: TDate;
  lTime: TTime;
  lDateTime: TDateTime;
  lExtended: Extended;
  lIndex: Integer;
  lLength: Integer;
  lValue: TValue;
begin
  Result := nil;
  case pValue.Kind of
    tkString, tkLString, tkWString, tkUString:
        Result := TJSONString.Create(pValue.AsString);
    tkEnumeration:
      begin
        lTypeName := GetTypeName(pValue.TypeInfo);
        if SameText(lTypeName, 'Boolean') then
        begin
          Result := TJSONBoolean.Create(pValue.AsBoolean);
        end
        else
        begin
          Result := TJSONEnumerator.Create(pValue.TypeInfo, pValue.AsOrdinal);
        end;
      end;
    tkInteger: Result := TJSONInteger.Create(pValue.AsInteger);

    tkInt64: Result := TJSONInteger.Create(pValue.AsInt64);

    tkFloat:
      begin
        lTypeData := GetTypeData(pValue.TypeInfo);
        lTypeName := GetTypeName(pValue.TypeInfo);
        case lTypeData.FloatType of
          ftSingle, ftExtended: begin
            Result := TJSONFloat.Create(pValue.AsExtended);
          end;
          ftCurr: begin
            Result := TJSONFloat.Create(pValue.AsCurrency);
          end;
          ftDouble:
            begin
              lExtended := pValue.AsExtended;
              if SameText(lTypeName, 'TDate') then
              begin
                lDate := lExtended;
                Result := TJSONDate.Create(lDate);
              end
              else if SameText(lTypeName, 'TTime') then
              begin
                lTime := lExtended;
                Result := TJSONTime.Create(lTime);
              end
              else if SameText(lTypeName, 'TDateTime') then
              begin
                lDateTime := lExtended;
                Result := TJSONDateTime.Create(lDateTime);
              end
              else
              begin
                Result := TJSONFloat.Create(lExtended);
              end;
            end;
        end;
      end;
    tkClass: begin
      lObject := pValue.AsObject;
      if (Assigned(lObject)) then
      begin
        lClassConverter := TJSONClassConverterRegister.GetConverterForClassOrClassName(lObject.ClassType, lObject.ClassName).Create;
        try
          Result := lClassConverter.Convert(lObject);
        finally
          lClassConverter.Free;
        end;
      end;
    end;
    tkVariant: Result := TJSONValueFactory.CreateFromVariant(pValue.AsVariant);

    tkArray, tkDynArray: begin
      Result := TJSONArray.Create;
      lLength := pValue.GetArrayLength - 1;
      for lIndex := 0 to lLength do begin
        lValue := pValue.GetArrayElement(lIndex);
        TJSONArray(Result).AddElement(ConvertFromFieldValue(lValue));
      end;
      if TJSONArray(Result).Elements.Count = 0 then
      begin
        FreeAndNil(Result);
      end;
    end;
  end;
  if (Result = nil) then
  begin
    Result := TJSONNull.Create;
  end;
end;

function TFieldJSONConverter.ConvertField: TJSONPair;
var
  lFieldValue: TJSONValue;
  lAttribute: TCustomAttribute;
  lReflector: TCustomJSONReflector;
  lObject: TObject;
  lContext: TRttiContext;
  lType: TRttiType;
  lField: TRttiField;
  lValue: TValue;
begin
  lFieldValue := nil;
  for lAttribute in FField.GetAttributes do
  begin
    if (lAttribute.InheritsFrom(JSONCustomValue)) then
    begin
      lFieldValue := JSONCustomValue(lAttribute).Value;
      Break;
    end
    else if (lAttribute.InheritsFrom(JSONNull)) then
    begin
      lFieldValue := TJSONNull.Create;
      Break;
    end
    else if (lAttribute.InheritsFrom(JSONReflector)) then
    begin
      if (rptConvert in JSONReflector(lAttribute).ReflectorParseTypes) then
      begin
        lReflector := JSONReflector(lAttribute).ReflectorClass.Create;
        try
          lFieldValue := lReflector.Convert(FObject, FField);
        finally
          lReflector.Free;
        end;
        Break;
      end;
    end
    else if (lAttribute.InheritsFrom(JSONListOnlyItens)) and (FField.FieldType.IsInstance) then
    begin
      if (TJSONUtils.ClassIsList(FField.FieldType.AsInstance.MetaClassType)) then
      begin
        lObject := FField.GetValue(FObject).AsObject;
        if (Assigned(lObject)) then
        begin
          lType := lContext.GetType(lObject.ClassInfo);
          lField := lType.GetField('FItems');
          lValue := lField.GetValue(lObject);
          lFieldValue := ConvertFromFieldValue(lValue);
        end
        else
        begin
          lFieldValue := TJSONNull.Create;
        end;
      end;
    end;
  end;
  if (lFieldValue = nil) then
  begin
    lFieldValue := ConvertFromFieldValue(FField.GetValue(FObject));
  end;
  if (lFieldValue = nil) then
  begin
    lFieldValue := TJSONNull.Create;
  end;
  Result := TJSONPair.Create(FField.Name, lFieldValue);
end;

{ TClassJSONConverter }

function TClassJSONConverter.Convert(pObject: TObject): TJSONValue;
begin
  Result := nil;
  if (Assigned(pObject)) then
  begin
    FObject := pObject;
    InitializeRtti;
    AssignSignatureHeader;
    AssignSignatureFields;
    Result := FJSONObject;
  end;
  if (not Assigned(Result)) then
  begin
    Result := TJSONNull.Create;
  end;
end;

procedure TClassJSONConverter.InitializeRtti;
begin
  FContext := TRttiContext.Create;
  FType := FContext.GetType(FObject.ClassInfo);
  FJSONObject := TJSONObject.Create;
end;

procedure TClassJSONConverter.AssignSignatureHeader;
begin
  FJSONObject.AddElement(TJSONPair.Create('type', TJSONString.Create(FObject.QualifiedClassName)));
end;

procedure TClassJSONConverter.AssignSignatureFields;
var
  lType: TRttiType;
  lField: TRttiField;
  lListOfFields: TList<TRttiField>;
  lFieldConverter: TFieldJSONConverter;
  lAttribute: TCustomAttribute;
  lIsMinBaseClassName: Boolean;
  lFieldPair: TJSONPair;
  lJSONObject: TJSONObject;
begin
  lType := FType;
  lListOfFields := TList<TRttiField>.Create;
  lJSONObject := TJSONObject.Create;
  try
    while Assigned(lType) do
    begin
      for lField in lType.GetFields do
      begin
        if (FieldCanBeConverted(lField)) then
        begin
          lListOfFields.Add(lField);
        end;
      end;
      lIsMinBaseClassName := False;
      for lAttribute in lType.GetAttributes do
      begin
        if (lAttribute.InheritsFrom(JSONMinBaseClassName)) then
        begin
          if SameText(lType.AsInstance.MetaclassType.ClassName,
             JSONMinBaseClassName(lAttribute).MinClassName) then
          begin
            lIsMinBaseClassName := True;
            Break;
          end;
        end;
      end;
      if (lIsMinBaseClassName) then
      begin
        Break;
      end;
      lType := lType.BaseType;
    end;
    if (lListOfFields.Count > 0) then
    begin
      lFieldConverter := TFieldJSONConverter.Create;
      try
        for lField in lListOfFields do
        begin
          lFieldPair := lFieldConverter.Convert(FObject, lField);
          if (lFieldPair <> nil) then
          begin
            lJSONObject.AddElement(lFieldPair);
          end;
        end;
      finally
        lFieldConverter.Free;
      end;
    end;
  finally
    lListOfFields.Free;
  end;
  if (lJSONObject.Elements.Count > 0) then
  begin
    FJSONObject.AddElement(TJSONPair.Create('fields', lJSONObject));
  end
  else begin
    lJSONObject.Free;
  end;
end;

function TClassJSONConverter.FieldCanBeConverted(pField: TRttiField): Boolean;
var
  lAttribute: TCustomAttribute;
begin
  Result := True;
  for lAttribute in pField.GetAttributes do
  begin
     if (lAttribute.InheritsFrom(JSONUnparsed)) then
     begin
       Result := False;
       Break;
     end;
  end;
end;

{ TJSONConverter }

function TJSONConverter.Convert(pObject: TObject): String;
var
  lClassConverter: TClassJSONConverter;
  lJSONValue: TJSONValue;
begin
  if (Assigned(pObject)) then
  begin
    lClassConverter := TClassJSONConverter.Create;
    try
      lJSONValue := lClassConverter.Convert(pObject);
    finally
      lClassConverter.Free;
    end;
  end;
  if (not Assigned(lJSONValue)) then
  begin
    lJSONValue := TJSONNull.Create;
  end;
  Result := lJSONValue.Value;
  FreeAndNil(lJSONValue);
end;

class function TJSONConverter.MakeJSON(pObject: TObject): String;
var
  lJSONConverter: TJSONConverter;
begin
  Result := '';
  if (Assigned(pObject)) then
  begin
    lJSONConverter := TJSONConverter.Create;
    try
      Result := lJSONConverter.Convert(pObject);
    finally
      lJSONConverter.Free;
    end;
  end;
end;

end.