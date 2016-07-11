unit AM.Freedom.FreedomObjectListToDataSet;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  Data.DB,
  AM.Freedom.ObjectMapper,
  AM.Freedom.FreedomObject,
  AM.Freedom.FreedomObjectList,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.EnumerationTypes;

type
  TOnDataSetOpen = procedure(pSender: TDataSet) of object;
  TFreedomObjectListToDataSet<T: class> = class sealed
  strict private
    FDataSet: TDataSet;
    FMapper: TObjectMapper;
    FObjectList: TList<T>;
    FOnDataSetOpen: TOnDataSetOpen;
    function AddField(pColumn: TCustomColumnMapper): TField;
    function ExtractFieldClassFromColumnType(pColumnType: TColumnType): TFieldClass;
    procedure Initialize;
    procedure AddAllFields;
    procedure PopulateDataSet;
    procedure VerifyFieldOptions;
    procedure SetDataSet(const Value: TDataSet);
    procedure DoOpen;
  strict protected
    function ExtractDataSetList(pObjectList: TList<T>): TDataSet;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property OnDataSetOpen: TOnDataSetOpen read FOnDataSetOpen write FOnDataSetOpen;
  public
    destructor Destroy; override;
    class procedure ObjectListToDataSet(pObjectList: TList<T>; pDataSet: TDataSet; pOnDataSetOpen: TOnDataSetOpen);
  end;


implementation

uses
  AM.Freedom.ObjectMapper.ObjectToMapper;

{ TFreedomObjectListToDataSet<T> }

procedure TFreedomObjectListToDataSet<T>.AddAllFields;
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in FMapper.Columns do
  begin
    AddField(lColumn);
  end;
end;

function TFreedomObjectListToDataSet<T>.AddField(pColumn: TCustomColumnMapper): TField;
var
  lFieldClass: TFieldClass;
begin
  lFieldClass := ExtractFieldClassFromColumnType(pColumn.ColumnType);
  if (lFieldClass <> nil) then
  begin
    Result := lFieldClass.Create(FDataSet);
    Result.FieldName := pColumn.Name;
    if (pColumn.Size > 0) then
    begin
      Result.Size := pColumn.Size;
    end
    else if (pColumn.ColumnType = ctyString) then
    begin
      Result.Size := 1000;
    end;
    if (pColumn.Scale > 0) then
    begin
      if (Result.InheritsFrom(TFloatField)) then
      begin
        TFloatField(Result).Precision := pColumn.Scale;
      end
      else if (Result.InheritsFrom(TExtendedField)) then
      begin
        TExtendedField(Result).Precision := pColumn.Scale;
      end
    end;
    if (pColumn.InheritsFrom(TNumericField)) then
    begin
      TNumericField(Result).DisplayFormat := pColumn.BindOptions.DisplayFormat;
      TNumericField(Result).DisplayLabel := pColumn.BindOptions.DisplayLabel;
    end;
    Result.DataSet := FDataSet;
  end;
end;

destructor TFreedomObjectListToDataSet<T>.Destroy;
begin
  FDataSet := nil;
  FObjectList := nil;
  TObjectToMapper.UnlockMapper(FMapper.GetHashCode);
  inherited;
end;

procedure TFreedomObjectListToDataSet<T>.DoOpen;
begin
  if (Assigned(FOnDataSetOpen)) then
  begin
    FOnDataSetOpen(FDataSet);
  end
  else
  begin
    FDataSet.Open;
  end;
end;

function TFreedomObjectListToDataSet<T>.ExtractDataSetList(pObjectList: TList<T>): TDataSet;
begin
  Initialize;
  AddAllFields;
  FObjectList := pObjectList;
  PopulateDataSet;
  VerifyFieldOptions;
  Result := FDataSet;
end;

function TFreedomObjectListToDataSet<T>.ExtractFieldClassFromColumnType(pColumnType: TColumnType): TFieldClass;
begin
  case pColumnType of
    ctyByte: Result := TByteField;
    ctySmallint: Result := TSmallintField;
    ctyInteger: Result := TIntegerField;
    ctyInt64: Result := TLargeintField;
    ctyChar: Result := TStringField;
    ctyString: Result := TStringField;
    ctySingle: Result := TSingleField;
    ctyDouble: Result := TFloatField;
    ctyCurrency: Result := TCurrencyField;
    ctyExtended: Result := TExtendedField;
    ctyDate: Result := TDateField;
    ctyTime: Result := TTimeField;
    ctyDateTime: Result := TDateTimeField;
    ctyBoolean: Result := TBooleanField;
    ctyEnumerator: Result := TByteField;
    ctyDetail: Result := TDataSetField;
    ctyBlob: Result := TBlobField;
    ctyMemo: Result := TMemoField;
    ctyGuid: Result := TGuidField;
    else
      Result := nil;
  end;
end;

procedure TFreedomObjectListToDataSet<T>.Initialize;
var
  lParams: TObjectToMapperParams;
begin
  lParams := TObjectToMapperParams.Create;
  lParams.MetaClassType := T.ClassInfo;
  lParams.Options := [Properties];
  FMapper := TObjectToMapper.ObjectToMapper(lParams);
end;

class procedure TFreedomObjectListToDataSet<T>.ObjectListToDataSet(pObjectList: TList<T>; pDataSet: TDataSet; pOnDataSetOpen: TOnDataSetOpen);
var
  lFreedomObjectListToDataSet: TFreedomObjectListToDataSet<T>;
begin
  if (not Assigned(pDataSet)) then
  begin
    raise EArgumentNilException.CreateFmt('The argument %s must be specified in method ObjectListToDataSet at class %s',
        [pDataSet, Self.ClassName]);
  end;
  lFreedomObjectListToDataSet := TFreedomObjectListToDataSet<T>.Create;
  try
    lFreedomObjectListToDataSet.OnDataSetOpen := pOnDataSetOpen;
    lFreedomObjectListToDataSet.DataSet := pDataSet;
    lFreedomObjectListToDataSet.ExtractDataSetList(pObjectList);
  finally
    lFreedomObjectListToDataSet.Free;
  end;
end;

procedure TFreedomObjectListToDataSet<T>.PopulateDataSet;
var
  lObject: T;
  lParams: TObjectToMapperParams;
  lMapper: TObjectMapper;
  lColumn: TCustomColumnMapper;
begin
  DoOpen;
  FDataSet.DisableControls;
  try
    for lObject in FObjectList do
    begin
      lParams := TObjectToMapperParams.Create;
      lParams.ObjectInstance := lObject;
      lParams.Options := [Properties];
      lMapper := TObjectToMapper.ObjectToMapper(lParams);
      try
        FDataSet.Append;
        for lColumn in lMapper.Columns do
        begin
          if FDataSet.FindField(lColumn.Name) <> nil then
          begin
            FDataSet.FieldByName(lColumn.Name).Value := lColumn.ColumnValue;
          end;
        end;
        FDataSet.Post;
      finally
        TObjectToMapper.UnlockMapper(lMapper.GetHashCode);
      end;
    end;
  finally
    FDataSet.First;
    FDataSet.EnableControls;
  end;
end;

procedure TFreedomObjectListToDataSet<T>.SetDataSet(const Value: TDataSet);
begin
  FDataSet := Value;
  FDataSet.Close;
end;

procedure TFreedomObjectListToDataSet<T>.VerifyFieldOptions;
var
  lColumn: TCustomColumnMapper;
  lField: TField;
begin
  for lColumn in FMapper.Columns do
  begin
    lField := FDataSet.FindField(lColumn.Name);
    if (lField <> nil) then
    begin
      lField.ReadOnly := lColumn.IdOptions.IsId;
      lField.Required := (Required in lColumn.ColumnOptions);
    end;
  end;
end;

end.
