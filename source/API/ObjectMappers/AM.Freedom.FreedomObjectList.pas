unit AM.Freedom.FreedomObjectList;

interface

uses
  System.Classes,
  System.Generics.Defaults,
  System.Generics.Collections,
  AM.Freedom.CustomFreedomObjectList,
  AM.Freedom.IFreedomObjectList,
  AM.Freedom.IPersistent,
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.ObjectMapper,
  AM.Freedom.GroupCriteria,
  AM.Freedom.DefaultsClassRegister,
  AM.Freedom.FreedomObject,
  AM.Freedom.SQLMappers.CustomSelect,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ICursor,
  AM.Freedom.Exceptions,
  AM.Freedom.FindPropertyItem,
  AM.Freedom.GroupFilterCriteria,
  AM.Freedom.FilteredItem,
  AM.Freedom.GroupFilterCriteria.FilterCriteria,
  AM.Freedom.ObjectMapper.ColumnValueItem,
  AM.Freedom.IFreedomObject, 
  System.SyncObjs,
  AM.Freedom.JSONAttributes;


type
  TFreedomObjectList<T: TFreedomObject, constructor> = class(TCustomFreedomObjectList<T>, IFreedomObjectList)
  strict private type
    TObjectComparer = class(TComparer<T>)
    private
      FPropertyName: string;
      FSortType: TArray<TSortType>;
      FStrSortOptions: TStrSortOptions;
      FListOfPropertyNames: TStrings;
      procedure SetPropertyName(const Value: string);
      procedure GenerateListOfPropertyNames;
    public
      destructor Destroy; override;
      function Compare(const pLeftObject, pRightObject: T): Integer; override;
      property PropertyName: string read FPropertyName write SetPropertyName;
      property SortType: TArray<TSortType> read FSortType write FSortType;
      property StrSortOptions: TStrSortOptions read FStrSortOptions write FStrSortOptions;
    end;
  strict private type
    TAssignCursorThread = class(TThread)
    private
      FCursor: ICursor;
      FObjectList: TList<T>;
      FCurrentSchema: string;
      FObjectClass: TFreedomObjectClass;
      FFinished: Boolean;
      FObjectMapper: TObjectMapper;
      FIFreedomObject: IFreedomObject;
      procedure SetObjectState(pObject: T; pState: TObjectState);
      procedure SetOldObjectState(pObject: T; pState: TObjectState);
      procedure ForceTerminate;
      procedure PersistentRecordToObject(pColumnValueList: TColumnValueList);
      function DoExtractInterface(pObject: TObject): IFreedomObject;
    protected
      procedure Execute; override;
    public
      constructor Create; reintroduce;
      destructor Destroy; override;
      property Cursor: ICursor read FCursor write FCursor;
      property ObjectList: TList<T> read FObjectList;
      property ObjectClass: TFreedomObjectClass read FObjectClass write FObjectClass;
      property CurrentSchema: string read FCurrentSchema write FCurrentSchema;
      property ObjectMapper: TObjectMapper read FObjectMapper write FObjectMapper;
      property Finished: Boolean read FFinished;
    end;
  strict private type
    TAssignCursorThreadList = class(TObjectList<TAssignCursorThread>)
    strict private
      FLastIndex: Integer;
      function ExtractObjectMapperFrom(pObjectClass: TFreedomObjectClass): TObjectMapper;
      procedure OnThreadTerminate(Sender: TObject);
      procedure AddThread(pThread: TAssignCursorThread);
      procedure StartNext;
      procedure StartAll;
    public
      procedure AssignCursor2(pCursor: TCursorList; pObjectClass: TFreedomObjectClass; pCurrentSchema: String;
          pObjectList: TFreedomObjectList<T>);
      procedure AssignCursor(pCursor: TCursorList; pObjectClass: TFreedomObjectClass; pCurrentSchema: String;
          pObjectList: TFreedomObjectList<T>);
    end;
  private
    [JSONUnparsed]
    FPersistent: IPersistent;
    [JSONUnparsed]
    FReadOnly: Boolean;
    [JSONUnparsed]
    FObjectClass: TFreedomObjectClass;
    [JSONUnparsed]
    FFilter: TGroupFilterCriteria;
    [JSONUnparsed]
    FFilterActive: Boolean;
    [JSONUnparsed]
    FFilteredList: TFilteredList<T>;
    [JSONUnparsed]
    FFindPropertyList: TFindPropertyList;
    [JSONUnparsed]
    FLastFindResultIndex: Integer;
    [JSONUnparsed]
    FLastFindObjectClass: TClass;
    [JSONUnparsed]
    FLastFindStrOptions: TFindStrOptions;
    [JSONUnparsed]
    FCriticalSection: TCriticalSection;
    [JSONUnparsed]
    FFixedMapper: Boolean;
    function ObjectClass: TFreedomObjectClass;
    function GetPersistent: IPersistent;
    procedure Assign(pCursor: TCursorList; pMapper: TObjectMapper);
    function FixMapper(pMapper: TObjectMapper): TObjectMapper;
    function AssignCursorWithThread: Boolean;
    procedure DoAssignCursor(pCursor: ICursor; pMapper: TObjectMapper);
    procedure DoAssignCursorWithThread(pCursor: TCursorList; pCurrentSchema: String);
    procedure PersistCleanAndInsertedObjects;
    procedure RemoveUnknowObjects;
    procedure PersistDeletedObjects;
    procedure UnpersistTrashList;
    procedure UnpersistInternalList;
    procedure SetRefColumnValue(pRefColumnName: string; pColumnValue: Variant);
    function IsCompatibleTypes(pColumnType, pColumnValueType: TColumnType): Boolean;
    procedure SetPersistState(pObjectState: TObjectState);
    function GenerateFindPropertyList(pPropertyNames: Array of String; pPropertyValue: Array of Variant): TFindPropertyList;
    function FindObjectByPropertyValue(pObject: TObject; pPropertys: TStrings; pPropertyValue: Variant; pFindStrOptions: TFindStrOptions = []): Boolean;
    function DoFindObjectByPropertyValue(pObject: TObject; pPropertys: TStrings; pPropertyValue: Variant; pFindStrOptions: TFindStrOptions = []): Boolean;
    function DoIfFreedomObjectList(pFreedomObjectList: IFreedomObjectList; pPropertys: TStrings; pPropertyValue: Variant; pFindStrOptions: TFindStrOptions = []): Boolean;
    function DoIfSimpleObject(pObject: TObject; pPropertyName: string; pPropertys: TStrings; pPropertyValue: Variant; pFindStrOptions: TFindStrOptions = []): Boolean;
    function FindObjectByProperty(pPropertyNames: string; pPropertyValue: Variant; pFindStrOptions: TFindStrOptions = []): TObject; overload;
    function ExtractObjectMapperFrom(pObject: TObject; pMetaClassType: TClass; pOptions: TObjectToMapperOptions = [SubLevels]): TObjectMapper;
    procedure FinalizeObjectMapperFrom(pMapperHash: Integer);
    procedure DoSearchWithClass(pGroupCriteria: TGroupCriteria; pClass: TFreedomObjectClass);
    procedure SetFilter(const pFilter: TGroupFilterCriteria);
    procedure UnMakeFilter;
    procedure MakeFilter;
    procedure AssignObjectToMapper(pObject: T; pMapper: TObjectMapper);
    procedure AssignObjectToFilter(pObject: T; pListOfCriterias: TList<TFilterCriteria>; pMapper: TObjectMapper);
    function GetObjectPropertyValue(pObject: T; pPropertyName: String; pMapper: TObjectMapper): Variant;
    function DoFindObjectByPropertyNames: T;
  strict protected
    function DoCreateObject: T; override;
    procedure DoFixGroupCriteriaForLoad(pGroupCriteria: TGroupCriteria); virtual;
    procedure DoSearch(pGroupCriteria: TGroupCriteria); override;
    procedure DoSearchWithSelect(pSelect: TCustomSelect);
    procedure DoBeforeClear; override;
    procedure MakeCreate; override;
  public
    constructor Create(pPersistent: IPersistent); reintroduce; overload;
    constructor Create(pGroupCriteria: TGroupCriteria; pPersistent: IPersistent = nil); reintroduce; overload;
    destructor Destroy; override;
    procedure GetObjects(pGroupCriteria: TGroupCriteria); overload;
    procedure GetAllObjects; overload;
    procedure GetObjects(pGroupCriteria: TGroupCriteria; pClass: TFreedomObjectClass); overload;
    procedure GetAllObjects(pClass: TFreedomObjectClass); overload;
    procedure Sort(pSortByPropertyName: string; pSortsType: Array of TSortType; pStrSortOptions: TStrSortOptions = []); reintroduce; overload;
    procedure Sort(pSortByPropertyName: string; pStrSortOptions: TStrSortOptions = []); reintroduce; overload;
    function FindObjectByPropertyNames(pPropertyNames: Array of string; pPropertyValue: Array of Variant; pFindStrOptions: TFindStrOptions = []): T; overload;
    function FindObjectByPropertyNames(pObjectClass: TClass; pPropertyNames: Array of string; pPropertyValue: Array of Variant; pFindStrOptions: TFindStrOptions = []): T; overload;
    function FindObjectByPropertyNames(pPropertyNames: string; pPropertyValue: Variant; pFindStrOptions: TFindStrOptions = []): T; overload;
    function FindObjectByPropertyNames(pObjectClass: TClass; pPropertyNames: string; pPropertyValue: Variant; pFindStrOptions: TFindStrOptions = []): T; overload;
    function FindNext: T;
    procedure ResetFind;
    function FindListOfObjectByPropertyNames(pPropertyNames: Array of string; pPropertyValue: Array of Variant; pFindStrOptions: TFindStrOptions = []): TList<T>; overload;
    function FindListOfObjectByPropertyNames(pObjectClass: TClass; pPropertyNames: Array of string; pPropertyValue: Array of Variant; pFindStrOptions: TFindStrOptions = []):  TList<T>; overload;
    function FindObjectByID(pID: Variant): T; overload;
    function FindObjectByID(pObjectClass: TClass; pID: Variant): T; overload;
    procedure PersistObjects; override;
    procedure UnpersistObjects; override;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property Filter: TGroupFilterCriteria read FFilter write SetFilter;
    procedure ApplyFilter;
    procedure CancelFilter;
    function IsEmpty: Boolean;
  end;

  TFreedomObjectListClass = class of TDefaultFreedomObjectList;

  TDefaultFreedomObjectList = class(TFreedomObjectList<TFreedomObject>);

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.Math,
  System.Variants,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.ObjectMapper.ObjectWriter,
  AM.Freedom.ObjectMapper.MapperToObject,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  System.Rtti,
  AM.Freedom.Helper.RttiField,
  AM.Freedom.Helper.Variant,
  AM.Freedom.Helper.RttiType,
  AM.Freedom.ObjectMapper.ObjectToMapperAssigner,
  AM.Freedom.GroupFilterCriteria.IFilterCriteria,
  AM.Freedom.ObjectPropertyValueFinder;

constructor TFreedomObjectList<T>.Create(pPersistent: IPersistent);
begin
  inherited Create;
  FPersistent := pPersistent;
end;

procedure TFreedomObjectList<T>.AssignObjectToFilter(pObject: T; pListOfCriterias: TList<TFilterCriteria>; pMapper: TObjectMapper);
var
  lPropertyValue: Variant;
  lIFilterCriteria: IFilterCriteria;
  lCriteria: TFilterCriteria;
begin
  for lCriteria in pListOfCriterias do
  begin
    Supports(lCriteria, IFilterCriteria, lIFilterCriteria);
    lPropertyValue := GetObjectPropertyValue(pObject, lCriteria.PropertyName, pMapper);
    lIFilterCriteria.SetPropertyValue(lPropertyValue);
  end;
end;

procedure TFreedomObjectList<T>.AssignObjectToMapper(pObject: T; pMapper: TObjectMapper);
var
  lAssigner: TObjectToMapperAssigner;
begin
  lAssigner := TObjectToMapperAssigner.Create;
  try
    lAssigner.AssignObjectToMapper(pMapper, pObject);
  finally
    lAssigner.Free;
  end;
end;

procedure TFreedomObjectList<T>.CancelFilter;
begin
  if FFilterActive then
  begin
    UnMakeFilter;
  end;
  FFilterActive := False;
end;

constructor TFreedomObjectList<T>.Create(pGroupCriteria: TGroupCriteria; pPersistent: IPersistent);
begin
  Create(pPersistent);
  if Assigned(pGroupCriteria) then
  begin
    try
      DoSearch(pGroupCriteria);
    finally
      pGroupCriteria.Free;
    end;
  end;
end;

destructor TFreedomObjectList<T>.Destroy;
begin
  UnMakeFilter;
  FPersistent := nil;
  FreeAndNil(FFilteredList);
  FreeAndNil(FFilter);
  FreeAndNil(FFindPropertyList);
  FreeAndNil(FCriticalSection);
  inherited;
end;

procedure TFreedomObjectList<T>.DoAssignCursor(pCursor: ICursor; pMapper: TObjectMapper);
var
  lCursor: ICursor;
  lObject: T;
  lFreedomObject: IFreedomObject;
  lColumnValueList: TColumnValueList;
begin
  pCursor.First;
  while not pCursor.Eof do
  begin
    lObject := T(ObjectClass.Create);
    try
      TObjectToMapper.AssignObjectToMapper(pMapper, lObject);
      Supports(lObject, IFreedomObject, lFreedomObject);
      lColumnValueList := lFreedomObject.GetColumnValueList;
      TObjectWriter.PersistentRecordToObject(pCursor, pMapper, pMapper.CurrentSchema, lColumnValueList);
      SetObjectState(lObject, Clean);
      SetOldObjectState(lObject, Clean);
    except
      lObject.Free;
      raise;
    end;
    inherited Add(lObject);
    pCursor.Next;
  end;
end;

procedure TFreedomObjectList<T>.DoAssignCursorWithThread(pCursor: TCursorList; pCurrentSchema: String);
var
  lAssignCursorThreadList: TAssignCursorThreadList;
begin
  lAssignCursorThreadList := TAssignCursorThreadList.Create;
  lAssignCursorThreadList.AssignCursor(pCursor, ObjectClass, pCurrentSchema, Self);
end;

procedure TFreedomObjectList<T>.DoBeforeClear;
begin
  UnMakeFilter;
end;

function TFreedomObjectList<T>.DoCreateObject: T;
begin
  if (Assigned(FObjectClass)) then
  begin
    Result := T(FObjectClass.Create);
  end
  else
  begin
    Result := T.Create;
  end;
end;

function TFreedomObjectList<T>.DoFindObjectByPropertyNames: T;
var
  lPropertys: TStrings;
  lObject: T;
  lFindItem: TFindPropertyItem;
  lIndex: Integer;
begin
  Result := nil;
  for lIndex := FLastFindResultIndex to Count - 1 do
  begin
    lObject := Items[lIndex];
    if TFreedomObject(lObject).ClassName = FLastFindObjectClass.ClassName then
    begin
      Result := lObject;
      for lFindItem in FFindPropertyList do
      begin
        lPropertys := TStringList.Create;
        try
          lPropertys.AddStrings(lFindItem.PropertyNames);
          if not FindObjectByPropertyValue(lObject, lPropertys, lFindItem.PropertyValue, FLastFindStrOptions) then
          begin
            Result := nil;
            Break;
          end;
        finally
          lPropertys.Free;
        end;
      end;
      if (Assigned(Result)) then
      begin
        FLastFindResultIndex := lIndex + 1;
        Break;
      end;
    end;
  end;
end;

function TFreedomObjectList<T>.DoFindObjectByPropertyValue(pObject: TObject; pPropertys: TStrings; pPropertyValue: Variant; pFindStrOptions: TFindStrOptions): Boolean;
var
  lPropertyName: string;
  lColumn: TCustomColumnMapper;
  lObject: TObject;
  lPropertys: TStrings;
  lFreedomObjectList: IFreedomObjectList;
begin
  Result := False;
  lPropertyName := pPropertys.Strings[0];
  lPropertys := TStringList.Create;
  try
    lPropertys.AddStrings(pPropertys);
    if Supports(pObject, IFreedomObjectList, lFreedomObjectList) then
    begin
      Result := DoIfFreedomObjectList(lFreedomObjectList, lPropertys, pPropertyValue, pFindStrOptions);
    end
    else
    begin
      Result := DoIfSimpleObject(pObject, lPropertyName, lPropertys, pPropertyValue, pFindStrOptions);
    end;
  finally
    lPropertys.Free;
  end;
end;

procedure TFreedomObjectList<T>.DoFixGroupCriteriaForLoad(pGroupCriteria: TGroupCriteria);
begin
  // override when necessary
end;

function TFreedomObjectList<T>.DoIfFreedomObjectList(pFreedomObjectList: IFreedomObjectList; pPropertys: TStrings; pPropertyValue: Variant; pFindStrOptions: TFindStrOptions): Boolean;
var
  lObject: TObject;
begin
  pPropertys.Delimiter := '.';
  pPropertys.StrictDelimiter := True;
  pPropertys.Delete(0);
  lObject := pFreedomObjectList.FindObjectByProperty(pPropertys.DelimitedText, pPropertyValue, pFindStrOptions);
  Result := Assigned(lObject);
end;

function TFreedomObjectList<T>.DoIfSimpleObject(pObject: TObject; pPropertyName: string; pPropertys: TStrings; pPropertyValue: Variant; pFindStrOptions: TFindStrOptions): Boolean;
var
  lMapper: TObjectMapper;
  lColumn: TCustomColumnMapper;
  lObject: TObject;
  lColumnValue: Variant;
begin
  Result := False;
  lMapper := ExtractObjectMapperFrom(pObject, nil, [Properties]);
  try
    lColumn := lMapper.Columns.FindColumn(pPropertyName);
    if Assigned(lColumn) then
    begin
      if lColumn.RttiOptions.RttiProperty.PropertyType.IsInstance then
      begin
        lObject := lColumn.RttiOptions.RttiProperty.GetValue(pObject).AsObject;
        if Assigned(lObject) then
        begin
          if not Supports(lObject, IFreedomObjectList) then
          begin
            pPropertys.Delete(0);
          end;
          Result := FindObjectByPropertyValue(lObject, pPropertys, pPropertyValue, pFindStrOptions);
        end;
      end
      else
      begin
        try
          lColumnValue := lColumn.CurrentValue;
          Result := VarSameValue(lColumnValue, pPropertyValue);
          if (not Result) then
          begin
            if lColumn.ColumnType in [ctyChar, ctyString, ctyMemo] then
            begin
              if fsoCaseInsensitive in pFindStrOptions then
              begin
                if (fsoContaining in pFindStrOptions) then
                begin
                  Result := ContainsText(lColumnValue, pPropertyValue);
                end
                else
                begin
                  Result := SameText(lColumnValue, pPropertyValue);
                end;
              end
              else
              begin
                if (fsoContaining in pFindStrOptions) then
                begin
                  Result := ContainsStr(lColumnValue, pPropertyValue);
                end
                else
                begin
                  Result := SameStr(lColumnValue, pPropertyValue);
                end;
              end;
            end;
          end;
        except
          Result := False;
        end;
      end;
    end;
  finally
    FinalizeObjectMapperFrom(lMapper.GetHashCode);
  end;
end;

procedure TFreedomObjectList<T>.DoSearch(pGroupCriteria: TGroupCriteria);
begin
  DoSearchWithClass(pGroupCriteria, T);
end;

procedure TFreedomObjectList<T>.DoSearchWithClass(pGroupCriteria: TGroupCriteria; pClass: TFreedomObjectClass);
var
  lPersistent: IPersistent;
  lFreeSearch: Boolean;
  lCursor: TCursorList;
  pParams: TObjectToMapperParams;
  lMapper: TObjectMapper;
begin
  Empty;
  lCursor := nil;
  if Assigned(pGroupCriteria) then
  begin
    DoFixGroupCriteriaForLoad(pGroupCriteria);
    pParams := TObjectToMapperParams.Create;
    pParams.MetaClassType := pClass;
    lMapper := TObjectToMapper.ObjectToMapper(pParams);
    try
      pGroupCriteria.ChangeAlias(lMapper.Alias, cmEmptys);
      lPersistent := GetPersistent;
      try
        lCursor := lPersistent.GetCursorList(pClass, pGroupCriteria, lMapper);
        Assign(lCursor, lMapper);
      finally
        FreeAndNil(lCursor);
      end;
    finally
      TObjectToMapper.UnlockMapper(lMapper.GetHashCode);
    end;
  end;
end;

procedure TFreedomObjectList<T>.DoSearchWithSelect(pSelect: TCustomSelect);
var
  lPersistent: IPersistent;
  lCursor: TCursorList;
begin
  Empty;
  lPersistent := GetPersistent;
  lCursor := nil;
  try
    DoFixGroupCriteriaForLoad(pSelect.WhereClause);
    lCursor := lPersistent.GetCursorListWithSelect(pSelect);
    Assign(lCursor, nil);
  finally
    FreeAndNil(lCursor);
  end;
end;

function TFreedomObjectList<T>.ExtractObjectMapperFrom(pObject: TObject; pMetaClassType: TClass; pOptions: TObjectToMapperOptions): TObjectMapper;
var
  lParams: TObjectToMapperParams;
begin
  lParams := TObjectToMapperParams.Create;
  lParams.ObjectInstance := pObject;
  if (pMetaClassType <> nil) then
  begin
    lParams.MetaClassType := pMetaClassType;
  end;
  lParams.Options := pOptions;
  Result := TObjectToMapper.ObjectToMapper(lParams);
end;

function TFreedomObjectList<T>.FindObjectByPropertyNames(pPropertyNames: string; pPropertyValue: Variant; pFindStrOptions: TFindStrOptions): T;
begin
  Result := FindObjectByPropertyNames([pPropertyNames], [pPropertyValue], pFindStrOptions);
end;

function TFreedomObjectList<T>.FindObjectByPropertyValue(pObject: TObject; pPropertys: TStrings; pPropertyValue: Variant; pFindStrOptions: TFindStrOptions): Boolean;
begin
  Result := False;
  if pPropertys.Count > 0 then
  begin
    Result := DoFindObjectByPropertyValue(pObject, pPropertys, pPropertyValue, pFindStrOptions);
  end;
end;

function TFreedomObjectList<T>.FixMapper(pMapper: TObjectMapper): TObjectMapper;
begin
  Result := pMapper;
  FFixedMapper := False;
  if (not Assigned(Result)) then
  begin
    Result := ExtractObjectMapperFrom(nil, ObjectClass);
    FFixedMapper := True;
  end;
  if (Result.CurrentSchema = '') and (Result.Schemas.Count > 0) then
  begin
    Result.CurrentSchema := Result.Schemas.DefaultSchema.Name;
  end;
end;

procedure TFreedomObjectList<T>.FinalizeObjectMapperFrom(pMapperHash: Integer);
begin
  TObjectToMapper.UnlockMapper(pMapperHash);
end;

function TFreedomObjectList<T>.FindListOfObjectByPropertyNames(pObjectClass: TClass;
  pPropertyNames: array of string; pPropertyValue: array of Variant;
  pFindStrOptions: TFindStrOptions): TList<T>;
var
  lObject: T;
begin
  Result := TList<T>.Create;
  lObject := FindObjectByPropertyNames(pObjectClass, pPropertyNames, pPropertyValue, pFindStrOptions);
  while Assigned(lObject) do
  begin
    Result.Add(lObject);
    lObject := FindNext;
  end;
  ResetFind;
end;

function TFreedomObjectList<T>.FindListOfObjectByPropertyNames(pPropertyNames: array of string;
  pPropertyValue: array of Variant; pFindStrOptions: TFindStrOptions): TList<T>;
begin
  Result := FindListOfObjectByPropertyNames(TClass(T), pPropertyNames, pPropertyValue, pFindStrOptions);
end;

function TFreedomObjectList<T>.FindNext: T;
begin
  Result := nil;
  if (Assigned(FFindPropertyList)) then
  begin
    Result := DoFindObjectByPropertyNames;
    if (not Assigned(Result)) then
    begin
      FLastFindResultIndex := 0;
    end;
  end;
end;

function TFreedomObjectList<T>.FindObjectByID(pObjectClass: TClass; pID: Variant): T;
var
  lObject: T;
  lFreedomObject: IFreedomObject;
  lMapper: TObjectMapper;
  lIDColumn: TCustomColumnMapper;
begin
  Result := nil;
  for lObject in Self do
  begin
    if TFreedomObject(lObject).ClassName = pObjectClass.ClassName then
    begin
      lMapper := ExtractObjectMapperFrom(lObject, nil);
      try
        lIDColumn := lMapper.Columns.IDColumn;
        if Assigned(lIDColumn) then
        begin
          if lIDColumn.RttiOptions.RttiFieldHelper.GetVariantValue(TObject(lObject), lIDColumn.ColumnType) = pID then
          begin
            Result := lObject;
            Break;
          end;
        end;
      finally
        FinalizeObjectMapperFrom(lMapper.GetHashCode);
      end;
    end;
  end;
end;

function TFreedomObjectList<T>.FindObjectByPropertyNames(pObjectClass: TClass; pPropertyNames: string; pPropertyValue: Variant; pFindStrOptions: TFindStrOptions): T;
begin
  Result := FindObjectByPropertyNames(pObjectClass, [pPropertyNames], [pPropertyValue], pFindStrOptions);
end;

function TFreedomObjectList<T>.FindObjectByID(pID: Variant): T;
begin
  Result := FindObjectByID(TClass(T), pID);
end;

function TFreedomObjectList<T>.FindObjectByProperty(pPropertyNames: string; pPropertyValue: Variant; pFindStrOptions: TFindStrOptions = []): TObject;
begin
  Result := FindObjectByPropertyNames(pPropertyNames, pPropertyValue);
end;

function TFreedomObjectList<T>.FindObjectByPropertyNames(pPropertyNames: array of string;
  pPropertyValue: array of Variant; pFindStrOptions: TFindStrOptions): T;
begin
  Result := FindObjectByPropertyNames(TClass(T), pPropertyNames, pPropertyValue, pFindStrOptions);
end;

function TFreedomObjectList<T>.FindObjectByPropertyNames(pObjectClass: TClass;
  pPropertyNames: array of string; pPropertyValue: array of Variant;
  pFindStrOptions: TFindStrOptions): T;
begin
  FreeAndNil(FFindPropertyList);
  FFindPropertyList := GenerateFindPropertyList(pPropertyNames, pPropertyValue);
  FLastFindResultIndex := 0;
  FLastFindObjectClass := pObjectClass;
  FLastFindStrOptions := pFindStrOptions;
  Result := DoFindObjectByPropertyNames;
  if (not Assigned(Result)) then
  begin
    FLastFindResultIndex := 0;
  end;
end;

function TFreedomObjectList<T>.GetPersistent: IPersistent;
begin
  if Assigned(FPersistent) then
  begin
    Result := FPersistent;
  end
  else if Assigned(TDefaultsClassRegister.DefaultPersistent) then
  begin
    Result := TDefaultsClassRegister.DefaultPersistent;
  end;
end;

function TFreedomObjectList<T>.IsCompatibleTypes(pColumnType, pColumnValueType: TColumnType): Boolean;
begin
  Result := False;
  case pColumnType of
    ctyByte, ctySmallint, ctyInteger, ctyInt64: Result := pColumnValueType in [ctyByte, ctySmallint, ctyInteger, ctyInt64];
    ctyChar, ctyString: Result := pColumnValueType in [ctyChar, ctyString];
    ctySingle, ctyDouble, ctyCurrency, ctyExtended: Result := pColumnValueType in [ctyByte, ctySmallint, ctyInteger,
        ctyInt64, ctySingle, ctyDouble, ctyCurrency, ctyExtended];
    ctyDate, ctyTime, ctyDateTime: Result := pColumnValueType in [ctyDate, ctyTime, ctyDateTime];
    ctyBoolean: Result := pColumnValueType in [ctyBoolean, ctyByte, ctySmallint, ctyInteger, ctyInt64];
    ctyEnumerator: Result := pColumnValueType in [ctyEnumerator, ctyByte, ctySmallint, ctyInteger, ctyInt64];
  end;
end;

function TFreedomObjectList<T>.IsEmpty: Boolean;
begin
  Result := Count <= 0;
end;

procedure TFreedomObjectList<T>.MakeFilter;
var
  lListOfCriterias: TList<TFilterCriteria>;
  lObject: T;
  lIndex: Integer;
  lMapper: TObjectMapper;
begin
  if (not IsEmpty) then
  begin
    lMapper := nil;
    lListOfCriterias := FFilter.ListOfCriterias;
    try
      for lIndex := Count - 1 downto 0 do
      begin
        lObject := Items[lIndex];
        if (Assigned(lMapper)) then
        begin
          AssignObjectToMapper(lObject, lMapper);
        end
        else
        begin
          lMapper := ExtractObjectMapperFrom(lObject, nil, [Properties]);
        end;
        AssignObjectToFilter(lObject, lListOfCriterias, lMapper);
        if (not FFilter.CompareValues) then
        begin
          Extract(lObject);
          FFilteredList.AddItem(lIndex, lObject);
        end;
      end;
    finally
      if (Assigned(lMapper)) then
      begin
        FinalizeObjectMapperFrom(lMapper.GetHashCode);
      end;
      lListOfCriterias.Free;
    end;
  end;
end;

function TFreedomObjectList<T>.ObjectClass: TFreedomObjectClass;
begin
  if (Assigned(FObjectClass)) then
  begin
    Result := FObjectClass;
  end
  else
  begin
    Result := T;
  end;
end;

procedure TFreedomObjectList<T>.ApplyFilter;
begin
  if FFilterActive then
  begin
    UnMakeFilter;
  end;
  MakeFilter;
  FFilterActive := True;
end;

procedure TFreedomObjectList<T>.Assign(pCursor: TCursorList; pMapper: TObjectMapper);
var
  lCurrentSchema: string;
  lMapper: TObjectMapper;
  lCursor: ICursor;
begin
  ObjectClass.DisableDefaults;
  lMapper := FixMapper(pMapper);
  try
    if (AssignCursorWithThread) then
    begin
      DoAssignCursorWithThread(pCursor, lMapper.CurrentSchema);
    end
    else
    begin
      for lCursor in pCursor do
      begin
        DoAssignCursor(lCursor, lMapper);
      end;
    end;
  finally
    if (FFixedMapper) then
    begin
      TObjectToMapper.UnlockMapper(lMapper.GetHashCode);
    end;
    ObjectClass.EnableDefaults;
  end;
end;

function TFreedomObjectList<T>.AssignCursorWithThread: Boolean;
begin
  Result := False;
end;

procedure TFreedomObjectList<T>.PersistCleanAndInsertedObjects;
var
  lPersistent: IPersistent;
  lObject: TFreedomObject;
begin
  lPersistent := GetPersistent;
  for lObject in Self do
  begin
    if lObject.ObjectState <> Unknown then
    begin
      case lObject.ObjectState of
        Clean:
          lObject.Update(lPersistent);
        Inserted:
          lObject.Insert(lPersistent);
      end;
    end;
  end;
end;

procedure TFreedomObjectList<T>.PersistDeletedObjects;
var
  lPersistent: IPersistent;
  lObject: T;
begin
  lPersistent := GetPersistent;
  if TrashList.Count > 0 then
  begin
    for lObject in TrashList do
    begin
      lObject.Delete;
    end;
    TrashList.Clear;
  end;
end;

procedure TFreedomObjectList<T>.PersistObjects;
begin
  if not FReadOnly then
  begin
    UnMakeFilter;
    PersistCleanAndInsertedObjects;
    RemoveUnknowObjects;
    PersistDeletedObjects;
  end;
end;

procedure TFreedomObjectList<T>.RemoveUnknowObjects;
var
  lObject: TFreedomObject;
  lCounter: Integer;
begin
  for lCounter := Count - 1 downto 0 do
  begin
    lObject := Items[lCounter];
    if lObject.ObjectState = Unknown then
    begin
      Remove(lObject);
    end;
  end;
end;

procedure TFreedomObjectList<T>.ResetFind;
begin
  if (Assigned(FFindPropertyList)) then
  begin
    FLastFindResultIndex := 0;
  end;
end;

procedure TFreedomObjectList<T>.GetObjects(pGroupCriteria: TGroupCriteria);
begin
  GetObjects(pGroupCriteria, T);
end;

function TFreedomObjectList<T>.GetObjectPropertyValue(pObject: T; pPropertyName: String; pMapper: TObjectMapper): Variant;
var
  lObjectPropertyValueFinder: TObjectPropertyValueFinder;
begin
  lObjectPropertyValueFinder := TObjectPropertyValueFinder.Create;
  try
    lObjectPropertyValueFinder.ObjectMapper := pMapper;
    lObjectPropertyValueFinder.CurrentObject := pObject;
    lObjectPropertyValueFinder.PropertyName := pPropertyName;
    Result := lObjectPropertyValueFinder.FindValue;
  finally
    lObjectPropertyValueFinder.Free;
  end;
end;

procedure TFreedomObjectList<T>.GetObjects(pGroupCriteria: TGroupCriteria; pClass: TFreedomObjectClass);
begin
  try
    FObjectClass := pClass;
    DoSearchWithClass(pGroupCriteria, pClass);
  finally
    FObjectClass := nil;
  end;
end;

procedure TFreedomObjectList<T>.GetAllObjects;
begin
  GetAllObjects(T);
end;

function TFreedomObjectList<T>.GenerateFindPropertyList(pPropertyNames: array of String;
    pPropertyValue: array of Variant): TFindPropertyList;
var
  lIndex: Integer;
  lProperties: TStrings;
begin
  Result := TFindPropertyList.Create;
  for lIndex := Low(pPropertyNames) to High(pPropertyNames) do
  begin
    lProperties := TStringList.Create;
    ExtractStrings(['.'], [' '], pWideChar(pPropertyNames[lIndex]), lProperties);
    if (High(pPropertyValue) >= lIndex) then
    begin
      Result.AddItem(lProperties, pPropertyValue[lIndex]);
    end
    else
    begin
      Result.AddItem(lProperties, Null);
    end;
  end;
end;

procedure TFreedomObjectList<T>.GetAllObjects(pClass: TFreedomObjectClass);
var
  xGroupCriteria: TGroupCriteria;
begin
  xGroupCriteria := TGroupCriteria.Create;
  try
    FObjectClass := pClass;
    DoSearchWithClass(xGroupCriteria, pClass);
  finally
    xGroupCriteria.Free;
    FObjectClass := nil;
  end;
end;

procedure TFreedomObjectList<T>.SetFilter(const pFilter: TGroupFilterCriteria);
begin
  if (FFilter <> pFilter) then
  begin
    FreeAndNil(FFilter);
  end;
  FFilter := pFilter;
end;

procedure TFreedomObjectList<T>.SetPersistState(pObjectState: TObjectState);
var
  lObject: TFreedomObject;
  lFreedomObject: IFreedomObject;
begin
  for lObject in Self do
  begin
    if Supports(lObject, IFreedomObject, lFreedomObject) then
    begin
      if (lFreedomObject.ObjectState <> Unknown) then
      begin
        lFreedomObject.SetObjectState(pObjectState);
        lFreedomObject.SetOldObjectState(pObjectState);
      end;
    end;
  end;
end;

procedure TFreedomObjectList<T>.SetRefColumnValue(pRefColumnName: string; pColumnValue: Variant);
var
  lObject: TFreedomObject;
  lMapper: TObjectMapper;
  lColumn: TCustomColumnMapper;
begin
  for lObject in Self do
  begin
    lMapper := ExtractObjectMapperFrom(lObject, nil, []);
    try
      for lColumn in lMapper.Columns do
      begin
        if SameText(lColumn.Name, pRefColumnName) then
        begin
          if IsCompatibleTypes(lColumn.RttiOptions.RttiField.FieldType.ToColumnType, pColumnValue.VariantType) then
          begin
            if (lColumn.IsNullable) then
            begin
              lColumn.RttiOptions.RttiFieldHelper.SetNullableVariantValue(lObject, pColumnValue);
            end
            else
            begin
              lColumn.RttiOptions.RttiFieldHelper.SetVariantValue(lObject, pColumnValue);
            end;
          end;
        end;
      end;
    finally
      FinalizeObjectMapperFrom(lMapper.GetHashCode);
    end;
  end;
end;

procedure TFreedomObjectList<T>.Sort(pSortByPropertyName: string; pStrSortOptions: TStrSortOptions);
begin
  Sort(pSortByPropertyName, [], pStrSortOptions);
end;

procedure TFreedomObjectList<T>.Sort(pSortByPropertyName: string; pSortsType: Array of TSortType; pStrSortOptions: TStrSortOptions);
var
  lObjectComparer: TObjectComparer;
  lArraySortType: TArray<TSortType>;
  lIndex: Integer;
begin
  lObjectComparer := TObjectComparer.Create;
  try
    lObjectComparer.PropertyName := pSortByPropertyName;
    SetLength(lArraySortType, lObjectComparer.FListOfPropertyNames.Count);
    for lIndex := Low(pSortsType) to lObjectComparer.FListOfPropertyNames.Count - 1 do
    begin
      if (lIndex <= High(pSortsType)) then
      begin
        lArraySortType[lIndex] := pSortsType[lIndex];
      end
      else
      begin
        lArraySortType[lIndex] := Asc;
      end;
    end;
    lObjectComparer.SortType := lArraySortType;
    lObjectComparer.StrSortOptions := pStrSortOptions;
    inherited Sort(lObjectComparer);
  finally
    lObjectComparer.Free;
  end;
end;

procedure TFreedomObjectList<T>.UnMakeFilter;
var
  lIndex: Integer;
begin
  if (Assigned(FFilteredList)) then
  begin
    for lIndex := FFilteredList.Count - 1 downto 0 do
    begin
      Insert(FFilteredList.Items[lIndex].Index, FFilteredList.Items[lIndex].CurrentObject);
    end;
    FFilteredList.Clear;
  end;
  FFilterActive := False;
end;

procedure TFreedomObjectList<T>.UnpersistInternalList;
var
  lCounter: Integer;
  lObject: TFreedomObject;
  lFreedomObject: IFreedomObject;
begin
  for lCounter := Count - 1 downto 0 do
  begin
    lObject := Items[lCounter];
    if lObject.ObjectState = Clean then
    begin
      SetObjectState(lObject, Clean);
      SetOldObjectState(lObject, Clean);
      Supports(lObject, IFreedomObject, lFreedomObject);
      lFreedomObject.AssignInitialValues;
    end
    else
    begin
      Remove(lObject);
    end;
  end;
end;

procedure TFreedomObjectList<T>.UnpersistObjects;
begin
  UnMakeFilter;
  UnpersistTrashList;
  UnpersistInternalList;
end;

procedure TFreedomObjectList<T>.UnpersistTrashList;
var
  lCounter: Integer;
  lItem: TFreedomObject;
begin
  for lCounter := TrashList.Count - 1 downto 0 do
  begin
    if TFreedomObject(TrashList.Items[lCounter]).OldObjectState = Clean then
    begin
      lItem := TrashList.Items[lCounter];
      Add(TrashList.Extract(lItem));
      SetObjectState(lItem, Clean);
    end;
  end;
  TrashList.Clear;
end;
{ TFreedomObjectList<T>.TObjectComparer }

function TFreedomObjectList<T>.TObjectComparer.Compare(const pLeftObject, pRightObject: T): Integer;
const
  cNoComparerResult = 0;
var
  lParams: TObjectToMapperParams;
  lLeftMapper, lRigthMapper: TObjectMapper;
  lLeftColumn, lRigthColumn: TCustomColumnMapper;
  lLeftValue: Variant;
  lRigthValue: Variant;
  lPropertyName: String;
  lListOfResults: Array of Integer;
  lIndex: Integer;
begin
  lParams := TObjectToMapperParams.Create;
  lParams.Options := [TObjectToMapperOption.Properties];
  lParams.ObjectInstance := pLeftObject;
  lLeftMapper := TObjectToMapper.ObjectToMapper(lParams);
  lParams := TObjectToMapperParams.Create;
  lParams.Options := [TObjectToMapperOption.Properties];
  lParams.ObjectInstance := pRightObject;
  lRigthMapper := TObjectToMapper.ObjectToMapper(lParams);
  SetLength(lListOfResults, FListOfPropertyNames.Count);

  try
    for lPropertyName in FListOfPropertyNames do
    begin
      Result := cNoComparerResult;
      lLeftColumn := lLeftMapper.Columns.FindColumn(lPropertyName);
      lRigthColumn := lRigthMapper.Columns.FindColumn(lPropertyName);
      if (Assigned(lLeftColumn)) and Assigned(lRigthColumn) then
      begin
        if (lLeftColumn.ColumnType = lRigthColumn.ColumnType) then
        begin
          lLeftValue := lLeftColumn.CurrentValue;
          lRigthValue := lRigthColumn.CurrentValue;
          case lLeftColumn.ColumnType of
            ctyChar, ctyString:
              begin
                if (ssoCaseInsensitive in FStrSortOptions) then
                begin
                  lLeftValue := UpperCase(lLeftValue);
                  lRigthValue := UpperCase(lRigthValue);
                end;
                if (FSortType[FListOfPropertyNames.IndexOf(lPropertyName)] = Desc) then
                begin
                  Result := CompareStr(lRigthValue, lLeftValue);
                end
                else
                begin
                  Result := CompareStr(lLeftValue, lRigthValue);
                end;
              end;
            ctyDetail, ctyJoin, ctyBlob, ctyMemo, ctyGuid:
              Result := cNoComparerResult;
          else
            begin
              if (FSortType[FListOfPropertyNames.IndexOf(lPropertyName)] = Desc) then
              begin
                Result := CompareValue(lRigthValue, lLeftValue);
              end
              else
              begin
                Result := CompareValue(lLeftValue, lRigthValue);
              end;
            end;
          end;
        end;
        lListOfResults[FListOfPropertyNames.IndexOf(lPropertyName)] := Result;
      end
      else
      begin
        raise ESortPropertyNotFound.Create(lPropertyName, pLeftObject.ClassName);
      end;
    end;
    Result := cNoComparerResult;
    for lIndex := Low(lListOfResults) to High(lListOfResults)  do
    begin
      if (lListOfResults[lIndex] <> cNoComparerResult) then
      begin
        Result := lListOfResults[lIndex];
        Break;
      end;
    end;
  finally
    TObjectToMapper.UnlockMapper(lLeftMapper.GetHashCode);
    TObjectToMapper.UnlockMapper(lRigthMapper.GetHashCode);
  end;
end;
destructor TFreedomObjectList<T>.TObjectComparer.Destroy;
begin
  FListOfPropertyNames.Free;
  inherited;
end;

procedure TFreedomObjectList<T>.TObjectComparer.GenerateListOfPropertyNames;
begin
  FListOfPropertyNames := TStringList.Create;
  ExtractStrings([';', ','], [' '], PWideChar(FPropertyName), FListOfPropertyNames);
end;

procedure TFreedomObjectList<T>.TObjectComparer.SetPropertyName(const Value: string);
begin
  FPropertyName := Value;
  GenerateListOfPropertyNames;
end;

{ TFreedomObjectList<T>.TAssignCursorThread }

constructor TFreedomObjectList<T>.TAssignCursorThread.Create;
begin
  inherited Create(True);
  Priority := tpHigher;
  FCursor := nil;
  FObjectList := TList<T>.Create;
  FreeOnTerminate := False;
  FFinished := False;
end;

destructor TFreedomObjectList<T>.TAssignCursorThread.Destroy;
begin
  FCursor := nil;
  FObjectList.Free;
  inherited;
end;

function TFreedomObjectList<T>.TAssignCursorThread.DoExtractInterface(pObject: TObject): IFreedomObject;
var
  lSupports: Boolean;
begin
  lSupports := (pObject <> nil) and pObject.GetInterface(IFreedomObject, Result);
end;

procedure TFreedomObjectList<T>.TAssignCursorThread.Execute;
var
  lObject: T;
  lColumnValueList: TColumnValueList;
  lObjectToMapperAssigner: TObjectToMapperAssigner;
begin
  try
    if Assigned(FCursor) then
    begin
      lObjectToMapperAssigner := TObjectToMapperAssigner.Create;
      try
        FCursor.First;
        while not FCursor.Eof do
        begin
          lObject := T(FObjectClass.Create);
          lObjectToMapperAssigner.AssignObjectToMapper(FObjectMapper, lObject);
          FIFreedomObject := DoExtractInterface(lObject);
          lColumnValueList := FIFreedomObject.GetColumnValueList;
          PersistentRecordToObject(lColumnValueList);
          SetObjectState(lObject, Clean);
          SetOldObjectState(lObject, Clean);
          FObjectList.Add(lObject);
          FCursor.Next;
        end;
      finally
        lObjectToMapperAssigner.Free;
        ForceTerminate;
      end;
    end;
  except
    on E:Exception do
    begin
      ForceTerminate;
    end;
  end;
end;

function TFreedomObjectList<T>.TAssignCursorThreadList.ExtractObjectMapperFrom(pObjectClass: TFreedomObjectClass): TObjectMapper;
var
  lParams: TObjectToMapperParams;
begin
  lParams := TObjectToMapperParams.Create;
  lParams.MetaClassType := pObjectClass;
  lParams.Options := [SubLevels];
  Result := TObjectToMapper.ObjectToMapper(lParams);
end;

procedure TFreedomObjectList<T>.TAssignCursorThreadList.OnThreadTerminate(Sender: TObject);
begin
//  StartNext;
end;

procedure TFreedomObjectList<T>.TAssignCursorThread.ForceTerminate;
begin
  FFinished := True;
  if (Assigned(OnTerminate)) then
  begin
    OnTerminate(Self);
    OnTerminate := nil;
  end;
end;

procedure TFreedomObjectList<T>.TAssignCursorThread.PersistentRecordToObject(pColumnValueList: TColumnValueList);
var
  lCursorReader: TObjectWriter;
begin
  lCursorReader := TObjectWriter.Create;
  try
    lCursorReader.Mapper := FObjectMapper;
    lCursorReader.Cursor := FCursor;
    lCursorReader.Schema := FCurrentSchema;
    lCursorReader.ColumnValueList := pColumnValueList;
    lCursorReader.ReadRecord;
  finally
    lCursorReader.Free;
  end;
end;

procedure TFreedomObjectList<T>.TAssignCursorThread.SetObjectState(pObject: T; pState: TObjectState);
begin
  FIFreedomObject.SetObjectState(pState);
end;

procedure TFreedomObjectList<T>.TAssignCursorThread.SetOldObjectState(pObject: T; pState: TObjectState);
begin
  FIFreedomObject.SetOldObjectState(pState);
end;
{ TFreedomObjectList<T>.TAssignCursorThreadList }

procedure TFreedomObjectList<T>.TAssignCursorThreadList.AddThread(pThread: TAssignCursorThread);
begin
  Add(pThread);
end;

procedure TFreedomObjectList<T>.TAssignCursorThreadList.AssignCursor(pCursor: TCursorList; pObjectClass: TFreedomObjectClass;
    pCurrentSchema: String; pObjectList: TFreedomObjectList<T>);
var
  lCursor: ICursor;
  lThread: TAssignCursorThread;
  lLastThreadDone: Integer;
begin
  Clear;
  for lCursor in pCursor do
  begin
    lThread := TAssignCursorThread.Create;
    lThread.ObjectClass := pObjectClass;
    lThread.CurrentSchema := pCurrentSchema;
    lThread.Cursor := lCursor;
    lThread.ObjectMapper := ExtractObjectMapperFrom(pObjectClass);
    lThread.OnTerminate := OnThreadTerminate;
    AddThread(lThread);
  end;
  StartAll;
  while lLastThreadDone <= Count - 1 do
  begin
    if (Items[lLastThreadDone].Finished) then
    begin
      lThread := Items[lLastThreadDone];
      Inc(lLastThreadDone);
      pObjectList.AddRange(lThread.ObjectList.ToArray);
      TObjectToMapper.RemoveIncompleteMapper(lThread.ObjectMapper.GetHashCode);
      lThread.OnTerminate := nil;
    end;
  end;
end;

procedure TFreedomObjectList<T>.TAssignCursorThreadList.AssignCursor2(pCursor: TCursorList;
  pObjectClass: TFreedomObjectClass; pCurrentSchema: String; pObjectList: TFreedomObjectList<T>);
var
  lCursor: ICursor;
  lThread: TAssignCursorThread;
  lLastThreadDone: Integer;
begin
  Clear;
  for lCursor in pCursor do
  begin
    lThread := TAssignCursorThread.Create;
    lThread.ObjectClass := pObjectClass;
    lThread.CurrentSchema := pCurrentSchema;
    lThread.Cursor := lCursor;
    lThread.ObjectMapper := ExtractObjectMapperFrom(pObjectClass);
    lThread.OnTerminate := OnThreadTerminate;
    AddThread(lThread);
  end;
  FLastIndex := -1;
  lLastThreadDone := 0;
  StartNext;
  while lLastThreadDone <= Count - 1 do
  begin
    if (Items[lLastThreadDone].Finished) then
    begin
      lThread := Items[lLastThreadDone];
      Inc(lLastThreadDone);
      pObjectList.AddRange(lThread.ObjectList.ToArray);
      TObjectToMapper.RemoveIncompleteMapper(lThread.ObjectMapper.GetHashCode);
      lThread.OnTerminate := nil;
    end;
  end;
end;

procedure TFreedomObjectList<T>.TAssignCursorThreadList.StartAll;
var
  lThread: TAssignCursorThread;
begin
  for lThread in Self do
  begin
    lThread.Start;
  end;
end;

procedure TFreedomObjectList<T>.TAssignCursorThreadList.StartNext;
var
  lNextThread: TAssignCursorThread;
begin
  if (FLastIndex + 1 <= Count - 1) then
  begin
    lNextThread := Items[FLastIndex + 1];
    FLastIndex := FLastIndex + 1;
    lNextThread.Start;
  end;
end;

procedure TFreedomObjectList<T>.MakeCreate;
begin
  inherited;
  FReadOnly := False;
  FFilteredList := TFilteredList<T>.Create;
  FFilter := TGroupFilterCriteria.Create;
  FFilterActive := False;
end;

end.

