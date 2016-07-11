unit AM.Freedom.FreedomObjectDataset;

interface

uses
  System.Generics.Collections,
  System.Classes,
  Data.DB,
  System.SysUtils,
  System.TypInfo,
  AM.Freedom.FreedomObject,
  AM.Freedom.FreedomObjectList,
  AM.Freedom.IFreedomObjectList,
  AM.Freedom.ObjectMapper.ColumnOptions,
  AM.Freedom.ObjectMapper,
  System.Rtti,
  AM.Freedom.GroupCriteria,
  AM.Freedom.GroupFilterCriteria;

type
  TFreedomEntityField = class(TVariantField)
  private
    {$IFDEF NEXTGEN}
    FObject: TObject;
    {$ENDIF}
    function GetAsObject: TObject;
    procedure SetAsObject(const pValue: TObject);
  protected
    {$IFDEF DELPHIXE3_LVL}
    function GetAsVariant: Variant; override;
    procedure SetVarValue(const pValue: Variant); override;
    {$ENDIF}
  public
    property AsObject: TObject read GetAsObject write SetAsObject;
    function AsEntity<T: TFreedomObject>: T;
  end;

  TDatasetCreateObjectEvent = procedure(pDataset: TDataset; var pObject: TObject) of object;
  TDatasetObjectEvent = procedure(pDataset: TDataset; pObject: TObject) of object;
  TCloneRecordOption = (croNewObject, croKeepEdit);

  TFieldValue = class
  private
    FFieldName: String;
    FValue: Variant;
  public
    constructor Create(pFieldName: String; pValue: Variant);
    property FieldName: String read FFieldName write FFieldName;
    property Value: Variant read FValue write FValue;
  end;

  TFieldValueList = class(TObjectList<TFieldValue>)
  public
    procedure AddFieldValue(pFieldName: String; pValue: Variant);
  end;

  TDataSetEvent = procedure (pDataSet: TDataSet) of object;
  TOnBeforeCloneRecord = procedure (pDataSet: TDataSet; pFieldValueList: TFieldValueList) of object;

  TCustomFreedomObjectDataset = class(TDataset)
  strict private
    type
      ValueType = Variant;
      PRecInfo = ^TRecInfo;

      TRecInfo = record
        ListIndex: integer;
        ObjectInfo: TObject;
        BookmarkFlag: TBookmarkFlag;
      end;

      PValueList = ^TValueList;
      TValueList = array[0..0] of ValueType;
  strict private const
    EMPTY_GUID: TGUID = '{00000000-0000-0000-0000-000000000000}';
  strict private
    FIsSelfSource: Boolean;
    FSourceList: TFreedomObjectList<TFreedomObject>;
    FInternalList: TFreedomObjectList<TFreedomObject>;
    FCurrentRecNo: integer;
    FIsOpen: boolean;
    FBufferSize: integer;
    FRecInfoOffset: integer;
    FRecordSize: integer;
    FOldValueBuffer: TRecBuf;
    FFilterBuffer: TRecBuf;
    FModifiedFields: TList<TField>;
    FObjectClass: TFreedomObjectClass;
    FInternalCriteria: TGroupCriteria;
    FOnBeforeCloneRecord: TOnBeforeCloneRecord;
    FOnAfterCloneRecord: TDataSetEvent;
    FChildsEnabled: Boolean;
    procedure SplitProp(const pText: string; var pProp, pSubProp: string);
    function ListRecordCount: integer;
    function GetBufferValueList(pBuffer: TRecBuf): PValueList;
    function GetPropValue(pPropName: string; pObject: TObject): ValueType;
    procedure SetPropValue(pPropName: string; pObject: TObject; pValue: ValueType);
    function ExtractMapperFrom(pClass: TClass; pObject: TObject): TObjectMapper;
    procedure FinalizeMapperFrom(pMapperHash: Integer);
    function ValueToVariant(pValue: TValue; pTypeInfo: PTypeInfo): Variant; overload;
    function ValueToVariant(pValue: TValue): Variant; overload;
    function VariantToValue(pValue: Variant; pTypInfo: PTypeInfo): TValue; overload;
    function VariantToValue(pValue: Variant): TValue; overload;
    function IsDynamicArray(pTypInfo: PTypeInfo): Boolean;
    function GuidToVariant(pGuid: TGuid): Variant;
    function BytesToVariant(pBytes: TBytes): Variant;
    function CreateObject: TObject;
    function GetRecordInfo(Buffer: TRecBuf): PRecInfo;
    procedure UpdateListFromParent(Field: TDatasetField);
    function ExtractObjectClassTypeFromObjectList(pClassInfo: Pointer): TFreedomObjectClass;
    function GetObjectClassTypeFromObjectList(pContext: TRttiContext; pRttiType: TRttiType): TFreedomObjectClass;
    function InternalGetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: boolean): TGetResult;
    procedure GetTheFieldList(pListFields: TList<TField>; const FieldNames: string);
    function GetFieldVariant(Field: TField; var Data: ValueType): boolean;
    procedure DoInvalidObjectClass;
  private const
    NullBuffer: TRecBuf = 0;
  private
    FObjectClassName: String;
    FScrollRecNo: Integer;
    FBookmarkToClonedObject: TBookmark;
    FApplyAfterPost: Boolean;
    function IsEnumeration(ATypeInfo: PTypeInfo): Boolean;
    function GetActiveRecBuf: TRecBuf;
    class function ObjectToVariant(Obj: TObject): Variant;
    class function VariantToObject(V: Variant): TObject;
    procedure SetObjectClass(const Value: TFreedomObjectClass);
    procedure SetChildsObjectClass;
    function GetBlobData(Field: TField): TBytes;
    function VariantToBytes(pValue: Variant): TBytes;
    function VariantToGuid(pVariant: Variant): TGuid;
    procedure DoCloneOnNewObject(pObject: TFreedomObject; pIgnoreFieldNames: Array of String; pDetails: Boolean = False);
    procedure DoCloneKeepEdit(pObject: TFreedomObject; pIgnoreFieldNames: Array of String; pDetails: Boolean = False);
    function GetGroupFilterCriteria: TGroupFilterCriteria;
    procedure ClearSourceList;
    procedure RefreshDataSetFields;
  protected
    {$HINTS OFF}
    property Filter;
    property Filtered;
    {$HINTS ON}
    {$IFDEF DELPHIXE2_LVL}
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
    {$ELSE}
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    {$ENDIF}
    {$IFDEF NEXTGEN}
    function AllocRecBuf: TRecBuf; override;
    procedure FreeRecBuf(var Buffer: TRecBuf); override;
    {$ELSE}
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    {$ENDIF}
    {$IFNDEF NEXTGEN}
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; overload; override;
    {$ENDIF}
    procedure CreateFields; override;
    function CreateNestedDataSet(DataSetField: TDataSetField): TDataSet; override;
    function LocateRecord(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions; SyncCursor: Boolean; ResultFields: string): Boolean; virtual;
    function GetCanModify: boolean; override;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    function GetRecordCount: integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: integer); override;
    procedure DoAfterOpen; override;
    procedure DoOnNewRecord; override;
    procedure InternalEdit; override;
    procedure SetDataSetField(const Value: TDataSetField); override;
    procedure GetBookmarkData(Buffer: TRecBuf; Data: TBookmark); overload; override;
    procedure SetBookmarkData(Buffer: TRecBuf; Data: TBookmark); overload; override;
    function GetBookmarkFlag(Buffer: TRecBuf): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag); override;
    function GetRecordSize: Word; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: TBookmark); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalLast; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: TRecBuf); override;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); overload; override;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer; NativeFormat: boolean); override;
    function GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult; overload; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: boolean; override;
    function FindRecord(Restart: Boolean; GoForward: Boolean): Boolean; override;
    procedure SetChildsDisabled;
    procedure SetChildsEnabled;
    procedure CloseChilds;
    procedure OpenChilds;
 protected
    procedure InternalSetSourceList(pSourceList: TFreedomObjectList<TFreedomObject>);
    procedure DoAfterCancel; override;
    procedure DoBeforeClose; override;
    function EntityFieldByName(const FieldName: string): TFreedomEntityField;
    procedure SetSourceList(pSourceList: TFreedomObjectList<TFreedomObject>);
    function InIgnoreList(pIgnoreFields: Array of String; pFieldName: String): Boolean;
    procedure DoBeforeCloneRecord(pFieldValueList: TFieldValueList);
    procedure DoAfterCloneRecord;
    procedure DoAfterPost; override;
    function SourceList: TFreedomObjectList<TFreedomObject>;
    property ObjectView default True;
    property ObjectClass: TFreedomObjectClass read FObjectClass write SetObjectClass;
    property ObjectClassName: String read FObjectClassName write FObjectClassName;
    property OnBeforeCloneRecord: TOnBeforeCloneRecord read FOnBeforeCloneRecord write FOnBeforeCloneRecord;
    property OnAfterCloneRecord: TDataSetEvent read FOnAfterCloneRecord write FOnAfterCloneRecord;
    property ApplyAfterPost: Boolean read FApplyAfterPost write FApplyAfterPost default False;
    property FilterCriteria: TGroupFilterCriteria read GetGroupFilterCriteria;

  public
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    {$IFDEF DELPHIXE4_LVL}
    function GetFieldData(Field: TField; Buffer: TValueBuffer): boolean; override;
    {$ELSE}
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): boolean; override;
    {$ENDIF}
    function GetCurrentRecord(Buffer: TRecBuf): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    procedure Open(pGroupCriteria: TGroupCriteria = nil); reintroduce;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
    function InternalList<T: TFreedomObject, constructor>: TFreedomObjectList<T>;

    function Current<T: TFreedomObject>: T;
    function CurrentObject: TObject;
    procedure CloneRecord(pObject: TFreedomObject; pIgnoreFieldNames: Array of String; pDetails: Boolean = False;
        pCloneRecordOption: TCloneRecordOption = croKeepEdit);
    procedure CloneCurrentRecord(pIgnoreFieldNames: Array of String; pDetails: Boolean = False;
        pCloneRecordOption: TCloneRecordOption = croKeepEdit);
    procedure ApplyUpdates;
    procedure CancelUpdates;
    procedure Reopen;
    procedure BeginScroll;
    procedure EndScroll;
    procedure ApplyFilter(pBeginScroll: Boolean = False);
    procedure CancelFilter(pEndScroll: Boolean = False);
    procedure DisableChilds;
    procedure EnableChilds;
    procedure EmptyDataSet;
    function IsNotEmpty: Boolean;

    property ChildsEnabled: Boolean read FChildsEnabled;
  end;

  TFreedomObjectDataset = class(TCustomFreedomObjectDataset)
  published
    property ApplyAfterPost;
    property FilterCriteria;
    property FieldDefs;
    property ObjectClass;
    property ObjectClassName;
    property DatasetField;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
    property OnBeforeCloneRecord;
    property OnAfterCloneRecord;
  end;

  TObjBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TCustomFreedomObjectDataset;
    FBuffer: TRecBuf;
    FFieldNo: integer;
    FModified: boolean;
  protected
    procedure ReadBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure Truncate;
  end;

implementation

uses
  DBConsts,
  Variants,
  FmtBCD,
  System.Math,
  System.AnsiStrings,
  System.StrUtils,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.Exceptions,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.INullable,
  Data.SqlTimSt,
  AM.Freedom.NullableCompare;

{ TCustomFreedomObjectDataset }

{$IFDEF NEXTGEN}
function TCustomFreedomObjectDataset.AllocRecBuf: TRecBuf;
begin
  Result := TRecBuf(AllocRecordBuffer);
end;
{$ELSE}
function TCustomFreedomObjectDataset.AllocRecordBuffer: TRecordBuffer;
begin
  Result := AllocMem(FBufferSize);
end;
{$ENDIF}

{$IFDEF NEXTGEN}
procedure TCustomFreedomObjectDataset.FreeRecBuf(var Buffer: TRecBuf);
begin
  FreeRecordBuffer(TRecordBuffer(Buffer))
end;
{$ELSE}
procedure TCustomFreedomObjectDataset.FinalizeMapperFrom(pMapperHash: Integer);
begin
  TObjectToMapper.UnlockMapper(pMapperHash);
end;

function TCustomFreedomObjectDataset.FindRecord(Restart, GoForward: Boolean): Boolean;
  function IsFirst: Boolean;
  begin
    Result := Restart and GoForward;
  end;
  function IsPrior: Boolean;
  begin
    Result := not Restart and not GoForward;
  end;
  function IsNext: Boolean;
  begin
    Result := not Restart and GoForward;
  end;
  function IsLast: Boolean;
  begin
    Result := Restart and not GoForward;
  end;
begin
  Result := True;
  if (IsFirst) then
  begin
    Result := FCurrentRecNo >= 0;
    FCurrentRecNo := -1;
  end
  else if (IsPrior) then
  begin
    Result := FCurrentRecNo >= 0;
    if Result then
    begin
      Dec(FCurrentRecNo);
    end;
  end else if (IsNext) then
  begin
    Result := FCurrentRecNo < ListRecordCount;
    if (Result) then
    begin
      inc(FCurrentRecNo);
    end;
  end else if (IsLast) then
  begin
    Result := FCurrentRecNo < ListRecordCount;
    FCurrentRecNo := ListRecordCount - 1;
  end
end;

procedure TCustomFreedomObjectDataset.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  if (RecordCount > 0) then
  begin
    Finalize(GetBufferValueList(NativeInt(Buffer))^, Fields.Count);
    FreeMem(Buffer, FBufferSize);
  end;
end;
{$ENDIF}

procedure TCustomFreedomObjectDataset.ApplyFilter(pBeginScroll: Boolean);
begin
  if (pBeginScroll) then
  begin
    BeginScroll;
  end;
  SourceList.ApplyFilter;
  if (not pBeginScroll) then
  begin
    Refresh;
  end;
end;

procedure TCustomFreedomObjectDataset.ApplyUpdates;
begin
  if (SourceList <> nil) and (DataSetField = nil) then
  begin
    DisableControls;
    try
      SourceList.PersistObjects;
      Refresh;
    finally
      EnableControls;
    end;
  end;
end;

procedure TCustomFreedomObjectDataset.BeginScroll;
begin
  FScrollRecNo := RecNo;
  if (not ControlsDisabled) then
  begin
    DisableControls;
  end;
  First;
end;

function TCustomFreedomObjectDataset.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := SourceList.IndexOf(TFreedomObject(PObject(Bookmark)^)) >= 0;
end;

function TCustomFreedomObjectDataset.BytesToVariant(pBytes: TBytes): Variant;
var
  lBounds: array of integer;
  lLock: Pointer;
begin
  VarClear(Result);
  if Length(pBytes) = 0 then
    Result := Null;

  SetLength(lBounds, 2);
  lBounds[0] := 0;
  lBounds[1] := Length(pBytes) - 1;
  Result := VarArrayCreate(lBounds, varByte);
  if pBytes <> nil then
  begin
    lLock := VarArrayLock(Result);
    Move(pBytes[0], lLock^, Length(pBytes));
    VarArrayUnlock(Result);
  end;
end;

procedure TCustomFreedomObjectDataset.CancelFilter(pEndScroll: Boolean);
begin
  SourceList.CancelFilter;
  if (pEndScroll) then
  begin
    EndScroll;
  end;
  if (not pEndScroll) then
  begin
    Refresh;
  end;
end;

procedure TCustomFreedomObjectDataset.CancelUpdates;
begin
  if (SourceList <> nil) and (DataSetField = nil) then
  begin
    DisableControls;
    try
      SourceList.UnpersistObjects;
      Refresh;
    finally
      EnableControls;
    end;
  end;
end;

procedure TCustomFreedomObjectDataset.CloneCurrentRecord(pIgnoreFieldNames: array of String; pDetails: Boolean;
  pCloneRecordOption: TCloneRecordOption);
var
  lObject: TFreedomObject;
begin
  CheckBrowseMode;
  lObject := Current<TFreedomObject>;
  CloneRecord(lObject, pIgnoreFieldNames, pDetails, pCloneRecordOption);
end;

procedure TCustomFreedomObjectDataset.CloneRecord(pObject: TFreedomObject; pIgnoreFieldNames: Array of String; pDetails: Boolean; pCloneRecordOption: TCloneRecordOption);
begin
  if (Assigned(pObject)) and (pObject.InheritsFrom(FObjectClass)) then
  begin
    CheckBrowseMode;
    case pCloneRecordOption of
      croNewObject: DoCloneOnNewObject(pObject, pIgnoreFieldNames, pDetails);
      croKeepEdit: DoCloneKeepEdit(pObject, pIgnoreFieldNames, pDetails);
    end;
  end;
end;

procedure TCustomFreedomObjectDataset.CloseChilds;
var
  lIndex: Integer;
  lDataSet: TCustomFreedomObjectDataset;
begin
  if FChildsEnabled and Assigned(NestedDataSets) then
  begin
    for lIndex := 0 to NestedDataSets.Count - 1 do
    begin
      if NestedDatasets.Items[lIndex] is TCustomFreedomObjectDataset then
      begin
        lDataSet := TCustomFreedomObjectDataset(NestedDataSets.Items[lIndex]);
        lDataSet.Close;
      end;
    end;
  end;
end;

function TCustomFreedomObjectDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
var
  lIndexBookmark1: Integer;
  lIndexBookmark2: Integer;
begin
  Result := 0;
  if (Bookmark1 <> nil) and (Bookmark2 <> nil) then
  begin
    lIndexBookmark1 := SourceList.IndexOf(TFreedomObject(PObject(Bookmark1)^));
    lIndexBookmark2 := SourceList.IndexOf(TFreedomObject(PObject(Bookmark2)^));
    Result := CompareValue(lIndexBookmark1, lIndexBookmark2)
  end;
end;

constructor TCustomFreedomObjectDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModifiedFields := TList<TField>.Create;
  BookmarkSize := SizeOf(TObject);
  ObjectView := True;
  FApplyAfterPost := False;
  FChildsEnabled := True;
end;

function TCustomFreedomObjectDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TObjBlobStream.Create(Field as TBlobField, Mode);
end;

procedure TCustomFreedomObjectDataset.CreateFields;
begin
  inherited CreateFields;
end;

function TCustomFreedomObjectDataset.CreateNestedDataSet(DataSetField: TDataSetField): TDataSet;
begin
  Result := inherited CreateNestedDataset(DataSetField);
end;

function TCustomFreedomObjectDataset.CreateObject: TObject;
begin
  Result := FObjectClass.Create;
end;

function TCustomFreedomObjectDataset.Current<T>: T;
begin
  Result := T(CurrentObject);
end;

function TCustomFreedomObjectDataset.CurrentObject: TObject;
var
  RecBuf: TRecBuf;
begin
  Result := nil;
  RecBuf := GetActiveRecBuf;
  if RecBuf <> NullBuffer then
  begin
    Result := GetRecordInfo(RecBuf)^.ObjectInfo;
  end;
end;

{$IFDEF DELPHIXE2_LVL}
procedure TCustomFreedomObjectDataset.DataEvent(Event: TDataEvent; Info: Longint);
{$ELSE}
procedure TCustomFreedomObjectDataset.DataEvent(Event: TDataEvent; Info: NativeInt);
{$ENDIF}
begin
  if (Event = deParentScroll) and (DataSetField <> nil) and
     (TCustomFreedomObjectDataset(DataSetField.DataSet).FChildsEnabled) then
  begin
    UpdateListFromParent(DataSetField);
    Resync([]);
  end;
  inherited;
end;

destructor TCustomFreedomObjectDataset.Destroy;
begin
  FModifiedFields.Free;
  if (DataSetField = nil) then
  begin
    FreeAndNil(FSourceList);
  end;
  FreeAndNil(FInternalList);
  FreeAndNil(FInternalCriteria);
  inherited;
end;

procedure TCustomFreedomObjectDataset.DisableChilds;
begin
  SetChildsDisabled;
end;

procedure TCustomFreedomObjectDataset.DoAfterCancel;
var
  lObject: TFreedomObject;
begin
  inherited;
  if (FBookmarkToClonedObject <> nil) then
  begin
    GotoBookmark(FBookmarkToClonedObject);
    lObject := Current<TFreedomObject>;
    SourceList.Extract(lObject);
    lObject.Free;
    FBookmarkToClonedObject := nil;
    Refresh;
  end;
end;

procedure TCustomFreedomObjectDataset.DoAfterCloneRecord;
begin
  if (Assigned(FOnAfterCloneRecord)) then
  begin
    FOnAfterCloneRecord(Self);
  end;
end;

procedure TCustomFreedomObjectDataset.DoAfterOpen;
begin
  OpenChilds;
  inherited;
end;

procedure TCustomFreedomObjectDataset.DoAfterPost;
begin
  inherited;
  if (FApplyAfterPost) then
  begin
    ApplyUpdates;
  end;
end;

procedure TCustomFreedomObjectDataset.DoBeforeCloneRecord(pFieldValueList: TFieldValueList);
begin
  if (Assigned(FOnBeforeCloneRecord)) then
  begin
    FOnBeforeCloneRecord(Self, pFieldValueList);
  end;
end;

procedure TCustomFreedomObjectDataset.DoBeforeClose;
begin
  CloseChilds;
  inherited;
end;

procedure TCustomFreedomObjectDataset.DoCloneKeepEdit(pObject: TFreedomObject; pIgnoreFieldNames: Array of String; pDetails: Boolean);
begin
  DoCloneOnNewObject(pObject, pIgnoreFieldNames, pDetails);
  Edit;
end;

procedure TCustomFreedomObjectDataset.DoCloneOnNewObject(pObject: TFreedomObject; pIgnoreFieldNames: Array of String; pDetails: Boolean);
var
  lObject: TObject;
begin
  DisableControls;
  try
    Cancel;
    lObject := CreateObject;
    TFreedomObject(lObject).CopyFrom(pObject, pIgnoreFieldNames, pDetails, True);
    SourceList.Insert(TFreedomObject(lObject));
    Last;
    FBookmarkToClonedObject := GetBookmark;
    Refresh;
  finally
    EnableControls;
  end;
end;

procedure TCustomFreedomObjectDataset.DoInvalidObjectClass;
begin
  if (Owner <> nil) then
  begin
    raise EInvalidObjectClass.Create(Self.Name, Self.Owner.Name);
  end
  else
  begin
    raise EInvalidObjectClass.Create(Self.Name, '');
  end;
end;

procedure TCustomFreedomObjectDataset.DoOnNewRecord;
begin
  FModifiedFields.Clear;
  inherited;
end;

procedure TCustomFreedomObjectDataset.EnableChilds;
begin
  SetChildsEnabled;
end;

procedure TCustomFreedomObjectDataset.EndScroll;
begin
  if (FScrollRecNo >= 0) then
  begin
    RecNo := FScrollRecNo;
    FScrollRecNo := -1;
  end;
  if (ControlsDisabled) then
  begin
    EnableControls;
  end;
end;

function TCustomFreedomObjectDataset.EntityFieldByName(const FieldName: string): TFreedomEntityField;
var
  lField: TField;
begin
  lField := FindField(FieldName);
  if lField = nil then
  begin
    DatabaseErrorFmt(SFieldNotFound, [FieldName], Self);
  end;
  if not (lField is TFreedomEntityField) then
  begin
    DatabaseErrorFmt(SFieldTypeMismatch, [FieldName, 'TFreedomEntityField', lField.ClassName], Self);
  end;
  Result := TFreedomEntityField(lField);
end;

function TCustomFreedomObjectDataset.ExtractMapperFrom(pClass: TClass; pObject: TObject): TObjectMapper;
var
  lParams: TObjectToMapperParams;
begin
  lParams := TObjectToMapperParams.Create;
  if (not Assigned(pObject)) then
  begin
    lParams.MetaClassType := pClass;
  end
  else
  begin
    lParams.ObjectInstance := pObject;
  end;
  lParams.Options := [Properties];
  Result := TObjectToMapper.ObjectToMapper(lParams);
end;

function TCustomFreedomObjectDataset.ExtractObjectClassTypeFromObjectList(pClassInfo: Pointer): TFreedomObjectClass;
var
  lContext: TRttiContext;
  lRttiType: TRttiType;
begin
  lContext := TRttiContext.Create;
  try
    lRttiType := lContext.GetType(pClassInfo);
    Result := GetObjectClassTypeFromObjectList(lContext, lRttiType);
  finally
    lContext.Free;
  end;
end;

function TCustomFreedomObjectDataset.GetActiveRecBuf: TRecBuf;
begin
  case State of
    dsBlockRead, dsBrowse:
      if IsEmpty then
        Result := NullBuffer
      else
        Result := ActiveBuffer;
    dsCalcFields, dsInternalCalc:
      Result := CalcBuffer;
    dsFilter:
      Result := FFilterBuffer;
    dsOldValue:
      if FOldValueBuffer <> NullBuffer then
        Result := FOldValueBuffer
      else
        Result := ActiveBuffer;
    dsEdit, dsInsert, dsNewValue:
      Result := ActiveBuffer;
  else
    Result := NullBuffer;
  end;
end;

function TCustomFreedomObjectDataset.GetBlobData(Field: TField): TBytes;
var
  lData: ValueType;
begin
  if GetFieldVariant(Field, lData) then
  begin
    // if field is special cases memo or widememo, and variant is string
    // then force the encoding for the expected encoding.
    // Memo needs ANSI encoding, WideMemo needs Unicode encoding.
    if (Field.DataType in [ftMemo, ftWideMemo]) and VarIsStr(lData) then
    begin
      case Field.DataType of
        ftMemo:
          {$IFDEF DELPHIXE2_LVL}
          Result := TEncoding.ANSI.GetBytes(VarToStr(lData));
          {$ELSE}
          Result := TEncoding.Default.GetBytes(VarToStr(lData));
          {$ENDIF}
        ftWideMemo:
          Result := TEncoding.Unicode.GetBytes(VarToStr(lData));
      end;
    end
    else
      Result := VariantToBytes(lData);
  end
  else
    SetLength(Result, 0);
end;

procedure TCustomFreedomObjectDataset.GetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  PObject(Data)^ := GetRecordInfo(Buffer)^.ObjectInfo;
end;

function TCustomFreedomObjectDataset.GetBookmarkFlag(Buffer: TRecBuf): TBookmarkFlag;
begin
  Result := GetRecordInfo(Buffer)^.BookmarkFlag;
end;

function TCustomFreedomObjectDataset.GetRecordInfo(Buffer: TRecBuf): PRecInfo;
begin
  Result := PRecInfo(TRecordBuffer(Buffer) + FRecInfoOffset);
end;

function TCustomFreedomObjectDataset.GetBufferValueList(pBuffer: TRecBuf): PValueList;
begin
  Result := PValueList(pBuffer);
end;

function TCustomFreedomObjectDataset.GetCanModify: boolean;
begin
  Result := True;
end;

function TCustomFreedomObjectDataset.GetCurrentRecord(Buffer: TRecBuf): Boolean;
begin
  if not IsEmpty and (GetBookmarkFlag(Buffer) = bfCurrent) then begin
    try
      Result := True;
      with GetRecordInfo(Buffer)^ do
      begin
        FCurrentRecNo := ListIndex;
        ObjectInfo := SourceList.Items[FCurrentRecNo];
      end;
      UpdateCursorPos;
    except
      Result := False;
    end;
  end
  else
    Result := False;
end;

function TCustomFreedomObjectDataset.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  case FieldType of
    ftVariant: Result := TFreedomEntityField;
  else
    result := inherited GetFieldClass(FieldType);
  end;
end;

{$IFDEF DELPHIXE4_LVL}
function TCustomFreedomObjectDataset.GetFieldData(Field: TField; Buffer: TValueBuffer): boolean;
{$ELSE}
function TCustomFreedomObjectDataset.GetFieldData(Field: TField; var Buffer: TValueBuffer): boolean;
{$ENDIF}
var
  Data: ValueType;

  procedure VariantToFieldBuffer;
  var
    Str: string;
    SInt: Smallint;
    I: integer;
    I64: LargeInt;
    B: boolean;
    F: double;
    BCD: TBCD;
    D: TDateTime;
    TempValue: TValueBuffer;
    {$IFDEF NEXTGEN}
    M: TMarshaller;
    {$ENDIF}
  begin
    case Field.DataType of
      ftString, ftFixedChar:
        begin
        {$IFNDEF NEXTGEN}
          {$IFDEF DELPHIXE3_LVL}
          FillChar(Buffer^, Field.DataSize, 0);
          {$ELSE}
          FillChar(Buffer[0], Field.DataSize, 0);
          {$ENDIF}
          Str := VarToStr(Data);
          System.AnsiStrings.StrLCopy(PAnsiChar(Buffer), PAnsiChar(AnsiString(Str)), Field.Size);
        {$ELSE}
          FillChar(Buffer[0], Field.DataSize, 0);
          Str := VarToStr(Data);
          TMarshal.Copy(M.AsAnsi(Str), Buffer, 0, Field.DataSize);
        {$ENDIF}
        end;
      ftWideString, ftFixedWideChar:
        begin
          FillChar(Buffer[0], Field.DataSize, 0);
          Str := VarToStr(Data);
          StrLCopy(PChar(Buffer), PChar(Str), Field.Size);
        end;
      ftSmallint:
        begin
          SInt := Data;
          TBitConverter.FromSmallInt(SInt, Buffer);
        end;
      ftInteger:
        begin
          I := Data;
          TBitConverter.FromInteger(I, Buffer);
        end;
      ftLargeint:
        begin
          I64 := Data;
          TBitConverter.FromLargeInt(I64, Buffer);
        end;
      ftBoolean:
        begin
          B := Data;
          TBitConverter.FromWordBool(B, Buffer);
        end;
      ftFloat, ftCurrency:
        begin
          F := Data;
          TBitConverter.FromDouble(F, Buffer);
        end;
      ftFmtBCD:
        begin
          BCD := VarToBcd(Data);
          TBitConverter.FromBcd(BCD, Buffer);
        end;
      ftDate, ftTime, ftDateTime:
        begin
          D := VarToDateTime(Data);
          if True then
          begin
            SetLength(TempValue, SizeOf(double));
            TBitConverter.FromDouble(D, TempValue);
            DataConvert(Field, TempValue, Buffer, True);
          end
          else
            TBitConverter.FromDouble(D, Buffer);
        end;
      ftBlob, ftMemo, ftWideMemo, ftGraphic:
        begin
           // Not needed, blob stream uses GetBlobData directly
//          TempBytes := TUtils.VariantToBytes(Data);
//          System.Move(TempBytes[0], Buffer^, Length(TempBytes));
//          Variant(Buffer^) := Data;
        end;
      ftVariant:
        begin
          Variant(PVariant(@Buffer[0])^) := Data;
        end;
      ftGuid:
        begin
        {$IFNDEF NEXTGEN}
          FillChar(Buffer[0], Field.DataSize, 0);
          Str := VarToStr(Data);
          System.AnsiStrings.StrLCopy(PAnsiChar(Buffer), PAnsiChar(AnsiString(Str)), Field.Size);
        {$ELSE}
          FillChar(Buffer[0], Field.DataSize * SizeOf(Char), 0);
          Str := VarToStr(Data);
          TMarshal.Copy(M.AsAnsi(Str), Buffer, 0, Field.DataSize);
        {$ENDIF}
        end
    else

      // ftWord,ftBytes,ftVarBytes,ftAutoInc,ftGraphic,ftFmtMemo,ftParadoxOle,ftDBaseOle,ftTypedBinary,
      // ftCursor,ftADT,ftArray,ftReference,ftDataSet,ftOraBlob,ftOraClob,ftVariant,ftInterface,ftIDispatch,
      // ftGuid,ftTimeStamp,ftFMTBcd,ftOraTimeStamp,ftOraInterval,ftLongWord,ftShortint,ftByte,ftExtended,
      // ftConnection,ftParams,ftStream,ftTimeStampOffset,ftObject,ftSingle,
    end;
  end;

  procedure ValueToFieldBuffer;
  begin
    VariantToFieldBuffer;
  end;

begin
  Result := GetFieldVariant(Field, Data);
  if Length(Buffer) > 0 then
  begin
    if Result and (Buffer <> nil) then
    begin
      ValueToFieldBuffer;
    end;
  end;
end;

function TCustomFreedomObjectDataset.GetFieldVariant(Field: TField; var Data: ValueType): boolean;
var
  RecBuf: TRecBuf;
  Obj: TObject;
begin
  RecBuf := GetActiveRecBuf;
  if RecBuf = NullBuffer then
    Exit(false);
  Data := GetBufferValueList(RecBuf)[Field.Index];
  if VarIsEmpty(Data) then
  begin
    Obj := GetRecordInfo(RecBuf)^.ObjectInfo;
    if Field.FieldKind = fkData then
      Data := GetPropValue(Field.FieldName, Obj);
    if VarIsEmpty(Data) then
      Data := Null;
    GetBufferValueList(RecBuf)[Field.Index] := Data;
  end;
  Result := not VarIsNull(Data);
end;

function TCustomFreedomObjectDataset.GetGroupFilterCriteria: TGroupFilterCriteria;
begin
  Result := SourceList.Filter;
end;

function TCustomFreedomObjectDataset.GetObjectClassTypeFromObjectList(pContext: TRttiContext; pRttiType: TRttiType): TFreedomObjectClass;
var
  lTypeName: string;
begin
  Result := nil;
  repeat
    lTypeName := pRttiType.ToString;
    if System.StrUtils.StartsText('TFreedomObjectList<', lTypeName) and EndsText('>', lTypeName) then
    begin
      lTypeName := AnsiRightStr(lTypeName, Length(lTypeName) - Length('TFreedomObjectList<'));
      lTypeName := AnsiLeftStr(lTypeName, Length(lTypeName) - Length('>'));
      Result := TFreedomObjectClass(pContext.FindType(lTypeName).AsInstance.MetaclassType);
      pRttiType := nil;
    end
    else
    begin
      pRttiType := pRttiType.AsInstance.BaseType;
    end;
  until pRttiType = nil;
end;

function TCustomFreedomObjectDataset.GetPropValue(pPropName: string; pObject: TObject): ValueType;
var
  lValue: TValue;
  lRttiColumnOptions: TRttiColumnOptions;
  lProperty, lSubProperty: string;
  lObjectMapper: TObjectMapper;
  lColumn: TCustomColumnMapper;
  lStrings: TStrings;
begin
  Result := Null;
  lRttiColumnOptions := nil;
  if pObject <> nil then
  begin
    lObjectMapper := ExtractMapperFrom(pObject.ClassType, pObject);
    try
      SplitProp(pPropName, lProperty, lSubProperty);
      lColumn := lObjectMapper.Columns.FindColumn(lProperty);
      if (Assigned(lColumn)) then
      begin
        lRttiColumnOptions := lColumn.RttiOptions;
      end;
      if lRttiColumnOptions <> nil then
      begin
        lValue := lRttiColumnOptions.RttiProperty.GetValue(pObject);
        if lRttiColumnOptions.RttiProperty.PropertyType.IsInstance then
        begin
          if lSubProperty = '' then
          begin
            if lRttiColumnOptions.RttiProperty.PropertyType.AsInstance.MetaclassType.InheritsFrom(TStrings) then
            begin
              lStrings := TStrings(lValue.AsObject);
              if (Assigned(lStrings)) and (lStrings.Text <> '') then
              begin
                Result := lStrings.Text;
              end;
            end
            else if (lColumn.IsNullable) then
            begin
              Result := lColumn.CurrentValue;
            end
            else
            begin
              Result := ObjectToVariant(lValue.AsObject);
            end;
          end
          else
          begin
            Result := GetPropValue(lSubProperty, lValue.AsObject);
          end;
        end
        else
        begin
          if lSubProperty = '' then
          begin
            if IsEnumeration(lValue.TypeInfo) then
            begin
              if (not lValue.IsEmpty) then
              begin
                Result := lValue.AsOrdinal
              end;
            end
            else
            begin
              Result := ValueToVariant(lValue, lRttiColumnOptions.RttiProperty.PropertyType.Handle)
            end;
          end;
        end;
      end;
    finally
      FinalizeMapperFrom(lObjectMapper.GetHashCode);
    end;
  end;
end;

function TCustomFreedomObjectDataset.GetRecNo: Integer;
var
  RecBuf: TRecBuf;
begin
  CheckActive;
  Result := -1;
  RecBuf := GetActiveRecBuf;
  if (RecBuf <> NullBuffer) and (GetRecordInfo(RecBuf)^.BookmarkFlag = bfCurrent) then
    Result := GetRecordInfo(RecBuf)^.ListIndex + 1;
end;

function TCustomFreedomObjectDataset.InternalGetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: boolean): TGetResult;
begin
  Result := grOK;
  try
    case GetMode of
      gmNext:
        if (FCurrentRecNo + 1) >= ListRecordCount then
        begin
          FCurrentRecNo := ListRecordCount
        end
        else
        begin
          Inc(FCurrentRecNo);
        end;
      gmPrior:
        if FCurrentRecNo >= 0 then
        begin
          Dec(FCurrentRecNo);
        end;
    end;
    if FCurrentRecNo >= ListRecordCount then
    begin
      Result := grEOF;
      FCurrentRecNo := ListRecordCount;
    end
    else if FCurrentRecNo < 0 then
    begin
      Result := grBOF;
      FCurrentRecNo := -1;
    end
    else if (Buffer <> NullBuffer) then
    begin
      with GetRecordInfo(Buffer)^ do
      begin
        ListIndex := FCurrentRecNo;
        ObjectInfo := SourceList.Items[FCurrentRecNo];
        BookmarkFlag := bfCurrent;
      end;
      Finalize(GetBufferValueList(Buffer)^, Fields.Count);
      GetCalcFields(Buffer);
    end;
  except
    Result := grError;
    if not DoCheck then
      raise;
  end;
end;

function TCustomFreedomObjectDataset.GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: boolean): TGetResult;
begin
  Result := InternalGetRecord(Buffer, GetMode, DoCheck)
end;

function TCustomFreedomObjectDataset.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := GetRecord(TRecBuf(Buffer), GetMode, DoCheck);
end;

function TCustomFreedomObjectDataset.GetRecordCount: integer;
begin
  Result := ListRecordCount;
end;

function TCustomFreedomObjectDataset.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

{$IFDEF DELPHIXE3_LVL}
procedure TCustomFreedomObjectDataset.GetTheFieldList(List: TList<TField>;
  const FieldNames: string);
begin
  GetFieldList(List, FieldNames);
end;
{$ELSE}
procedure TCustomFreedomObjectDataset.GetTheFieldList(pListFields: TList<TField>; const FieldNames: string);
var
  lListFields: TList<TField>;
  I: integer;
begin
  lListFields := TList<TField>.Create;
  try
    GetFieldList(lListFields, FieldNames);
    pListFields.Clear;
    for I := 0 to lListFields.Count - 1 do
      pListFields.Add(lListFields.Items[I]);
  finally
    lListFields.Free;
  end;
end;
function TCustomFreedomObjectDataset.GuidToVariant(pGuid: TGuid): Variant;
begin
  if IsEqualGUID(EMPTY_GUID, pGuid) then
    Result := Null
  else
  begin
    Result := GuidToString(pGuid);
  end;
end;

{$ENDIF}

function TCustomFreedomObjectDataset.InIgnoreList(pIgnoreFields: array of String; pFieldName: String): Boolean;
begin
  Result := IndexText(pFieldName, pIgnoreFields) >= 0;
end;

procedure TCustomFreedomObjectDataset.InternalClose;
begin
  FIsOpen := False;
  CloseChilds;
  ClearSourceList;
  FCurrentRecNo := -1;
end;

procedure TCustomFreedomObjectDataset.InternalDelete;
var
  lRecBuf: TRecBuf;
  lObject: TObject;
  lIndex: integer;
begin
  lRecBuf := GetActiveRecBuf;
  lObject := GetRecordInfo(lRecBuf)^.ObjectInfo;
  lIndex := SourceList.IndexOf(TFreedomObject(lObject));

  if lIndex > -1 then
  begin
    SourceList.Delete(TFreedomObject(lObject));
    if FCurrentRecNo > ListRecordCount then
    begin
      FCurrentRecNo := ListRecordCount - 1;
    end;
  end;
  RefreshDataSetFields;
end;

procedure TCustomFreedomObjectDataset.InternalEdit;
begin
  FModifiedFields.Clear;
  inherited;
end;

procedure TCustomFreedomObjectDataset.InternalFirst;
begin
  FCurrentRecNo := -1;
end;

procedure TCustomFreedomObjectDataset.InternalGotoBookmark(Bookmark: TBookmark);
var
  lIndex: integer;
begin
  lIndex := SourceList.IndexOf(TFreedomObject(PObject(Bookmark)^));
  if lIndex <> -1 then
  begin
    FCurrentRecNo := lIndex;
  end
  else
  begin
    DatabaseError('Bookmark not found');
  end;
end;

procedure TCustomFreedomObjectDataset.InternalHandleException;
begin
  ApplicationHandleException(Self);
end;

procedure TCustomFreedomObjectDataset.InternalInitFieldDefs;
begin
  //do nothing;
end;

procedure TCustomFreedomObjectDataset.InternalInitRecord(Buffer: TRecordBuffer);
var
  lIndex: integer;
begin
  for lIndex := 0 to Fields.Count - 1 do
    GetBufferValueList(NativeInt(Buffer))[lIndex] := Null;
end;

procedure TCustomFreedomObjectDataset.InternalLast;
begin
  FCurrentRecNo := ListRecordCount;
end;

function TCustomFreedomObjectDataset.InternalList<T>: TFreedomObjectList<T>;
begin
  Result := TFreedomObjectList<T>(SourceList);
end;

procedure TCustomFreedomObjectDataset.InternalOpen;
begin
  if DataSetField <> nil then
  begin
    if (TCustomFreedomObjectDataset(DataSetField.DataSet).FChildsEnabled) then
    begin
      UpdateListFromParent(DataSetField);
    end;
  end
  else
  begin
    if FObjectClass = nil then
    begin
      DoInvalidObjectClass;
    end;
    if (SourceList = nil) then
    begin
      FSourceList := TFreedomObjectList<TFreedomObject>.Create;
      FIsSelfSource := True;
    end
    else
    begin
      SourceList.Empty;
    end;
    if (Assigned(FInternalCriteria)) then
    begin
      SourceList.GetObjects(FInternalCriteria, TFreedomObjectClass(FObjectClass));
    end
    else
    begin
      SourceList.GetAllObjects(TFreedomObjectClass(FObjectClass));
    end;
  end;
  if (SourceList = nil) and (DataSetField = nil) then
  begin
    raise EInvalidSourceList.Create(Self.Name, '');
  end;
  FCurrentRecNo := -1;
  FRecordSize := Fields.Count * SizeOf(ValueType);
  FBufferSize := FRecordSize + SizeOf(TRecInfo);
  FRecInfoOffset := FRecordSize;
  InternalInitFieldDefs;
  CreateFields;
  FIsOpen := True;
  BindFields(True);
end;

procedure TCustomFreedomObjectDataset.InternalPost;
var
  RecBuf: TRecBuf;

  procedure UpdateObjectFields(Obj: TObject);
  var
    Field: TField;
    Value: ValueType;
  begin
    for Field in FModifiedFields do
    begin
      if Field.FieldKind = fkData then
      begin
        Value := GetBufferValueList(RecBuf)[Field.Index];
        SetPropValue(Field.FieldName, Obj, Value);
      end;
    end;
  end;

var
  lObject: TObject;
  IndexToInsert: integer;
begin
  inherited;
  UpdateCursorPos;
  RecBuf := GetActiveRecBuf;
  if State = dsEdit then
  begin
    lObject := GetRecordInfo(RecBuf)^.ObjectInfo;
    UpdateObjectFields(lObject);
  end
  else
  begin
    case GetRecordInfo(RecBuf)^.BookmarkFlag of
      bfBOF:
        IndexToInsert := 0;
      bfEOF:
        IndexToInsert := -1;
      else
        IndexToInsert := SourceList.IndexOf(TFreedomObject(GetRecordInfo(RecBuf)^.ObjectInfo));
    end;
    lObject := CreateObject;
    UpdateObjectFields(lObject);
    if IndexToInsert = -1 then
    begin
      SourceList.Insert(TFreedomObject(lObject));
    end
    else
      SourceList.Insert(IndexToInsert, TFreedomObject(lObject));
  end;
  FBookmarkToClonedObject := nil;
end;

procedure TCustomFreedomObjectDataset.InternalSetSourceList(pSourceList: TFreedomObjectList<TFreedomObject>);
var
  lIndex: Integer;
  lObject: TFreedomObject;
begin
  if (FIsSelfSource) then
  begin
    FreeAndNil(FSourceList);
  end;
  if pSourceList = nil then
  begin
    FSourceList := nil;
  end
  else
  begin
    FSourceList := pSourceList;
    FIsSelfSource := False;
    if Assigned(FInternalList) then
    begin
      if (FInternalList.Count > 0) then
      begin
        for lIndex := FInternalList.Count - 1 downto 0 do
        begin
          lObject := FInternalList.Items[lIndex];
          FInternalList.Extract(lObject);
          FSourceList.Insert(lObject);
        end;
      end;
      FreeAndNil(FInternalList);
    end;
  end;
end;

procedure TCustomFreedomObjectDataset.InternalSetToRecord(Buffer: TRecBuf);
var
  BookmarkBuffer: TBookmark;
begin
  SetLength(BookmarkBuffer, BookmarkSize);
  GetBookmarkData(Buffer, BookmarkBuffer);
  InternalGotoBookmark(BookmarkBuffer);
end;

function TCustomFreedomObjectDataset.IsCursorOpen: boolean;
begin
  Result := FIsOpen;
end;

function TCustomFreedomObjectDataset.IsDynamicArray(pTypInfo: PTypeInfo): Boolean;
begin
  Result := (pTypInfo^.Kind = tkDynArray);
end;

function TCustomFreedomObjectDataset.IsEnumeration(ATypeInfo: PTypeInfo): boolean;
begin
  Result:= (ATypeInfo <> nil) and (ATypeInfo.Kind = tkEnumeration) and (ATypeInfo <> TypeInfo(Boolean));
end;

function TCustomFreedomObjectDataset.IsNotEmpty: Boolean;
begin
  Result := not IsEmpty;
end;

function TCustomFreedomObjectDataset.ListRecordCount: integer;
begin
  Result := SourceList.Count;
  if (Result = 0) then
  begin
    Result := -1;
  end;
end;

function TCustomFreedomObjectDataset.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): boolean;
begin
  DoBeforeScroll;
  Result := LocateRecord(KeyFields, KeyValues, Options, True, '');
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TCustomFreedomObjectDataset.LocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions;
  SyncCursor: Boolean; ResultFields: string): Boolean;
var
  FieldList: TList<TField>;

  function MatchField(Field: TField; Value: Variant; Options: TLocateOptions): Boolean;
  var
    FieldValue: string;
  begin
    case Field.DataType of
      ftString, ftFixedChar, ftWideString, ftFixedWideChar:
        begin
          FieldValue := VarToStr(Field.Value);
          if loPartialKey in Options then
            FieldValue := Copy(FieldValue, 1, Length(Value));
          if loCaseInsensitive in Options then
            Result := SameText(VarToStr(Value), FieldValue)
          else
            Result := SameStr(VarToStr(Value), FieldValue);
        end;
    else
      Result := (Field.Value = Value);
    end;
  end;

  function MatchRecord: boolean;
  var
    I: integer;
  begin
    Result := False;
    for I := 0 to FieldList.Count - 1 do
    begin
      if FieldList.Count = 1 then
        Result := MatchField(FieldList[I], KeyValues, Options)
      else
        Result := MatchField(FieldList[I], KeyValues[I], Options);
      if not Result then
        Break;
    end;
  end;

var
  lOldIndex: Integer;
  lRecBuffer: TRecBuf;
  lResultFieldList: TList<TField>;
  lIndex: integer;
  lNullValueBuffer: TValueBuffer;
begin
  CheckBrowseMode;
  CursorPosChanged;
  Result := False;
  FieldList := TList<TField>.Create;
  try
    GetTheFieldList(FieldList, KeyFields);
    lOldIndex := FCurrentRecNo;
    SetTempState(dsFilter);
    lRecBuffer := TempBuffer;
    FFilterBuffer := lRecBuffer;
    try
      InternalFirst;
      while GetRecord(lRecBuffer, gmNext, True) = grOK do
        if MatchRecord then
        begin
          Result := True;
          Break;
        end;


      if Result and not SyncCursor then
      begin
        lResultFieldList := TList<TField>.Create;
        try
          GetTheFieldList(lResultFieldList, ResultFields);
          for lIndex := 0 to lResultFieldList.Count - 1 do
          begin
              SetLength(lNullValueBuffer, 0);
              GetFieldData(lResultFieldList[lIndex], lNullValueBuffer);
          end;
        finally
          lResultFieldList.Free;
        end;
      end;
      if not (Result and SyncCursor) then
        FCurrentRecNo := lOldIndex;
    finally
      RestoreState(dsBrowse);
    end;
  finally
    FieldList.Free;
  end;
end;

function TCustomFreedomObjectDataset.Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
begin
  VarClear(Result);
  if LocateRecord(KeyFields, KeyValues, [], False, ResultFields) then
  begin
    SetTempState(dsFilter);
    try
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

class function TCustomFreedomObjectDataset.ObjectToVariant(Obj: TObject): Variant;
begin
  if (Assigned(Obj)) then
  begin
    {$IFDEF DELPHIXE2_LVL}
    Result := IntPtr(Obj);
    {$ELSE}
    Result := Integer(Obj);
    {$ENDIF}
  end
  else begin
    Result := Null;
  end;
end;

procedure TCustomFreedomObjectDataset.Open(pGroupCriteria: TGroupCriteria);
begin
  if (Active) then
  begin
    Close;
  end;
  if (FInternalCriteria <> pGroupCriteria) then
  begin
    FreeAndNil(FInternalCriteria);
  end;
  FInternalCriteria := pGroupCriteria;
  if (Assigned(FInternalCriteria)) then
  begin
    FInternalCriteria.CannotBeDestroyed := True;
  end;
  inherited Open;
end;

procedure TCustomFreedomObjectDataset.OpenChilds;
var
  lIndex: Integer;
  lDataSet: TCustomFreedomObjectDataset;
  lMapper: TObjectMapper;
  lColumn: TCustomColumnMapper;
begin
  if FChildsEnabled and Assigned(NestedDataSets) and (NestedDataSets.Count > 0) then
  begin
    lMapper := ExtractMapperFrom(FObjectClass, nil);
    try
      for lIndex := 0 to NestedDataSets.Count - 1 do
      begin
        if NestedDatasets.Items[lIndex] is TCustomFreedomObjectDataset then
        begin
          lDataSet := TCustomFreedomObjectDataset(NestedDataSets.Items[lIndex]);
          if lDataSet.Active then
          begin
            if (lDataSet.ObjectClass = nil) and (FObjectClass <> nil) then
            begin
              lColumn := lMapper.Columns.FindColumn(lDataSet.DatasetField.FieldName);
              if Supports(lColumn.RttiOptions.RttiProperty.PropertyType.AsInstance.MetaclassType, IFreedomObjectList) then
              begin
                lDataSet.ObjectClass := ExtractObjectClassTypeFromObjectList(lColumn.RttiOptions.RttiProperty.PropertyType.AsInstance.MetaclassType.ClassInfo);
              end;
            end;
            lDataSet.UpdateListFromParent(lDataSet.DatasetField);
            lDataSet.Resync([]);
          end;
        end;
      end;
    finally
      FinalizeMapperFrom(lMapper.GetHashCode);
    end;
  end;
end;

procedure TCustomFreedomObjectDataset.Reopen;
begin
  Close;
  Open(FInternalCriteria);
end;

procedure TCustomFreedomObjectDataset.SetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  with GetRecordInfo(Buffer)^ do
  begin
    ObjectInfo := PObject(Data)^;
    ListIndex := SourceList.IndexOf(TFreedomObject(ObjectInfo));
  end;
end;

procedure TCustomFreedomObjectDataset.SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag);
begin
  GetRecordInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TCustomFreedomObjectDataset.SetChildsDisabled;
begin
  if (FChildsEnabled) then
  begin
    FChildsEnabled := False;
    CloseChilds;
  end;
end;

procedure TCustomFreedomObjectDataset.SetChildsEnabled;
begin
  if (not FChildsEnabled) then
  begin
    FChildsEnabled := True;
    OpenChilds;
  end;
end;

procedure TCustomFreedomObjectDataset.SetDataSetField(const Value: TDataSetField);
begin
  if Assigned(Value) then
    UpdateListFromParent(Value);
  inherited SetDataSetField(Value);
end;

procedure TCustomFreedomObjectDataset.SetFieldData(Field: TField; Buffer: TValueBuffer);
begin
  SetFieldData(Field, Buffer, True);
end;

procedure TCustomFreedomObjectDataset.SetFieldData(Field: TField; Buffer: TValueBuffer; NativeFormat: boolean);

  function FieldBufferToVariant(Buffer: TValueBuffer): Variant;
  var
    SInt: Smallint;
    I: integer;
    I64: LargeInt;
    B: boolean;
    F: double;
    BCD: TBCD;
    TempValue: TValueBuffer;
    {$IFDEF NEXTGEN}
    NullIndex: integer;
    Str: string;
    {$ENDIF}
  begin
    case Field.DataType of
      ftString, ftFixedChar:
        begin
          {$IFNDEF NEXTGEN}
          Result := AnsiString(PAnsiChar(Buffer));
          {$ELSE}
          Str := TEncoding.ANSI.GetString(Buffer);
          NullIndex := Str.IndexOf(#0);
          if NullIndex >= 0 then
            Result := Str.Remove(NullIndex)
          else
            Result := Str;
          {$ENDIF}
        end;
      ftWideString, ftFixedWideChar:
        begin
          {$IFNDEF NEXTGEN}
          Result := WideString(PWideChar(Buffer));
          {$ELSE}
          Result := string(PChar(Buffer));
          {$ENDIF}
        end;
      ftSmallint:
        begin
          SInt := TBitConverter.ToSmallInt(Buffer);
          Result := SInt;
        end;
      ftInteger:
        begin
          I := TBitConverter.ToInteger(Buffer);
          Result := I;
        end;
      ftLargeint:
        begin
          I64 := TBitConverter.ToLargeInt(Buffer);
          Result := I64;
        end;
      ftBoolean:
        begin
          B := TBitConverter.ToWordBool(Buffer);
          Result := B;
        end;
      ftFloat, ftCurrency:
        begin
          F := TBitConverter.ToDouble(Buffer);
          Result := F;
        end;
      ftFmtBCD:
        begin
          BCD := TBitConverter.ToBcd(Buffer);
          VarFMTBcdCreate(Result, BCD);
        end;
      ftDate, ftTime, ftDateTime:
        begin
          if NativeFormat then
          begin
            SetLength(TempValue, SizeOf(TVarData(Result).VDate));
            DataConvert(Field, Buffer, TempValue, False);
            TVarData(Result).VDate := TBitConverter.ToDouble(TempValue);
          end else
            Result := TDateTime(TBitConverter.ToDouble(Buffer));
        end;
      ftBlob, ftGraphic:
        begin
          Result := BytesToVariant(TBytes(Buffer));
        end;
      ftMemo:
        begin
          {$IFNDEF NEXTGEN}
          Result := AnsiString(StringOf(TBytes(Buffer)));
          {$ELSE}
          Str := TEncoding.ANSI.GetString(Buffer);
          NullIndex := Str.IndexOf(#0);
          if NullIndex >= 0 then
            Result := Str.Remove(NullIndex)
          else
            Result := Str;
          {$ENDIF}
        end;
      ftWideMemo:
        begin
          Result := WideStringOf(TBytes(Buffer));
        end;
      ftVariant:
        begin
          Result := Variant(PVariant(@Buffer[0])^);
        end;
      ftGuid:
        begin
          {$IFNDEF NEXTGEN}
          Result := AnsiString(PAnsiChar(Buffer));
          {$ELSE}
          Str := TEncoding.ANSI.GetString(Buffer);
          NullIndex := Str.IndexOf(#0);
          if NullIndex >= 0 then
            Result := Str.Remove(NullIndex)
          else
            Result := Str;
          {$ENDIF}
        end;
    end;
  end;

  function FieldBufferToValue(Buffer: TValueBuffer): Variant;
  begin
    Result := FieldBufferToVariant(Buffer);
  end;

var
  RecBuf: TRecBuf;
  Data: ValueType;
begin
  if not(State in dsWriteModes) then
    DatabaseError(SNotEditing, Self);

  RecBuf := GetActiveRecBuf;
  if RecBuf = NullBuffer then
    Exit;

  if Field.ReadOnly and not(State in [dsSetKey, dsFilter]) then
    DatabaseErrorFmt(SFieldReadOnly, [Field.DisplayName]);
  Field.Validate(Buffer);
  if FModifiedFields.IndexOf(Field) = -1 then
    FModifiedFields.Add(Field);

  if Buffer = nil then
    Data := Null
  else
    Data := FieldBufferToValue(Buffer);
  GetBufferValueList(RecBuf)[Field.Index] := Data;
  if not(State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, IntPtr(Field));
end;

procedure TCustomFreedomObjectDataset.SetObjectClass(const Value: TFreedomObjectClass);
begin
  if Assigned(Value) and (not Value.InheritsFrom(TFreedomObject)) then
  begin
    if (Assigned(Owner)) then
    begin
      raise EInvalidObjectClass.Create(Self.Name, Self.Owner.Name);
    end
    else
    begin
      raise EInvalidObjectClass.Create(Self.Name, '');
    end;
  end;
  if FObjectClass <> Value then
  begin
    FObjectClass := Value;
    SetChildsObjectClass;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TCustomFreedomObjectDataset.SetPropValue(pPropName: string; pObject: TObject; pValue: ValueType);
var
  lPropValue: TValue;
  lRttiColumnOptions: TRttiColumnOptions;
  lProperty, lSubProperty: string;
  lObjectMapper: TObjectMapper;
  lColumn: TCustomColumnMapper;
  lStrings: TStrings;
  lObject: TObject;
  lNullable: INullable;
begin
  SplitProp(pPropName, lProperty, lSubProperty);
  lRttiColumnOptions := nil;
  lObjectMapper := ExtractMapperFrom(pObject.ClassType, pObject);
  try
    lColumn := lObjectMapper.Columns.FindColumn(lProperty);
    if (Assigned(lColumn)) then
    begin
      lRttiColumnOptions := lColumn.RttiOptions;
    end;
    if (Assigned(lRttiColumnOptions)) then
    begin
      if lSubProperty = '' then
      begin
        if lRttiColumnOptions.RttiProperty.PropertyType.IsInstance then
        begin
          if lRttiColumnOptions.RttiProperty.PropertyType.AsInstance.MetaClassType.InheritsFrom(TStrings) then
          begin
            lPropValue := lRttiColumnOptions.RttiProperty.GetValue(pObject);
            lStrings := TStrings(lPropValue.AsObject);
            if (Assigned(lStrings)) then
            begin
              lStrings.Text := VarToStr(pValue);
            end;
          end
//          else if lRttiColumnOptions.RttiProperty.PropertyType.AsInstance.MetaClassType.InheritsFrom(TStream) then
//          begin
//            lStream := lRttiColumnOptions.RttiProperty.GetValue(pObject).AsObject;
//            lStream.Text := pValue;
//          end
          else if (lColumn.IsNullable) then
          begin
            lObject := lRttiColumnOptions.RttiProperty.GetValue(pObject).AsObject;
            Supports(lObject, INullable, lNullable);
            lNullable.SetInternalValue(pValue);
          end;
        end
        else
        begin
          if IsEnumeration(lRttiColumnOptions.RttiProperty.PropertyType.Handle) then
            lPropValue := TValue.FromOrdinal(lRttiColumnOptions.RttiProperty.PropertyType.Handle, pValue)
          else
            lPropValue := VariantToValue(pValue, lRttiColumnOptions.RttiProperty.PropertyType.Handle);
          lRttiColumnOptions.RttiProperty.SetValue(pObject, lPropValue);
        end;
      end
      else
      begin
        if lRttiColumnOptions.RttiProperty.PropertyType.IsInstance then
        begin
          lPropValue := lRttiColumnOptions.RttiProperty.GetValue(pObject);
          SetPropValue(lSubProperty, lPropValue.AsObject, pValue);
        end else
        begin
          if IsEnumeration(lRttiColumnOptions.RttiProperty.PropertyType.Handle) then
          begin
            lPropValue := TValue.FromOrdinal(lRttiColumnOptions.RttiProperty.PropertyType.Handle, GetEnumValue(lRttiColumnOptions.RttiProperty.PropertyType.Handle, pValue));
            lRttiColumnOptions.RttiProperty.SetValue(pObject, lPropValue);
          end
          else
          begin
            raise Exception.Create('Cant set value for property ' + lRttiColumnOptions.RttiProperty.PropertyType.Name);
          end;
        end;
      end;
    end;
  finally
    FinalizeMapperFrom(lObjectMapper.GetHashCode);
  end;
end;

procedure TCustomFreedomObjectDataset.SetRecNo(Value: integer);
begin
  CheckBrowseMode;
  Value := Value - 1;
  if Value < 0 then
  begin
    Value := 0;
  end;
  if Value > ListRecordCount then
  begin
    Value := ListRecordCount - 1;
  end;
  if Value < -1 then
  begin
    Value := -1;
  end;
  DoBeforeScroll;
  FCurrentRecNo := Value;
  Resync([rmCenter]);
  DoAfterScroll;
end;

procedure TCustomFreedomObjectDataset.SetSourceList(pSourceList: TFreedomObjectList<TFreedomObject>);
begin
  CheckInactive;
  InternalSetSourceList(pSourceList);
end;

function TCustomFreedomObjectDataset.SourceList: TFreedomObjectList<TFreedomObject>;
begin
  if (Assigned(FSourceList)) then
  begin
    Result := FSourceList;
  end
  else
  begin
    if (not Assigned(FInternalList)) then
    begin
      FInternalList := TFreedomObjectList<TFreedomObject>.Create;
    end;
    Result := FInternalList;
  end;
end;

procedure TCustomFreedomObjectDataset.SplitProp(const pText: string; var pProp, pSubProp: string);
var
  p: integer;
begin
  p := Pos('.', pText);
  if p = 0 then
  begin
    pProp := pText;
    pSubProp := '';
  end else
  begin
    pProp := Copy(pText, 1, p - 1);
    pSubProp := Copy(pText, p + 1, MaxInt);
  end;
end;

procedure TCustomFreedomObjectDataset.UpdateListFromParent(Field: TDatasetField);
var
  lObjectList: TFreedomObjectList<TFreedomObject>;
  lParentDataSet: TCustomFreedomObjectDataset;
  lMasterObject: TObject;
  lFreedomObjectList: IFreedomObjectList;
begin
  if Field <> nil then
  begin
    lParentDataSet := TCustomFreedomObjectDataset(Field.DataSet);
    lMasterObject := lParentDataSet.Current<TFreedomObject>;
    if lMasterObject <> nil then
    begin
      lObjectList := TFreedomObjectList<TFreedomObject>(VariantToObject(GetPropValue(Field.FieldName, lMasterObject)));
    end
    else
    begin
      lObjectList := nil;
    end;

    if lObjectList <> nil then
    begin
      InternalSetSourceList(lObjectList);
      if (Supports(lObjectList, IFreedomObjectList, lFreedomObjectList)) then
      begin
        FObjectClass := ExtractObjectClassTypeFromObjectList(lObjectList.ClassInfo);
        FObjectClassName := FObjectClass.ClassName;
      end;
    end;
  end;
end;

function TCustomFreedomObjectDataset.ValueToVariant(pValue: TValue; pTypeInfo: PTypeInfo): Variant;
var
  lBytes: TArray<byte>;
//  blobBytes: TBytes;
//  Blob: TBlob;
begin
  Result := Variants.Null;
  if IsDynamicArray(pTypeInfo) then
  begin
    if not pValue.IsEmpty then
    begin
      lBytes := pValue.AsType<TArray<byte>>;
      Result := BytesToVariant(TBytes(lBytes));
    end;
  end else
    Result := ValueToVariant(pValue);
end;

function TCustomFreedomObjectDataset.ValueToVariant(pValue: TValue): Variant;
var
  lDateAux: TDateTime;
begin
  Result := Variants.Null;
  if not pValue.IsEmpty then
  begin
    if pValue.TypeInfo = TypeInfo(Boolean) then
      Result := pValue.AsBoolean
    else
    if (pValue.TypeInfo = TypeInfo(TDateTime))
      or (pValue.TypeInfo = TypeInfo(TDate))
      or (pValue.TypeInfo = TypeInfo(TTime)) then
    begin
      lDateAux := TDateTime(pValue.AsType<Double>);
      if (lDateAux > 0) then
      begin
        Result := lDateAux;
      end;
    end
    else
    if (pValue.TypeInfo = TypeInfo(TBcd)) then
    begin
      Result := BcdToStr(pValue.AsType<TBcd>);
    end
    else
    if (pValue.TypeInfo = TypeInfo(TGuid)) then
    begin
      Result := GuidToVariant(pValue.AsType<TGuid>);
    end
    else
    begin
      Result := TNullableCompare.NullIfNullable(pValue.AsVariant);
    end;
  end;
end;

function TCustomFreedomObjectDataset.VariantToBytes(pValue: Variant): TBytes;
var
  lLock: Pointer;
  lHighBound: Integer;
begin
  if VarIsClear(pValue) or VarIsNull(pValue) then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  if VarIsArray(pValue) then
  begin
    lHighBound := VarArrayHighBound(pValue, 1) - VarArrayLowBound(pValue, 1) + 1;
    lLock := VarArrayLock(pValue);
    try
      SetLength(Result, lHighBound);
      System.Move(lLock^, Result[0], lHighBound);
    finally
      VarArrayUnlock(pValue);
    end;
  end
  else
  begin
    case VarType(pValue) of
      varString: Result := BytesOf(string(pValue));
      varUString: Result := BytesOf(string(pValue));
      varOleStr: Result := WideBytesOf(UnicodeString(pValue));
    else
      Result := TEncoding.Unicode.GetBytes(VarToStr(pValue));
    end;
  end;
end;

class function TCustomFreedomObjectDataset.VariantToObject(V: Variant): TObject;
begin
  if VarIsNull(V) then Exit(nil);
  {$IFDEF DELPHIXE2_LVL}
  Result := TObject(IntPtr(V));
  {$ELSE}
  Result := TObject(integer(V));
  {$ENDIF}
end;

function TCustomFreedomObjectDataset.VariantToValue(pValue: Variant): TValue;
var
  lDateTimeAux: TDateTime;
  lBcdAux: string;
  lInt64Aux: Int64;
  lIntAux: Integer;
begin
  if VarIsNull(pValue) or VarIsEmpty(pValue) then
    Exit(TValue.Empty);
  case VarTypeToDataType(VarType(pValue)) of
    ftDateTime:
      begin
        lDateTimeAux := pValue;
        TValue.Make(@lDateTimeAux, TypeInfo(TDateTime), Result);
      end;
    ftTimeStamp:
      begin
        lDateTimeAux := SQLTimeStampToDateTime(VarToSQLTimeStamp(pValue));
        TValue.Make(@lDateTimeAux, TypeInfo(TDateTime), Result);
      end;
    ftTimeStampOffset:
      begin
        lDateTimeAux := SQLTimeStampOffsetToDateTime(VarToSQLTimeStampOffset(pValue));
        TValue.Make(@lDateTimeAux, TypeInfo(TDateTime), Result);
      end;
    ftFMTBCD, ftBCD:
      begin
        // Get the BCD
        lBcdAux := BcdToStr(VarToBcd(pValue));
        if TryStrToInt(lBcdAux, lIntAux) then
          Result := TValue.From<Integer>(lIntAux)
        else if TryStrToInt64(lBcdAux, lInt64Aux) then
          Result := TValue.From<Int64>(lInt64Aux)
        else
          Result := TValue.From<Double>(StrToFloat(lBcdAux));
      end;
  else
    if ((VarType(pValue) and varArray) = varArray) and ((VarType(pValue) and varTypeMask) = varVariant) then
    begin
      // Composite id, set as variant
      TValue.Make(@pValue, TypeInfo(Variant), Result);
    end
    else
      Result := TValue.FromVariant(pValue);
  end;
end;

function TCustomFreedomObjectDataset.VariantToValue(pValue: Variant; pTypInfo: PTypeInfo): TValue;
var
  lBytes: TBytes;
//  lBlob: TBlob;
  {$IFNDEF NEXTGEN}
  lAsWide: WideString;
  {$ENDIF}
begin
  if IsDynamicArray(pTypInfo) then
  begin
    lBytes := VariantToBytes(pValue);
    TValue.Make(@lBytes, pTypInfo, Result);
  end
//  else if IsBlob(ATypInfo) then
//  begin
//    if VarIsNull(Value) or VarIsEmpty(Value) then
//      lBlob.IsNull := True
//    else
//      lBlob.AsBytes := VariantToBytes(Value);
//    Result := TValue.From<TBlob>(lBlob);
//  end
  {$IFNDEF NEXTGEN}
  else
  if pTypInfo = TypeInfo(WideString) then
  begin
    // Workaround for bug QC 91912 (Internal Tracking 282866
    // Could not set a TValue of type WideString from other string types
    // We must force the value to be widestring now
    lAsWide := VarToStr(pValue);
    TValue.Make(@lAsWide, pTypInfo, Result);
  end
  {$ENDIF}
  else
  if pTypInfo = TypeInfo(TGuid) then
  begin
    Result := TValue.From<TGuid>(VariantToGuid(pValue));
  end
  else
    Result := VariantToValue(pValue);
end;

function TCustomFreedomObjectDataset.VariantToGuid(pVariant: Variant): TGuid;
var
  lString: string;
  lBytes: TBytes;
begin
  if VarIsNull(pVariant) or VarIsEmpty(pVariant) then
  begin
    Exit(EMPTY_GUID);
  end;
  if VarIsArray(pVariant) then
  begin
    lBytes := VariantToBytes(pVariant);
    {$IFDEF DELPHIXE_LVL}
    Result := TGuid.Create(lBytes);
    {$ELSE}
    if Length(lBytes) <> 16 then
    begin
      raise EArgumentException.Create('Byte array for GUID must be exactly 16 bytes long');
    end;
    Move(lBytes[0], Result, SizeOf(Result));
    {$ENDIF}
  end
  else
  begin
    lString := VarToStr(pVariant);
    if lString <> '' then
    begin
      if Length(lString) = 36 then
        lString := '{' + lString + '}';
      Result := StringToGuid(lString);
    end
    else
      Result := EMPTY_GUID;
  end;
end;


{ TObjBlobStream }

constructor TObjBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  FField := Field;
  FFieldNo := FField.FieldNo;
  FDataSet := FField.DataSet as TCustomFreedomObjectDataset;
  if Mode <> bmRead then
  begin
    if FField.ReadOnly then
      DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName], FDataSet);
    if not(FDataSet.State in [dsEdit, dsInsert, dsNewValue]) then
      DatabaseError(SNotEditing, FDataSet);
  end;
  FBuffer := FDataSet.GetActiveRecBuf;
  if FBuffer = TCustomFreedomObjectDataset.NullBuffer then
    Exit;
  if Mode = bmWrite then
    Truncate
  else
    ReadBlobData;
end;

destructor TObjBlobStream.Destroy;
var
  LocalBytes: TBytes;
begin
  if FModified then
    try
      SetLength(LocalBytes, Size);
      Move(Self.Memory^, LocalBytes[0], Size);
      FDataset.SetFieldData(FField, LocalBytes);

//      FDataset.SetFieldData(FField, @LocalBytes[0]);

//      case FField.DataType of
//        ftMemo: data.db
//          VariantData := AnsiString(StringOf(LocalBytes));
//        ftWideMemo:
//          VariantData := WideStringOf(LocalBytes);
//      else
//        VariantData := TUtils.BytesToVariant(LocalBytes);
//      end;
//      FDataSet.SetFieldData(FField, @VariantData);

      FField.Modified := True;
      FDataSet.DataEvent(deFieldChange, IntPtr(FField));
    except
      ApplicationHandleException(Self);
    end;
  inherited;
end;

procedure TObjBlobStream.ReadBlobData;
var
  LocalBytes: TBytes;
begin
  LocalBytes := FDataSet.GetBlobData(FField);
  Clear;
  Write(LocalBytes[0], Length(LocalBytes));
  Position := 0;
  FModified := false;
end;

procedure TObjBlobStream.Truncate;
begin
  Clear;
  FModified := True;
end;

function TObjBlobStream.Write(const Buffer; Count: integer): Longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;

{ TEntityField }

function TFreedomEntityField.AsEntity<T>: T;
begin
  Result := T(AsObject);
end;

function TFreedomEntityField.GetAsObject: TObject;
begin
  Result := TFreedomObjectDataset.VariantToObject(AsVariant);
end;

{$IFDEF DELPHIXE3_LVL}
function TFreedomEntityField.GetAsVariant: Variant;
var
  lTempBuff: TValueBuffer;
begin
  SetLength(lTempBuff, SizeOf(Variant));
  if GetData(lTempBuff) then
    // TBitConverter.ToVariant is wrong! (buggy)
    //Result := TBitConverter.ToVariant(TempBuff)
    System.Move(lTempBuff[0], Result, SizeOf(Variant))
  else
    Result := Null;
end;
{$ENDIF}

{$IFDEF DELPHIXE3_LVL}
procedure TFreedomEntityField.SetVarValue(const pValue: Variant);
begin
  SetData(BytesOf(@pValue, SizeOf(Variant)));
end;
{$ENDIF}

procedure TFreedomEntityField.SetAsObject(const pValue: TObject);
begin
  {$IFDEF NEXTGEN}
  FObject := pValue;
  {$ENDIF}
  AsVariant := TFreedomObjectDataset.ObjectToVariant(pValue);
end;

{ TFieldValue }

constructor TFieldValue.Create(pFieldName: String; pValue: Variant);
begin
  FFieldName := pFieldName;
  FValue := pValue;
end;

{ TFieldValueList }

procedure TFieldValueList.AddFieldValue(pFieldName: String; pValue: Variant);
begin
  Add(TFieldValue.Create(pFieldName, pValue));
end;

procedure TCustomFreedomObjectDataset.SetChildsObjectClass;
var
  lIndex: Integer;
  lDataSet: TCustomFreedomObjectDataSet;
  lMapper: TObjectMapper;
  lColumn: TCustomColumnMapper;
  lObjectClass: TFreedomObjectClass;
begin
  if (Assigned(FObjectClass)) then
  begin
    for lIndex := 0 to NestedDataSets.Count - 1 do
    begin
      lDataSet := TCustomFreedomObjectDataSet(NestedDataSets.Items[lIndex]);
      lMapper := ExtractMapperFrom(FObjectClass, nil);
      try
        lColumn := lMapper.Columns.FindColumn(lDataSet.DataSetField.FieldName);
        lObjectClass := ExtractObjectClassTypeFromObjectList(lColumn.RttiOptions.RttiProperty.PropertyType.AsInstance.MetaclassType.ClassInfo);
        if (Assigned(lObjectClass)) then
        begin
          lDataSet.ObjectClass := lObjectClass;
          lDataSet.ObjectClassName := lObjectClass.ClassName;
        end;
      finally
        FinalizeMapperFrom(lMapper.GetHashCode);
      end;
    end;
  end;
end;

procedure TCustomFreedomObjectDataset.EmptyDataSet;
begin
  DisableControls;
  try
    if (State in dsEditModes) then
    begin
      Cancel;
    end;
    First;
    while not IsEmpty do
    begin
      Delete;
    end;
  finally
    EnableControls;
  end;
end;

procedure TCustomFreedomObjectDataset.ClearSourceList;
begin
  if (Assigned(FInternalList)) then
  begin
    FInternalList.Empty;
  end;
end;

procedure TCustomFreedomObjectDataset.RefreshDataSetFields;
var
  lField: TField;
  lIndex: Integer;
begin
  for lIndex := 0 to FieldCount - 1 do
  begin
    lField := Fields[lIndex];
    if (lField.InheritsFrom(TDataSetField)) then
    begin
      lField.DataSet.Refresh;
    end;
  end;
end;

end.
