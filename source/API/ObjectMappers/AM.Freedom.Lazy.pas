unit AM.Freedom.Lazy;

interface

uses
  System.Generics.Collections,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ILazy,
  AM.Freedom.IFreedomObject,
  AM.Freedom.Exceptions,
  AM.Freedom.GroupCriteria,
  AM.Freedom.ObjectMapper.LiveRefresh,
  AM.Freedom.JSONFreedomObject,
  Data.DBXJSONReflect;

type
  TLazy<T: class> = class(TJSONFreedomObject, ILazy)
  strict private
    {$HINTS OFF}
    [JSONMarshalled(False)]
    FClassType: T;
    {$HINTS ON}
    [JSONMarshalled(False)]
    FValue: T;
    FLazySearch: TGroupCriteria;
    FLazyID: Variant;
    [JSONMarshalled(False)]
    FLiveRefresh: Boolean;
    [JSONMarshalled(False)]
    FOnGetLiveRefreshColumnValue: TOnGetLiveRefreshColumnValue;
    [JSONMarshalled(False)]
    FLastColumnValue: Variant;
    procedure SetValue(const pValue: T);
    procedure SetLazySearch(const pSearch: TGroupCriteria);
    function GetLoaded: Boolean;
    function GetObject: TObject;
    function GetValue: T;
    function LiveRefreshModified: Boolean;
    procedure SetOnGetLiveRefreshColumnValue(pOnGetLiveRefreshColumnValue: TOnGetLiveRefreshColumnValue);
    function GetLazyType: TLazyType;
    function GetClassType: TClass;
  strict protected
    procedure Load;
    function GetLazyID: Variant;
    procedure SetLazyID(pLazyID: Variant);
    function GetLazySearch: TGroupCriteria;
    function GetInternalValue: T;
    procedure SetInternalValue(pValue: T);
    procedure SetFreeInternalValue;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reload;
    property Value: T read GetValue write SetValue;
    property LazyType: TLazyType read GetLazyType;
    property Loaded: Boolean read GetLoaded;
    property LiveRefresh: Boolean read FLiveRefresh write FLiveRefresh default False;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  AM.Freedom.Helper.RttiField,
  AM.Freedom.FreedomObject,
  AM.Freedom.Helper.Variant, 
  AM.Freedom.LiveRefreshObject, 
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMappers.Arguments;

{ TLazy<T> }

constructor TLazy<T>.Create;
begin
  FLazyID := Null;
  FLiveRefresh := False;
  FLastColumnValue := Null;
end;

destructor TLazy<T>.Destroy;
begin
  FreeAndNil(FLazySearch);
  FreeAndNil(FValue);
  inherited;
end;

function TLazy<T>.GetLazyType: TLazyType;
begin
  Result := TLazyType.Simple;
end;

function TLazy<T>.LiveRefreshModified: Boolean;
var
  lLastKey: TLiveRefreshs;
  lLiveRefresh: TLiveRefresh;
  lColumnValue: Variant;
  lLiveRefreshObject: TLiveRefreshObject;
  lGroupCriteria: TGroupCriteria;
  lValueArgument: TValueArgument;
begin
  Result := False;
  if (Assigned(FOnGetLiveRefreshColumnValue)) then
  begin
    lLiveRefreshObject := FOnGetLiveRefreshColumnValue(Self);
    try
      Result := (lLiveRefreshObject.Value <> FLastColumnValue);
      if Result then
      begin
        FLastColumnValue := Null;
        SetLazyID(FLastColumnValue);
        if (lLiveRefreshObject.ColumnName = lLiveRefreshObject.RefColumnName) then
        begin
          FLastColumnValue := lLiveRefreshObject.Value;
          SetLazyID(FLastColumnValue);
        end
        else
        begin
          lGroupCriteria := TGroupCriteria.Create;
          case lLiveRefreshObject.ColumnType of
            ctyByte, ctySmallint, ctyInteger: lValueArgument := TValueArgument.CreateAsInteger(lLiveRefreshObject.Value);
            ctyInt64: lValueArgument := TValueArgument.CreateAsInt64(lLiveRefreshObject.Value);
            ctyChar, ctyString: lValueArgument := TValueArgument.CreateAsString(lLiveRefreshObject.Value);
            ctySingle, ctyDouble, ctyCurrency, ctyExtended: lValueArgument := TValueArgument.CreateAsCurrency(lLiveRefreshObject.Value);
          end;
          lGroupCriteria.AddCriteria(TCriteria.CreateAsEqual(TFieldArgument.Create(lLiveRefreshObject.RefColumnName),
            lValueArgument));
          SetLazySearch(lGroupCriteria);
        end;
      end;
    finally
      lLiveRefreshObject.Free;
    end;
  end;
end;

procedure TLazy<T>.Load;
var
  lValue: TFreedomObject;
  lFreedomObject: IFreedomObject;
  lClass: TFreedomObjectClass;
  lId: Variant;
begin
  lId := GetLazyID;
  lValue := nil;
  lClass := TFreedomObjectClass(T);
  lValue := lClass.Create;
  if lId.IsNotNull then
  begin
   Supports(lValue, IFreedomObject, lFreedomObject);
    lFreedomObject.DoFindWithID(lId);
  end
  else
  begin
    Supports(lValue, IFreedomObject, lFreedomObject);
    lFreedomObject.DoSearch(GetLazySearch);
  end;
  SetInternalValue(lValue);
end;

procedure TLazy<T>.Reload;
begin
  SetFreeInternalValue;
end;

function TLazy<T>.GetInternalValue: T;
begin
  Result := FValue;
end;

function TLazy<T>.GetLazyID: Variant;
begin
  Result := FLazyID;
end;

function TLazy<T>.GetLazySearch: TGroupCriteria;
begin
  Result := FLazySearch;
end;

function TLazy<T>.GetLoaded: Boolean;
begin
  Result := Assigned(FValue);
end;

function TLazy<T>.GetObject: TObject;
begin
  Result := FValue;
end;

function TLazy<T>.GetValue: T;
begin
  if (FLiveRefresh) and (LiveRefreshModified) then
  begin
    SetFreeInternalValue;
  end;
  if not Assigned(FValue) then
  begin
    Load;
  end;
  Result := FValue;
end;

procedure TLazy<T>.SetFreeInternalValue;
begin
  if Assigned(FValue) then
  begin
    FValue.Free;
  end;
  FValue := nil;
end;

procedure TLazy<T>.SetInternalValue(pValue: T);
begin
  if (FValue <> pValue) then
  begin
    FreeAndNil(FValue);
  end;
  FValue := pValue;
end;

procedure TLazy<T>.SetLazyID(pLazyID: Variant);
begin
  FLazyID := pLazyID;
end;

procedure TLazy<T>.SetLazySearch(const pSearch: TGroupCriteria);
begin
  FreeAndNil(FLazySearch);
  FLazySearch := pSearch;
end;

procedure TLazy<T>.SetOnGetLiveRefreshColumnValue(pOnGetLiveRefreshColumnValue: TOnGetLiveRefreshColumnValue);
begin
  FOnGetLiveRefreshColumnValue := pOnGetLiveRefreshColumnValue;
end;

procedure TLazy<T>.SetValue(const pValue: T);
begin
  FreeAndNil(FValue);
  FValue := pValue;
end;

function TLazy<T>.GetClassType: TClass;
begin
  if Assigned(FClassType) then
  begin
    Result := FClassType;
  end
  else
  begin
    Result := T;
  end;
end;

end.
