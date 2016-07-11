unit AM.Freedom.GroupCriteria.Criteria;

interface

uses
  System.Generics.Collections,
  System.Variants,
  AM.Freedom.GroupCriteria.ICriteria,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria.Comparators,
  AM.Freedom.Helper.Variant,
  AM.Freedom.SQLMappers.AliasableObject,
  AM.Freedom.GroupCriteria.ValueCriteria,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.Exceptions,
  AM.Freedom.JSONFreedomObject,
  AM.Freedom.InterfacedObjects;

type
  TCriteria = class(TJSONFreedomObject, ICriteria)
  strict private
    FLeftArgument: TCustomArgument;
    FComparator: TCustomComparator;
    FRigthArgument: TCustomArgument;
    function IsValidArgument(pArgument: TCustomArgument): Boolean;
  strict protected
    function GetLeftArgument: TCustomArgument; virtual;
    function GetComparator: TCustomComparator; virtual;
    function GetRigthArgument: TCustomArgument; virtual;

    procedure SetLeftArgument(const pLeftArgument: TCustomArgument); virtual;
    procedure SetComparator(const pComparator: TCustomComparator); virtual;
    procedure SetRigthArgument(const pRigthArgument: TCustomArgument); virtual;
  public
    constructor Create; overload;
    constructor Create(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument; pComparator: TComparatorClass); overload;
    destructor Destroy; override;

    class function CreateAsEqual(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
    class function CreateAsDifferent(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
    class function CreateAsGreaterThan(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
    class function CreateAsLessThan(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
    class function CreateAsGreaterThanOrEqualTo(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
    class function CreateAsLessThanOrEqualTo(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
    class function CreateAsContaining(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
    class function CreateAsNull(pLeftArgument: TCustomArgument): TCriteria;
    class function CreateAsNotNull(pLeftArgument: TCustomArgument): TCriteria;
    class function CreateAsStartingWith(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
    class function CreateAsLikeLeft(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
    class function CreateAsLikeMidle(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
    class function CreateAsLikeRigth(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
    class function CreateAsBetween(pLeftArgument: TCustomArgument; pBetweenArgument, pAndArgument: TCustomArgument): TCriteria;
    class function CreateAsIn(pLeftArgument: TCustomArgument; pInArguments: Array of TCustomArgument): TCriteria; overload;
    class function CreateAsIn(pLeftArgument: TCustomArgument; pInArguments: TList<TCustomArgument>): TCriteria; overload;
    class function CreateAsExists(pSelectArgument: TCustomArgument): TCriteria;

    property LeftArgument: TCustomArgument read GetLeftArgument write SetLeftArgument;
    property Comparator: TCustomComparator read GetComparator write SetComparator;
    property RigthArgument: TCustomArgument read GetRigthArgument write SetRigthArgument;
  end;

  TListCriterias = class(TInterfacedObjectList<TCriteria>)
  strict private
    function GetValueArgument<T>(pValue: T): TValueArgument;
  protected
    procedure Notify(const Value: TCriteria; Action: TCollectionNotification); override;
  public
    function AddFieldToValueArgument<T>(pFieldName: String; pComparator: TComparatorClass; pValue: T;
        pTableAlias: String = ''): TCriteria;
    function AddFieldToFieldArgument(pLeftFieldName: String; pComparator: TComparatorClass; pRigthFieldName: String;
        pLeftTableAlias: String = ''; pRigthTableAlias: String = ''): TCriteria;
  end;

implementation

uses
  System.SysUtils,
  System.Rtti,
  AM.Freedom.SQLMappers.SelectClause;

{ TCustomCriteria }


constructor TCriteria.Create(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument; pComparator: TComparatorClass);
begin
  SetLeftArgument(pLeftArgument);
  FComparator := pComparator.Create;
  SetRigthArgument(pRigthArgument);
end;

destructor TCriteria.Destroy;
begin
  FreeAndNil(FComparator);
  FreeAndNil(FLeftArgument);
  FreeAndNil(FRigthArgument);
  inherited;
end;

function TCriteria.GetComparator: TCustomComparator;
begin
  Result := FComparator;
end;

function TCriteria.GetLeftArgument: TCustomArgument;
begin
  Result := FLeftArgument
end;

function TCriteria.GetRigthArgument: TCustomArgument;
begin
  Result := FRigthArgument
end;

function TCriteria.IsValidArgument(pArgument: TCustomArgument): Boolean;
begin
  Result := True;
end;

procedure TCriteria.SetComparator(const pComparator: TCustomComparator);
begin
  FComparator := pComparator;
end;

procedure TCriteria.SetLeftArgument(const pLeftArgument: TCustomArgument);
begin
  if IsValidArgument(pLeftArgument) then
  begin
    if FLeftArgument <> pLeftArgument then
    begin
      FreeAndNil(FLeftArgument);
    end;
    FLeftArgument := pLeftArgument;
  end else
  begin
    if Assigned(pLeftArgument) then
    begin
      raise EInvalidArgument.Create(pLeftArgument.ClassName, Self.ClassName);
    end;
  end;
end;

procedure TCriteria.SetRigthArgument(const pRigthArgument: TCustomArgument);
begin
  if IsValidArgument(pRigthArgument) then
  begin
    if FRigthArgument <> pRigthArgument then
    begin
      FreeAndNil(FRigthArgument);
    end;
    FRigthArgument := pRigthArgument;
  end else
  begin
    if Assigned(pRigthArgument) then
    begin
      raise EInvalidArgument.Create(pRigthArgument.ClassName, Self.ClassName);
    end;
  end;
end;

constructor TCriteria.Create;
begin
  FLeftArgument := nil;
  FComparator := TEqual.Create;
  FRigthArgument := nil;
end;

class function TCriteria.CreateAsBetween(pLeftArgument, pBetweenArgument, pAndArgument: TCustomArgument): TCriteria;
begin
  Result := Self.Create(pLeftArgument, TBetweenArgument.Create(pBetweenArgument, pAndArgument), TBetween);
end;

class function TCriteria.CreateAsContaining(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
begin
  Result := Self.Create(pLeftArgument, pRigthArgument, TContaining);
end;

class function TCriteria.CreateAsDifferent(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
begin
  Result := Self.Create(pLeftArgument, pRigthArgument, TDifferent);
end;

class function TCriteria.CreateAsEqual(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
begin
  Result := Self.Create(pLeftArgument, pRigthArgument, TEqual);
end;

class function TCriteria.CreateAsExists(pSelectArgument: TCustomArgument): TCriteria;
begin
  if pSelectArgument.ClassType <> TSelectClause then
  begin
    raise Exception.Create('Rigth argument in CreateAsExists needs TSelectClause');
  end;
  Result := TCriteria.Create(nil, pSelectArgument, TExists);
end;

class function TCriteria.CreateAsGreaterThan(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
begin
  Result := Self.Create(pLeftArgument, pRigthArgument, TGreaterThan);
end;

class function TCriteria.CreateAsGreaterThanOrEqualTo(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
begin
  Result := Self.Create(pLeftArgument, pRigthArgument, TGreaterThanOrEqualTo);
end;

class function TCriteria.CreateAsIn(pLeftArgument: TCustomArgument;
  pInArguments: TList<TCustomArgument>): TCriteria;
begin
  Result := Self.Create(pLeftArgument, TInArgument.Create(pInArguments.ToArray), TIn);
end;

class function TCriteria.CreateAsIn(pLeftArgument: TCustomArgument; pInArguments: Array of TCustomArgument): TCriteria;
begin
  Result := Self.Create(pLeftArgument, TInArgument.Create(pInArguments), TIn);
end;

class function TCriteria.CreateAsLessThan(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
begin
  Result := Self.Create(pLeftArgument, pRigthArgument, TLessThan);
end;

class function TCriteria.CreateAsLessThanOrEqualTo(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
begin
  Result := Self.Create(pLeftArgument, pRigthArgument, TLessThanOrEqualTo);
end;

class function TCriteria.CreateAsLikeLeft(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
begin
  Result := Self.Create(pLeftArgument, pRigthArgument, TLikeLeft);
end;

class function TCriteria.CreateAsLikeMidle(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
begin
  Result := Self.Create(pLeftArgument, pRigthArgument, TLikeMidle);
end;

class function TCriteria.CreateAsLikeRigth(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
begin
  Result := Self.Create(pLeftArgument, pRigthArgument, TLikeRigth);
end;

class function TCriteria.CreateAsNotNull(pLeftArgument: TCustomArgument): TCriteria;
begin
  Result := Self.Create(pLeftArgument, TNullArgument.Create,  TDifferent);
end;

class function TCriteria.CreateAsNull(pLeftArgument: TCustomArgument): TCriteria;
begin
  Result := Self.Create(pLeftArgument, TNullArgument.Create,  TEqual);
end;

class function TCriteria.CreateAsStartingWith(pLeftArgument: TCustomArgument; pRigthArgument: TCustomArgument): TCriteria;
begin
  Result := Self.Create(pLeftArgument, pRigthArgument, TStartingWith);
end;



{ TCriterias }

function TListCriterias.AddFieldToFieldArgument(pLeftFieldName: String; pComparator: TComparatorClass; pRigthFieldName,
  pLeftTableAlias, pRigthTableAlias: String): TCriteria;
begin
  Result := TCriteria.Create(TFieldArgument.Create(pLeftFieldName, pLeftTableAlias, ''),
      TFieldArgument.Create(pRigthFieldName, pRigthTableAlias, ''), pComparator);
end;

function TListCriterias.AddFieldToValueArgument<T>(pFieldName: String; pComparator: TComparatorClass; pValue: T;
        pTableAlias: String): TCriteria;
begin
  Result := TCriteria.Create(TFieldArgument.Create(pFieldName, pTableAlias, ''),
    GetValueArgument<T>(pValue), pComparator);
end;

function TListCriterias.GetValueArgument<T>(pValue: T): TValueArgument;
var
  lValue: TValue;
begin
  lValue := TValue.From<T>(pValue);
  if TypeInfo(T) = TypeInfo(String) then
  begin
    Result := TValueArgument.CreateAsString(lValue.AsString);
  end else if TypeInfo(T) = TypeInfo(Smallint) then
  begin
    Result := TValueArgument.CreateAsSmallint(lValue.AsInteger);
  end else if TypeInfo(T) = TypeInfo(Integer) then
  begin
    Result := TValueArgument.CreateAsInteger(lValue.AsInteger);
  end else if TypeInfo(T) = TypeInfo(TDate) then
  begin
    Result := TValueArgument.CreateAsDate(lValue.AsType<TDate>);
  end else if TypeInfo(T) = TypeInfo(TTime) then
  begin
    Result := TValueArgument.CreateAsTime(lValue.AsType<TTime>);
  end else if TypeInfo(T) = TypeInfo(TDateTime) then
  begin
    Result := TValueArgument.CreateAsDateTime(lValue.AsType<TDateTime>);
  end else if TypeInfo(T) = TypeInfo(Single) then
  begin
    Result := TValueArgument.CreateAsDouble(lValue.AsExtended);
  end else if TypeInfo(T) = TypeInfo(Double) then
  begin
    Result := TValueArgument.CreateAsDouble(lValue.AsExtended);
  end else if TypeInfo(T) = TypeInfo(Extended) then
  begin
    Result := TValueArgument.CreateAsExtended(lValue.AsExtended);
  end else if TypeInfo(T) = TypeInfo(Currency) then
  begin
    Result := TValueArgument.CreateAsCurrency(lValue.AsCurrency);
  end;
end;

procedure TListCriterias.Notify(const Value: TCriteria; Action: TCollectionNotification);
begin
  inherited;
  if (Action = cnAdded) then
  begin
    Value.RegisterFreeNotification(Self);
  end;
end;

end.
