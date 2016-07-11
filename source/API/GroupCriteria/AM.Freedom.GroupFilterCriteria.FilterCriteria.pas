unit AM.Freedom.GroupFilterCriteria.FilterCriteria;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  AM.Freedom.GroupCriteria.Comparators,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.InterfacedObjects,
  AM.Freedom.GroupFilterCriteria.IFilterCriteria;

type
  TFilterCriteria = class(TFreedomInterfacedObject, IFilterCriteria)
  private
    FPropertyName: String;
    FPropertyValue: Variant;
    FComparator: TCustomComparator;
    FInValues: TList<Variant>;
    FAndValue: Variant;
    FCompareValue: Variant;
    FCompareStrOptions: TCompareStrOptions;
    procedure SetPropertyValue(pValue: Variant);
    constructor Create(pPropertyName: String; pCompareValue: Variant; pComparatorClass: TComparatorClass; pCompareStrOptions: TCompareStrOptions = []);
  public
    destructor Destroy; override;
    function CompareValues: Boolean;

    class function CreateAsEqual(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions = []): TFilterCriteria;
    class function CreateAsDifferent(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions = []): TFilterCriteria;
    class function CreateAsGreaterThan(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions = []): TFilterCriteria;
    class function CreateAsLessThan(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions = []): TFilterCriteria;
    class function CreateAsGreaterThanOrEqualTo(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions = []): TFilterCriteria;
    class function CreateAsLessThanOrEqualTo(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions = []): TFilterCriteria;
    class function CreateAsContaining(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions = []): TFilterCriteria;
    class function CreateAsNull(pPropertyName: String): TFilterCriteria;
    class function CreateAsNotNull(pPropertyName: String): TFilterCriteria;
    class function CreateAsStartingWith(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions = []): TFilterCriteria;
    class function CreateAsLikeLeft(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions = []): TFilterCriteria;
    class function CreateAsLikeMidle(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions = []): TFilterCriteria;
    class function CreateAsLikeRigth(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions = []): TFilterCriteria;
    class function CreateAsBetween(pPropertyName: String; pBetweenValue, pAndValue: Variant; pCompareStrOptions: TCompareStrOptions = []): TFilterCriteria;
    class function CreateAsIn(pPropertyName: String; pInValues: Array of Variant; pCompareStrOptions: TCompareStrOptions = []): TFilterCriteria; overload;
    class function CreateAsIn(pPropertyName: String; pInValues: TList<Variant>; pCompareStrOptions: TCompareStrOptions = []): TFilterCriteria; overload;

    property PropertyName: String read FPropertyName;
    property PropertyValue: Variant read FPropertyValue;
    property CompareValue: Variant read FCompareValue;
    property Comparator: TCustomComparator read FComparator;
    property InValues: TList<Variant> read FInValues;
    property AndValue: Variant read FAndValue;
  end;

  TListFilterCriterias = class(TObjectList<TFilterCriteria>)
  public
    procedure AddCriteria(pFilterCriteria: TFilterCriteria); overload;
    procedure AddCriteria(pFilterCriteria: array of TFilterCriteria); overload;
    function CompareValues(pPolicy: TPolicy): Boolean;
  end;



implementation

uses
  System.Variants;

{ TFilterCriteria }

function TFilterCriteria.CompareValues: Boolean;
begin
  Result := False;
  if FComparator.ComparatorType in [cpEqual, cpDifferent, cpGreaterThan, cpLessThan,
      cpGreaterThanOrEqualTo, cpLessThanOrEqualTo, cpNull, cpNotNull, cpContaining,
      cpStartingWith, cpLikeLeft, cpLikeMidle, cpLikeRigth] then
  begin
    Result := FComparator.CompareValues(FPropertyValue, FCompareValue, FCompareStrOptions);
  end
  else if FComparator.ComparatorType = cpBetween then
  begin
    Result := TBetween(FComparator).CompareValues(FPropertyValue, FAndValue, FCompareValue, FCompareStrOptions);
  end
  else if FComparator.ComparatorType = cpIn then
  begin
    Result := TIn(FComparator).CompareValues(FInValues, FCompareValue, FCompareStrOptions);
  end;
end;

constructor TFilterCriteria.Create(pPropertyName: String; pCompareValue: Variant; pComparatorClass: TComparatorClass; pCompareStrOptions: TCompareStrOptions);
begin
  FPropertyName := pPropertyName;
  FComparator := pComparatorClass.Create;
  FCompareValue := pCompareValue;
  FCompareStrOptions := pCompareStrOptions;
end;

class function TFilterCriteria.CreateAsBetween(pPropertyName: String; pBetweenValue, pAndValue: Variant; pCompareStrOptions: TCompareStrOptions): TFilterCriteria;
begin
  Result := Self.Create(pPropertyName, pBetweenValue, TBetween, pCompareStrOptions);
  Result.FAndValue := pAndValue;
end;

class function TFilterCriteria.CreateAsContaining(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions): TFilterCriteria;
begin
  Result := Self.Create(pPropertyName, pCompareValue, TContaining, pCompareStrOptions);
end;

class function TFilterCriteria.CreateAsDifferent(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions): TFilterCriteria;
begin
  Result := Self.Create(pPropertyName, pCompareValue, TDifferent, pCompareStrOptions);
end;

class function TFilterCriteria.CreateAsEqual(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions): TFilterCriteria;
begin
  Result := Self.Create(pPropertyName, pCompareValue, TEqual, pCompareStrOptions);
end;

class function TFilterCriteria.CreateAsGreaterThan(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions): TFilterCriteria;
begin
  Result := Self.Create(pPropertyName, pCompareValue, TGreaterThan, pCompareStrOptions);
end;

class function TFilterCriteria.CreateAsGreaterThanOrEqualTo(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions): TFilterCriteria;
begin
  Result := Self.Create(pPropertyName, pCompareValue, TGreaterThanOrEqualTo, pCompareStrOptions);
end;

class function TFilterCriteria.CreateAsIn(pPropertyName: String; pInValues: TList<Variant>; pCompareStrOptions: TCompareStrOptions): TFilterCriteria;
begin
  Result := Self.Create(pPropertyName, Null, TIn, pCompareStrOptions);
  Result.FInValues := pInValues;
end;

class function TFilterCriteria.CreateAsIn(pPropertyName: String; pInValues: array of Variant; pCompareStrOptions: TCompareStrOptions): TFilterCriteria;
var
  lIndex: Integer;
begin
  Result := Self.Create(pPropertyName, Null, TIn, pCompareStrOptions);
  Result.FInValues := TList<Variant>.Create;
  for lIndex := Low(pInValues) to High(pInValues) do
  begin
    Result.FInValues.Add(pInValues[lIndex]);
  end;
end;

class function TFilterCriteria.CreateAsLessThan(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions): TFilterCriteria;
begin
  Result := Self.Create(pPropertyName, pCompareValue, TLessThan, pCompareStrOptions);
end;

class function TFilterCriteria.CreateAsLessThanOrEqualTo(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions): TFilterCriteria;
begin
  Result := Self.Create(pPropertyName, pCompareValue, TLessThanOrEqualTo, pCompareStrOptions);
end;

class function TFilterCriteria.CreateAsLikeLeft(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions): TFilterCriteria;
begin
  Result := Self.Create(pPropertyName, pCompareValue, TLikeLeft, pCompareStrOptions);
end;

class function TFilterCriteria.CreateAsLikeMidle(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions): TFilterCriteria;
begin
  Result := Self.Create(pPropertyName, pCompareValue, TLikeMidle, pCompareStrOptions);
end;

class function TFilterCriteria.CreateAsLikeRigth(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions): TFilterCriteria;
begin
  Result := Self.Create(pPropertyName, pCompareValue, TLikeRigth, pCompareStrOptions);
end;

class function TFilterCriteria.CreateAsNotNull(pPropertyName: String): TFilterCriteria;
begin
  Result := Self.Create(pPropertyName, Null, TDifferent, []);
end;

class function TFilterCriteria.CreateAsNull(pPropertyName: String): TFilterCriteria;
begin
  Result := Self.Create(pPropertyName, Null, TEqual, []);
end;

class function TFilterCriteria.CreateAsStartingWith(pPropertyName: String; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions): TFilterCriteria;
begin
  Result := Self.Create(pPropertyName, pCompareValue, TStartingWith, pCompareStrOptions);
end;

destructor TFilterCriteria.Destroy;
begin
  FComparator.Free;
  FInValues.Free;
  inherited;
end;

procedure TFilterCriteria.SetPropertyValue(pValue: Variant);
begin
  FPropertyValue := pValue;
end;

{ TListFilterCriterias }

procedure TListFilterCriterias.AddCriteria(pFilterCriteria: TFilterCriteria);
begin
  Add(pFilterCriteria);
end;

procedure TListFilterCriterias.AddCriteria(pFilterCriteria: array of TFilterCriteria);
begin
  AddRange(pFilterCriteria);
end;

function TListFilterCriterias.CompareValues(pPolicy: TPolicy): Boolean;
var
  lCriteria: TFilterCriteria;
  lIndex: Integer;
begin
  Result := True;
  for lIndex := 0 to Count - 1 do
  begin
    lCriteria  := Items[lIndex];
    if (lIndex > 0) then
    begin
      case pPolicy of
        poAnd: Result := Result and lCriteria.CompareValues;
        poOr: Result := Result or lCriteria.CompareValues;
        poAndNot: Result := Result and (not lCriteria.CompareValues);
        poOrNot: Result := Result or (not lCriteria.CompareValues);
      end;
    end
    else
    begin
      case pPolicy of
        poAnd: Result := lCriteria.CompareValues;
        poOr: Result := lCriteria.CompareValues;
        poAndNot: Result := not lCriteria.CompareValues;
        poOrNot: Result := not lCriteria.CompareValues;
      end;
    end;
    if (pPolicy = poOr) and Result then
    begin
      Break;
    end;
  end;
end;

end.