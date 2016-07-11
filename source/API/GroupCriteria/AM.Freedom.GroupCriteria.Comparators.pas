unit AM.Freedom.GroupCriteria.Comparators;

interface

uses
  System.Generics.Collections,
  AM.Freedom.GroupCriteria.IComparator,
  AM.Freedom.EnumerationTypes;

type
  TComparatorClass = class of TCustomComparator;

  TCustomComparator = class abstract(TInterfacedObject, IComparator)
  protected
    function GetAllowsSQLAfter: Boolean; virtual;
    function GetComparatorType: TComparatorType; virtual; abstract;
  public
    function CompareValues(pLeftValue, pRigthValue: Variant; pCompareStrOptions: TCompareStrOptions = []): Boolean; virtual; abstract;
    function Comparator: string; virtual;
    property ComparatorType: TComparatorType read GetComparatorType;
    property AllowsSQLAfter: Boolean read GetAllowsSQLAfter;
  end;

  TEqual = class(TCustomComparator)
  protected
    function GetComparatorType: TComparatorType; override;
  public
    function CompareValues(pLeftValue: Variant; pRigthValue: Variant; pCompareStrOptions: TCompareStrOptions = []): Boolean; override;
  end;

  TDifferent = class(TCustomComparator)
  protected
    function GetComparatorType: TComparatorType; override;
  public
    function CompareValues(pLeftValue: Variant; pRigthValue: Variant; pCompareStrOptions: TCompareStrOptions = []): Boolean; override;
  end;

  TGreaterThan = class(TCustomComparator)
  protected
    function GetComparatorType: TComparatorType; override;
  public
    function CompareValues(pLeftValue: Variant; pRigthValue: Variant; pCompareStrOptions: TCompareStrOptions = []): Boolean; override;
  end;

  TLessThan = class(TCustomComparator)
  protected
    function GetComparatorType: TComparatorType; override;
  public
    function CompareValues(pLeftValue: Variant; pRigthValue: Variant;
      pCompareStrOptions: TCompareStrOptions = []): Boolean; override;
  end;

  TGreaterThanOrEqualTo = class(TCustomComparator)
  protected
    function GetComparatorType: TComparatorType; override;
  public
    function CompareValues(pLeftValue: Variant; pRigthValue: Variant;
      pCompareStrOptions: TCompareStrOptions = []): Boolean; override;
  end;

  TLessThanOrEqualTo = class(TCustomComparator)
  protected
    function GetComparatorType: TComparatorType; override;
  public
    function CompareValues(pLeftValue: Variant; pRigthValue: Variant;
      pCompareStrOptions: TCompareStrOptions = []): Boolean; override;
  end;

  TNull = class(TCustomComparator)
  protected
    function GetComparatorType: TComparatorType; override;
    function GetAllowsSQLAfter: Boolean; override;
  public
    function CompareValues(pLeftValue: Variant; pRigthValue: Variant;
      pCompareStrOptions: TCompareStrOptions = []): Boolean; override;
  end;

  TContaining = class(TCustomComparator)
  protected
    function GetComparatorType: TComparatorType; override;
  public
    function CompareValues(pLeftValue: Variant; pRigthValue: Variant;
      pCompareStrOptions: TCompareStrOptions = []): Boolean; override;
  end;

  TStartingWith = class(TCustomComparator)
  protected
    function GetComparatorType: TComparatorType; override;
  public
    function CompareValues(pLeftValue: Variant; pRigthValue: Variant;
      pCompareStrOptions: TCompareStrOptions = []): Boolean; override;
  end;

  TCustomLike = class abstract(TCustomComparator);

  TLikeLeft = class(TCustomLike)
  protected
    function GetComparatorType: TComparatorType; override;
  public
    function CompareValues(pLeftValue: Variant; pRigthValue: Variant;
      pCompareStrOptions: TCompareStrOptions = []): Boolean; override;
  end;

  TLikeMidle = class(TCustomLike)
  protected
    function GetComparatorType: TComparatorType; override;
  public
    function CompareValues(pLeftValue: Variant; pRigthValue: Variant;
      pCompareStrOptions: TCompareStrOptions = []): Boolean; override;
  end;

  TLikeRigth = class(TCustomLike)
  protected
    function GetComparatorType: TComparatorType; override;
  public
    function CompareValues(pLeftValue: Variant; pRigthValue: Variant;
      pCompareStrOptions: TCompareStrOptions = []): Boolean; override;
  end;

  TBetween = class(TCustomComparator)
  protected
    function GetComparatorType: TComparatorType; override;
  public
    function CompareValues(pLeftValue: Variant; pBetweenValue, pAndValue: Variant; pCompareStrOptions: TCompareStrOptions = []): Boolean; reintroduce;
  end;

  TIn = class(TCustomComparator)
  protected
    function GetComparatorType: TComparatorType; override;
  public
    function CompareValues(pInValues: TList<Variant>; pCompareValue: Variant;
      pCompareStrOptions: TCompareStrOptions = []): Boolean; reintroduce;
  end;

  TExists = class(TCustomComparator)
  protected
    function GetComparatorType: TComparatorType; override;
  end;


implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.Variants,
  System.Math,
  AM.Freedom.Helper.ComparatorType,
  AM.Freedom.Helper.Variant;

{ TCustomComparator }

function TCustomComparator.Comparator: string;
begin
  Result := GetComparatorType.ToString;
end;

function TCustomComparator.GetAllowsSQLAfter: Boolean;
begin
  Result := True;
end;

{ TEqual }

function TEqual.CompareValues(pLeftValue, pRigthValue: Variant; pCompareStrOptions: TCompareStrOptions): Boolean;
begin
  if VarIsStr(pLeftValue) then
  begin
    if (cssCaseInsensitive in pCompareStrOptions) then
    begin
      Result := SameText(pLeftValue, pRigthValue);
    end
    else
    begin
      Result := SameStr(pLeftValue, pRigthValue);
    end;
  end
  else
  begin
    Result := VarSameValue(pLeftValue, pRigthValue);
  end;
end;

function TEqual.GetComparatorType: TComparatorType;
begin
  Result := cpEqual;
end;

{ TDifferent }

function TDifferent.CompareValues(pLeftValue, pRigthValue: Variant; pCompareStrOptions: TCompareStrOptions): Boolean;
begin
  if VarIsStr(pLeftValue) then
  begin
    if (cssCaseInsensitive in pCompareStrOptions) then
    begin
      Result := not SameText(pLeftValue, pRigthValue);
    end
    else
    begin
      Result := not SameStr(pLeftValue, pRigthValue);
    end;
  end
  else
  begin
    Result := not VarSameValue(pLeftValue, pRigthValue);
  end;
end;

function TDifferent.GetComparatorType: TComparatorType;
begin
  Result := cpDifferent;
end;

{ TGreaterThan }

function TGreaterThan.CompareValues(pLeftValue, pRigthValue: Variant; pCompareStrOptions: TCompareStrOptions): Boolean;
begin
  Result := False;
  if VarIsStr(pLeftValue) then
  begin
    if (cssCaseInsensitive in pCompareStrOptions) then
    begin
      Result := CompareText(pLeftValue, pRigthValue) > 0;
    end
    else
    begin
      Result := CompareText(pLeftValue, pRigthValue) > 0;
    end;
  end
  else
  begin
    if (VarIsOrdinal(pLeftValue)) then
    begin
      Result := CompareValue(pLeftValue, pRigthValue, 0.001) > 0;
    end
    else
    begin
      case pLeftValue.VariantType of
        ctySingle, ctyDouble, ctyCurrency, ctyExtended: Result := CompareValue(pLeftValue, pRigthValue, 0.001) > 0;
        ctyDate, ctyTime, ctyDateTime: Result := pLeftValue.TryToDateTime > pRigthValue.TryToDateTime;
      end;
    end;
  end;
end;

function TGreaterThan.GetComparatorType: TComparatorType;
begin
  Result := cpGreaterThan;
end;

{ TLessThan }

function TLessThan.CompareValues(pLeftValue, pRigthValue: Variant;
  pCompareStrOptions: TCompareStrOptions): Boolean;
begin
  Result := False;
  if VarIsStr(pLeftValue) then
  begin
    if (cssCaseInsensitive in pCompareStrOptions) then
    begin
      Result := CompareText(pLeftValue, pRigthValue) < 0;
    end
    else
    begin
      Result := CompareText(pLeftValue, pRigthValue) < 0;
    end;
  end
  else
  begin
    if (VarIsOrdinal(pLeftValue)) then
    begin
      Result := CompareValue(pLeftValue, pRigthValue, 0.001) < 0;
    end
    else
    begin
      case pLeftValue.VariantType of
        ctySingle, ctyDouble, ctyCurrency, ctyExtended: Result := CompareValue(pLeftValue, pRigthValue, 0.001) < 0;
        ctyDate, ctyTime, ctyDateTime: Result := pLeftValue.TryToDateTime < pRigthValue.TryToDateTime;
      end;
    end;
  end;
end;

function TLessThan.GetComparatorType: TComparatorType;
begin
  Result := cpLessThan;
end;

{ TGreaterThanOrEqualTo }

function TGreaterThanOrEqualTo.CompareValues(pLeftValue, pRigthValue: Variant;
  pCompareStrOptions: TCompareStrOptions): Boolean;
begin
  Result := False;
  if VarIsStr(pLeftValue) then
  begin
    if (cssCaseInsensitive in pCompareStrOptions) then
    begin
      Result := CompareText(pLeftValue, pRigthValue) >= 0;
    end
    else
    begin
      Result := CompareText(pLeftValue, pRigthValue) >= 0;
    end;
  end
  else
  begin
    if (VarIsOrdinal(pLeftValue)) then
    begin
      Result := CompareValue(pLeftValue, pRigthValue, 0.001) >= 0;
    end
    else
    begin
      case pLeftValue.VariantType of
        ctySingle, ctyDouble, ctyCurrency, ctyExtended: Result := CompareValue(pLeftValue, pRigthValue, 0.001) >= 0;
        ctyDate, ctyTime, ctyDateTime: Result := pLeftValue.TryToDateTime >= pRigthValue.TryToDateTime;
      end;
    end;
  end;
end;

function TGreaterThanOrEqualTo.GetComparatorType: TComparatorType;
begin
  Result := cpGreaterThanOrEqualTo;
end;

{ TLessThanOrEqualTo }

function TLessThanOrEqualTo.CompareValues(pLeftValue, pRigthValue: Variant;
  pCompareStrOptions: TCompareStrOptions): Boolean;
begin
  Result := False;
  if VarIsStr(pLeftValue) then
  begin
    if (cssCaseInsensitive in pCompareStrOptions) then
    begin
      Result := CompareText(pLeftValue, pRigthValue) <= 0;
    end
    else
    begin
      Result := CompareText(pLeftValue, pRigthValue) <= 0;
    end;
  end
  else
  begin
    if (VarIsOrdinal(pLeftValue)) then
    begin
      Result := CompareValue(pLeftValue, pRigthValue, 0.001) <= 0;
    end
    else
    begin
      case pLeftValue.VariantType of
        ctySingle, ctyDouble, ctyCurrency, ctyExtended: Result := CompareValue(pLeftValue, pRigthValue, 0.001) <= 0;
        ctyDate, ctyTime, ctyDateTime: Result := pLeftValue.TryToDateTime <= pRigthValue.TryToDateTime;
      end;
    end;
  end;
end;

function TLessThanOrEqualTo.GetComparatorType: TComparatorType;
begin
  Result := cpLessThanOrEqualTo;
end;

{ TNull }

function TNull.CompareValues(pLeftValue, pRigthValue: Variant; pCompareStrOptions: TCompareStrOptions): Boolean;
begin
  Result := False;
  if VarIsStr(pLeftValue) then
  begin
    Result := pLeftValue.ToString = '';
  end
  else
  begin
    if (VarIsOrdinal(pLeftValue)) then
    begin
      Result := CompareValue(pLeftValue, 0, 0.001) = 0;
    end
    else
    begin
      case pLeftValue.VariantType of
        ctySingle, ctyDouble, ctyCurrency, ctyExtended: Result := CompareValue(pLeftValue, 0, 0.001) = 0;
        ctyDate, ctyTime, ctyDateTime: Result := pLeftValue.TryToDateTime = 0;
      end;
    end;
  end;
end;

function TNull.GetAllowsSQLAfter: Boolean;
begin
  Result := False;
end;

function TNull.GetComparatorType: TComparatorType;
begin
  Result := cpNull;
end;

{ TContaining }

function TContaining.CompareValues(pLeftValue, pRigthValue: Variant;
  pCompareStrOptions: TCompareStrOptions): Boolean;
var
  lLeftStr, lRigthStr: String;
begin
  lLeftStr := VarToStr(pLeftValue);
  lRigthStr := VarToStr(pRigthValue);
  if (cssCaseInsensitive in pCompareStrOptions) then
  begin
    Result := ContainsText(lLeftStr, lRigthStr);
  end
  else
  begin
    Result := ContainsStr(lLeftStr, lRigthStr);
  end;
end;

function TContaining.GetComparatorType: TComparatorType;
begin
  Result := cpContaining;
end;

{ TStartingWith }

function TStartingWith.CompareValues(pLeftValue, pRigthValue: Variant;
  pCompareStrOptions: TCompareStrOptions): Boolean;
var
  lLeftStr, lRigthStr: String;
begin
  lLeftStr := VarToStr(pLeftValue);
  lRigthStr := VarToStr(pRigthValue);
  if (cssCaseInsensitive in pCompareStrOptions) then
  begin
    Result := StartsText(lRigthStr, lLeftStr);
  end
  else
  begin
    Result := StartsStr(lRigthStr, lLeftStr);
  end;
end;

function TStartingWith.GetComparatorType: TComparatorType;
begin
  Result := cpStartingWith;
end;

{ TLikeLeft }

function TLikeLeft.CompareValues(pLeftValue, pRigthValue: Variant;
  pCompareStrOptions: TCompareStrOptions): Boolean;
var
  lLeftStr, lRigthStr: String;
begin
  lLeftStr := VarToStr(pLeftValue);
  lRigthStr := VarToStr(pRigthValue);
  if (cssCaseInsensitive in pCompareStrOptions) then
  begin
    Result := EndsText(lRigthStr, lLeftStr);
  end
  else
  begin
    Result := EndsStr(lRigthStr, lLeftStr);
  end;
end;

function TLikeLeft.GetComparatorType: TComparatorType;
begin
  Result := cpLikeLeft;
end;

{ TLikeMidle }

function TLikeMidle.CompareValues(pLeftValue, pRigthValue: Variant;
  pCompareStrOptions: TCompareStrOptions): Boolean;
var
  lLeftStr, lRigthStr: String;
begin
  lLeftStr := VarToStr(pLeftValue);
  lRigthStr := VarToStr(pRigthValue);
  if (cssCaseInsensitive in pCompareStrOptions) then
  begin
    Result := ContainsText(lLeftStr, lRigthStr);
  end
  else
  begin
    Result := ContainsStr(lLeftStr, lRigthStr);
  end;
end;

function TLikeMidle.GetComparatorType: TComparatorType;
begin
  Result := cpLikeMidle;
end;

{ TLikeRigth }

function TLikeRigth.CompareValues(pLeftValue, pRigthValue: Variant;
  pCompareStrOptions: TCompareStrOptions): Boolean;
var
  lLeftStr, lRigthStr: String;
begin
  lLeftStr := VarToStr(pLeftValue);
  lRigthStr := VarToStr(pRigthValue);
  if (cssCaseInsensitive in pCompareStrOptions) then
  begin
    Result := StartsText(lRigthStr, lLeftStr);
  end
  else
  begin
    Result := StartsStr(lRigthStr, lLeftStr);
  end;
end;

function TLikeRigth.GetComparatorType: TComparatorType;
begin
  Result := cpLikeRigth;
end;

{ TBetween }

function TBetween.CompareValues(pLeftValue, pBetweenValue, pAndValue: Variant; pCompareStrOptions: TCompareStrOptions): Boolean;
begin
  Result := False;
  if VarIsStr(pLeftValue) then
  begin
    if (cssCaseInsensitive in pCompareStrOptions) then
    begin
      Result := CompareText(pLeftValue, pBetweenValue) >= 0;
    end
    else
    begin
      Result := CompareText(pLeftValue, pBetweenValue) >= 0;
    end;
  end
  else
  begin
    if (VarIsOrdinal(pLeftValue)) then
    begin
      Result := CompareValue(pLeftValue, pBetweenValue, 0.001) >= 0;
    end
    else
    begin
      case pLeftValue.VariantType of
        ctySingle, ctyDouble, ctyCurrency, ctyExtended: Result := CompareValue(pLeftValue, pBetweenValue, 0.001) >= 0;
        ctyDate, ctyTime, ctyDateTime: Result := pLeftValue.TryToDateTime >= pBetweenValue.TryToDateTime;
      end;
    end;
  end;

  if VarIsStr(pLeftValue) then
  begin
    if (cssCaseInsensitive in pCompareStrOptions) then
    begin
      Result := Result and (CompareText(pLeftValue, pAndValue) <= 0);
    end
    else
    begin
      Result := Result and (CompareText(pLeftValue, pAndValue) <= 0);
    end;
  end
  else
  begin
    if (VarIsOrdinal(pLeftValue)) then
    begin
      Result := Result and (CompareValue(pLeftValue, pAndValue, 0.001) <= 0);
    end
    else
    begin
      case pLeftValue.VariantType of
        ctySingle, ctyDouble, ctyCurrency, ctyExtended: Result := Result and (CompareValue(pLeftValue, pAndValue, 0.001) <= 0);
        ctyDate, ctyTime, ctyDateTime: Result := Result and (pLeftValue.TryToDateTime <= pAndValue.TryToDateTime);
      end;
    end;
  end;
end;

function TBetween.GetComparatorType: TComparatorType;
begin
  Result := cpBetween;
end;

{ TIn }

function TIn.CompareValues(pInValues: TList<Variant>; pCompareValue: Variant; pCompareStrOptions: TCompareStrOptions): Boolean;
var
  lIndex: Integer;
  lValue: Variant;
begin
  Result := False;
  lIndex := 0;
  while (not Result) and (lIndex <= pInValues.Count - 1) do
  begin
    lValue := pInValues.Items[lIndex];
    if VarIsStr(lValue) then
    begin
      if (cssCaseInsensitive in pCompareStrOptions) then
      begin
        Result := SameText(lValue, pCompareValue);
      end
      else
      begin
        Result := SameStr(lValue, pCompareValue);
      end;
    end
    else
    begin
      Result := VarSameValue(lValue, pCompareValue);
    end;
    Inc(lIndex);
  end;
end;

function TIn.GetComparatorType: TComparatorType;
begin
  Result := cpIn;
end;

{ TExists }

function TExists.GetComparatorType: TComparatorType;
begin
  Result := cpExists;
end;

end.
