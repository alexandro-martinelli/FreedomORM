unit AM.Freedom.SQLMappers.SQLFieldList;

interface

uses
  System.Generics.Collections,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLMappers.Expressions,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLCommands.CustomFieldCommand;

type
  TSQLFieldList = class(TObjectList<TCustomArgument>)
  public
    constructor Create(pFields: Array of TCustomArgument); overload;
    function FindField(const pName: string; const pTableAlias: String = ''; const pAlias: String = ''): TFieldArgument;
    function FieldExists(const pName: string; const pTableAlias: String = ''; const pAlias: String = ''): Boolean;
    function FieldNotExists(const pName: string; const pTableAlias: String = ''; const pAlias: String = ''): Boolean;
    function AddField(const pName: string; const pTableAlias: String = ''; const pAlias: String = ''): TFieldArgument;
    function AddLiteral(const pLiteralValue: string): TLiteralArgument;

    function AddStringValue(pValue: String): TValueArgument;
    function AddSmallintValue(pValue: Smallint): TValueArgument;
    function AddIntegerValue(pValue: Integer): TValueArgument;
    function AddInt64Value(pValue: Int64): TValueArgument;
    function AddDoubleValue(pValue: Double): TValueArgument;
    function AddExtendedValue(pValue: Extended): TValueArgument;
    function AddCurrencyValue(pValue: Currency): TValueArgument;
    function AddDateValue(pValue: TDate): TValueArgument;
    function AddTimeValue(pValue: TTime): TValueArgument;
    function AddDateTimeValue(pValue: TDateTime): TValueArgument;
    function AddValue<T>(pValue: T): TValueArgument;
    function AddSum(pArgument: TCustomArgument): TSum;
    function AddMin(pArgument: TCustomArgument): TMin;
    function AddMax(pArgument: TCustomArgument): TMax;
    function AddAvg(pArgument: TCustomArgument): TAvg;
    function AddCount(pArgument: TCustomArgument): TCount;
    function AddCoalesce(pArgument: TCustomArgument; pAnotherArguments: Array of TCustomArgument): TCoalesce;
    function AddCast(pArgument: TCustomArgument; pCastAs: TCustomFieldCommand): TCast;
  end;

implementation

uses
  System.SysUtils;

{ TSQLFieldList }

function TSQLFieldList.AddAvg(pArgument: TCustomArgument): TAvg;
begin
  Result := TAvg.Create(pArgument);
  Add(Result);
end;

function TSQLFieldList.AddCast(pArgument: TCustomArgument; pCastAs: TCustomFieldCommand): TCast;
begin
  Result := TCast.Create(pArgument, pCastAs);
  Add(Result);
end;

function TSQLFieldList.AddCoalesce(pArgument: TCustomArgument; pAnotherArguments: Array of TCustomArgument): TCoalesce;
begin
  Result := TCoalesce.Create(pArgument, pAnotherArguments);
  Add(Result);
end;

function TSQLFieldList.AddCount(pArgument: TCustomArgument): TCount;
begin
  Result := TCount.Create(pArgument);
  Add(Result);
end;

function TSQLFieldList.AddCurrencyValue(pValue: Currency): TValueArgument;
begin
  Result := TValueArgument.CreateAsCurrency(pValue);
  Add(Result);
end;

function TSQLFieldList.AddDateTimeValue(pValue: TDateTime): TValueArgument;
begin
  Result := TValueArgument.CreateAsDateTime(pValue);
  Add(Result);
end;

function TSQLFieldList.AddDateValue(pValue: TDate): TValueArgument;
begin
  Result := TValueArgument.CreateAsDate(pValue);
  Add(Result);
end;

function TSQLFieldList.AddDoubleValue(pValue: Double): TValueArgument;
begin
  Result := TValueArgument.CreateAsDouble(pValue);
  Add(Result);
end;

function TSQLFieldList.AddExtendedValue(pValue: Extended): TValueArgument;
begin
  Result := TValueArgument.CreateAsExtended(pValue);
  Add(Result);
end;

function TSQLFieldList.AddField(const pName, pTableAlias, pAlias: String): TFieldArgument;
begin
  Result := TFieldArgument.Create(pName, pTableAlias, pAlias);
  Add(Result);
end;

function TSQLFieldList.AddInt64Value(pValue: Int64): TValueArgument;
begin
  Result := TValueArgument.CreateAsInt64(pValue);
  Add(Result);
end;

function TSQLFieldList.AddIntegerValue(pValue: Integer): TValueArgument;
begin
  Result := TValueArgument.CreateAsInteger(pValue);
  Add(Result);
end;

function TSQLFieldList.AddLiteral(const pLiteralValue: string): TLiteralArgument;
begin
  Result := TLiteralArgument.Create(pLiteralValue);
  Add(Result);
end;

function TSQLFieldList.AddMax(pArgument: TCustomArgument): TMax;
begin
  Result := TMax.Create(pArgument);
  Add(Result);
end;

function TSQLFieldList.AddMin(pArgument: TCustomArgument): TMin;
begin
  Result := TMin.Create(pArgument);
  Add(Result);
end;

function TSQLFieldList.AddSmallintValue(pValue: Smallint): TValueArgument;
begin
  Result := TValueArgument.CreateAsSmallint(pValue);
  Add(Result);
end;

function TSQLFieldList.AddStringValue(pValue: String): TValueArgument;
begin
  Result := TValueArgument.CreateAsString(pValue);
  Add(Result);
end;

function TSQLFieldList.AddSum(pArgument: TCustomArgument): TSum;
begin
  Result := TSum.Create(pArgument);
  Add(Result);
end;

function TSQLFieldList.AddTimeValue(pValue: TTime): TValueArgument;
begin
  Result := TValueArgument.CreateAsTime(pValue);
  Add(Result);
end;

function TSQLFieldList.AddValue<T>(pValue: T): TValueArgument;
begin
  Result := TValueArgument.CreateAs<T>(pValue);
  Add(Result);
end;

constructor TSQLFieldList.Create(pFields: array of TCustomArgument);
var
  I: Integer;
begin
  inherited Create;
  for I := Low(pFields) to High(pFields) do
  begin
    Add(pFields[I]);
  end;
end;

function TSQLFieldList.FindField(const pName, pTableAlias, pAlias: String): TFieldArgument;
var
  lField: TCustomArgument;
  lFieldArgument: TFieldArgument;
  lCanContinue: Boolean;
begin
  Result := nil;
  for lField in Self do
  begin
    lCanContinue := True;
    if lField.InheritsFrom(TFieldArgument) then
    begin
      lFieldArgument := TFieldArgument(lField);
      if (pTableAlias <> '') then
      begin
        lCanContinue := SameText(pTableAlias, lFieldArgument.TableAlias);
      end;
      if (pAlias <> '') and (lCanContinue) then
      begin
        lCanContinue := SameText(pAlias, lFieldArgument.Alias);
      end;
      if SameText(lFieldArgument.Name, pName) and lCanContinue then
      begin
        Result := TFieldArgument(lField);
        Break;
      end;
    end;
  end;
end;

function TSQLFieldList.FieldExists(const pName, pTableAlias, pAlias: String): Boolean;
begin
  Result := FindField(pName, pTableAlias, pAlias) <> nil;
end;

function TSQLFieldList.FieldNotExists(const pName, pTableAlias, pAlias: String): Boolean;
begin
  Result := not FieldExists(pName, pTableAlias, pAlias);
end;

end.
