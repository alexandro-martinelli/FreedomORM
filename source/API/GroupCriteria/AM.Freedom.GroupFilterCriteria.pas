unit AM.Freedom.GroupFilterCriteria;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupFilterCriteria.FilterCriteria;

type
  TGroupFilterCriteria = class(TObjectList<TGroupFilterCriteria>)
  private
    FPolicy: TPolicy;
    FListFilterCriterias: TListFilterCriterias;
    procedure AddCriteriasToList(pListOfCriterias: TList<TFilterCriteria>);
  public
    constructor Create(pPolicy: TPolicy = poAnd);
    destructor Destroy; override;
    function ListOfCriterias: TList<TFilterCriteria>;
    function CompareValues: Boolean;
    function AddGroupFilterCriteria(pPolicy: TPolicy = poAnd): TGroupFilterCriteria;
    procedure AddFilterCriteria(pCriteria: TFilterCriteria); overload;
    procedure AddFilterCriteria(pCriteria: array of TFilterCriteria); overload;
    function IsEmpty: Boolean;
    procedure ClearAll;
    property Policy: TPolicy read FPolicy write FPolicy default poAnd;
    property ListFilterCriterias: TListFilterCriterias read FListFilterCriterias;
  end;


implementation

{ TGroupFilterCriteria }

procedure TGroupFilterCriteria.AddFilterCriteria(pCriteria: TFilterCriteria);
begin
  FListFilterCriterias.AddCriteria(pCriteria);
end;

procedure TGroupFilterCriteria.AddCriteriasToList(pListOfCriterias: TList<TFilterCriteria>);
var
  lCriteria: TFilterCriteria;
  lGroup: TGroupFilterCriteria;
begin
  for lCriteria in FListFilterCriterias do
  begin
    pListOfCriterias.Add(lCriteria);
  end;
  for lGroup in Self do
  begin
    lGroup.AddCriteriasToList(pListOfCriterias);
  end;
end;

procedure TGroupFilterCriteria.AddFilterCriteria(pCriteria: array of TFilterCriteria);
begin
  FListFilterCriterias.AddCriteria(pCriteria);
end;

function TGroupFilterCriteria.AddGroupFilterCriteria(pPolicy: TPolicy): TGroupFilterCriteria;
begin
  Result := TGroupFilterCriteria.Create(pPolicy);
  Add(Result);
end;

procedure TGroupFilterCriteria.ClearAll;
begin
  Clear;
  FListFilterCriterias.Clear;
end;

function TGroupFilterCriteria.CompareValues: Boolean;
var
  lGroup: TGroupFilterCriteria;
  lIndex: Integer;
begin
  Result := FListFilterCriterias.CompareValues(FPolicy);
  if not (Result and (FPolicy = poOr)) then
  begin
    for lIndex := 0 to Count - 1 do
    begin
      lGroup  := Items[lIndex];
      case FPolicy of
        poAnd: Result := Result and lGroup.CompareValues;
        poOr: Result := Result or lGroup.CompareValues;
        poAndNot: Result := Result and (not lGroup.CompareValues);
        poOrNot: Result := Result or (not lGroup.CompareValues);
      end;
      if (Result and (FPolicy = poOr)) then
      begin
        Break;
      end;
    end;
  end;
end;

constructor TGroupFilterCriteria.Create(pPolicy: TPolicy);
begin
  FListFilterCriterias := TListFilterCriterias.Create;
  FPolicy := pPolicy;
end;

destructor TGroupFilterCriteria.Destroy;
begin
  FListFilterCriterias.Free;
  inherited;
end;

function TGroupFilterCriteria.ListOfCriterias: TList<TFilterCriteria>;
begin
  Result := TList<TFilterCriteria>.Create;
  AddCriteriasToList(Result);
end;

function TGroupFilterCriteria.IsEmpty: Boolean;
begin
  Result := (Count = 0) and (FListFilterCriterias.Count = 0);
end;

end.