unit AM.Freedom.GroupCriteria;

interface

uses
  AM.Freedom.InterfacedObjects,
  System.Generics.Collections,
  AM.Freedom.GroupCriteria.IGroupCriteria,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria.Criteria,
  System.JSON;

type
  TGroupCriteria = class(TInterfacedObjectList<TGroupCriteria>, IGroupCriteria)
  strict private
    FPolicy: TPolicy;
    FLimitRows: UInt32;
    FCriterias: TListCriterias;
    FSchemaName: String;
    FCannotBeDestroyed: Boolean;
    procedure Initialize;
  strict protected
    function GetPolicy: TPolicy; virtual;
    procedure SetPolicy(const pPolicy: TPolicy); virtual;
    function GetLimitRows: UInt32; virtual;
    procedure SetLimitRows(const pLimitRows: UInt32); virtual;
    function GetCriterias: TListCriterias; virtual;
  protected
    procedure Notify(const Value: TGroupCriteria; Action: TCollectionNotification); override;
  public
    constructor Create(pPolicy: TPolicy = poAnd; pLimitRows: UInt32 = 0); reintroduce;
    destructor Destroy; override;
    procedure AddCriteria(pCriteria: TCriteria); overload;
    procedure AddCriteria(pCriterias: Array of TCriteria); overload;
    function AddGroupCriteria(pPolicy: TPolicy = poAnd; pLimitRows: UInt32 = 0): TGroupCriteria;
    procedure ChangeAlias(pAlias: String; pMode: TChangeAliasMode = cmEmptys);
    function IsEmpty: Boolean;
    function ToJSON: TJSONArray;
    procedure FromJSON(pJSONArray: TJSONArray);
    property Policy: TPolicy read GetPolicy write SetPolicy default poAnd;
    property LimitRows: UInt32 read GetLimitRows write SetLimitRows default 0;
    property SchemaName: String read FSchemaName write FSchemaName;
    property ListCriterias: TListCriterias read GetCriterias;
    property CannotBeDestroyed: Boolean read FCannotBeDestroyed write FCannotBeDestroyed default False;
  end;

implementation

uses
  System.SysUtils,
  System.Rtti,
  System.Variants,
  AM.Freedom.SQLMappers.Arguments,
  Data.DBXJSONReflect,
  System.StrUtils;

{ TGroupCriteria }

procedure TGroupCriteria.AddCriteria(pCriteria: TCriteria);
begin
  FCriterias.Add(pCriteria);
end;

procedure TGroupCriteria.AddCriteria(pCriterias: array of TCriteria);
begin
  FCriterias.AddRange(pCriterias);
end;

function TGroupCriteria.AddGroupCriteria(pPolicy: TPolicy; pLimitRows: UInt32): TGroupCriteria;
begin
  Result := TGroupCriteria.Create(pPolicy, pLimitRows);
  Add(Result);
end;

procedure TGroupCriteria.Initialize;
begin
  if (Assigned(FCriterias)) then
  begin
    FreeAndNil(FCriterias);
  end;
  FCriterias := TListCriterias.Create;
  FPolicy := poAnd;
  FLimitRows := 0;
end;

function TGroupCriteria.IsEmpty: Boolean;
var
  lGroupCriteria: TGroupCriteria;
begin
  Result := True;
  for lGroupCriteria in Self do
  begin
    Result := lGroupCriteria.IsEmpty;
    if (not Result) then
    begin
      Break;
    end;
  end;
  Result := (Result) and (FCriterias.Count <= 0);
end;

procedure TGroupCriteria.Notify(const Value: TGroupCriteria; Action: TCollectionNotification);
begin
  inherited;
  if (Action = cnAdded) then
  begin
    Value.RegisterFreeNotification(Self);
  end;
end;

procedure TGroupCriteria.ChangeAlias(pAlias: String; pMode: TChangeAliasMode);
var
  lCriteria: TCriteria;
  lGroup: TGroupCriteria;
begin
  for lCriteria in FCriterias do
  begin
    if (lCriteria.LeftArgument.InheritsFrom(TFieldArgument)) and ((TFieldArgument(lCriteria.LeftArgument).TableAlias = '') or (pMode = cmAll)) then
    begin
      if not Containstext(TFieldArgument(lCriteria.LeftArgument).Name, '.') then
      begin
        TFieldArgument(lCriteria.LeftArgument).TableAlias := pAlias;
      end;
    end;
  end;
  for lGroup in Self do
  begin
    lGroup.ChangeAlias(pAlias, pMode);
  end;
end;

constructor TGroupCriteria.Create(pPolicy: TPolicy; pLimitRows: UInt32);
begin
  inherited Create(True);
  Initialize;
  FLimitRows := pLimitRows;
  FPolicy := pPolicy;
  FCannotBeDestroyed := False;
end;

destructor TGroupCriteria.Destroy;
var
  lGroup: TGroupCriteria;
  lIndex: Integer;
begin
  for lIndex := Count - 1 downto 0 do
  begin
    lGroup := Items[lIndex];
    if (lGroup.CannotBeDestroyed) then
    begin
      Extract(lGroup);
    end;
  end;
  FreeAndNil(FCriterias);
  inherited;
end;

procedure TGroupCriteria.FromJSON(pJSONArray: TJSONArray);
var
  lCriterias: TJSONArray;
  lCriteria: TCriteria;
  lGroupCriteria: TGroupCriteria;
  lJSON: TJSONValue;
  lIndex: Integer;
begin
  Clear;
  FCriterias.Clear;
  lJSON := pJSONArray.Remove(0);
  lJSON.Free;
  FPolicy := TPolicy(TJSONNumber(pJSONArray.Items[0]).AsInt);
  FLimitRows := TJSONNumber(pJSONArray.Items[1]).AsInt;
  FSchemaName := pJSONArray.Items[2].Value;
  FCannotBeDestroyed := (pJSONArray.Items[3] is TJSONTrue);
  if (pJSONArray.Items[4] is TJSONArray) then
  begin
    lCriterias := TJSONArray(pJSONArray.Items[4]);
    if (lCriterias.Items[0].Value = TCriteria.QualifiedClassName) then
    begin
      lJSON := lCriterias.Remove(0);
      lJSON.Free;
      for lIndex := 0 to lCriterias.Count do
      begin
        lCriteria := TCriteria.FromJSON<TCriteria>(lCriterias.Items[lIndex]);
        FCriterias.Add(lCriteria);
      end;
    end;
    lJSON := pJSONArray.Remove(4);
    lJSON.Free;
  end;
  if (pJSONArray.Items[4] is TJSONArray) then
  begin
    lCriterias := TJSONArray(pJSONArray.Items[4]);
    if (lCriterias.Items[0].Value = TGroupCriteria.QualifiedClassName) then
    begin
      for lIndex := 0 to lCriterias.Count - 1 do
      begin
        lGroupCriteria := TGroupCriteria.Create;
        lGroupCriteria.FromJSON(TJSONArray(lCriterias.Items[lIndex]));
        Add(lGroupCriteria);
      end;
    end;
  end;
end;

function TGroupCriteria.GetCriterias: TListCriterias;
begin
  Result := FCriterias;
end;

function TGroupCriteria.GetLimitRows: UInt32;
begin
  Result := FLimitRows;
end;

function TGroupCriteria.GetPolicy: TPolicy;
begin
  Result := FPolicy;
end;

procedure TGroupCriteria.SetLimitRows(const pLimitRows: UInt32);
begin
  FLimitRows := pLimitRows;
end;

procedure TGroupCriteria.SetPolicy(const pPolicy: TPolicy);
begin
  FPolicy := pPolicy;
end;

function TGroupCriteria.ToJSON: TJSONArray;
var
  lCriterias: TJSONArray;
  lCriteria: TCriteria;
  lGroupCriteria: TGroupCriteria;
begin
  Result := TJSONArray.Create;
  Result.Add(Self.QualifiedClassName);
  Result.Add(Ord(FPolicy));
  Result.Add(FLimitRows);
  Result.Add(FSchemaName);
  Result.Add(FCannotBeDestroyed);
  if (FCriterias.Count > 0) then
  begin
    lCriterias := TJSONArray.Create;
    Result.AddElement(lCriterias);
    lCriterias.Add(FCriterias.QualifiedClassName);
    for lCriteria in FCriterias do
    begin
      lCriterias.AddElement(lCriteria.ToJSON);
    end;
  end;
  if (Count > 0) then
  begin
    lCriterias := TJSONArray.Create;
    Result.AddElement(lCriterias);
    for lGroupCriteria in Self do
    begin
      lCriterias.AddElement(lGroupCriteria.ToJSON);
    end;
  end;
end;

end.


