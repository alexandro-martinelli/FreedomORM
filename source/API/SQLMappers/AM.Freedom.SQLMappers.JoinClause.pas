unit AM.Freedom.SQLMappers.JoinClause;

interface

uses
  System.Generics.Collections,
  AM.Freedom.SQLMappers.AliasableObject,
  AM.Freedom.GroupCriteria,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLMappers.SQLClause,
  AM.Freedom.SQLMappers.IJoin, AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.GroupCriteria.Criteria;


type
  TJoin = class(TSQLClause, IJoin)
  strict private
    FJoinOn: TGroupCriteria;
    FKind: TJoinKind;
  strict
  private
    function GetAlias: String; protected
    function GetJoinOn: TGroupCriteria;
    function GetJoinKind: TJoinKind;
    procedure SetJoinKind(const pKind: TJoinKind);
    procedure SetArgument(const pArgument: TCustomArgument); override;
  public
    constructor Create; overload;
    constructor Create(const pArgument: TCustomArgument; pJoinOn: Array of TCriteria; const pJoinKind: TJoinKind = jkJoin); overload;
    destructor Destroy; override;
    function JoinTable(pTableName: String; pAlias: String = ''; pSchemaName: string = ''): TJoin;
    property JoinOn: TGroupCriteria read GetJoinOn;
    property Kind: TJoinKind read FKind write FKind;
    property Argument;
    property Alias: String read GetAlias;

  end;

  TJoinClause = class(TObjectList<TJoin>)
  private
    function LeftArgumentIsNamed(pColumnName: String; pJoin: TJoin): Boolean;
    function RigthArgumentIsNamed(pColumnName: String; pJoin: TJoin): Boolean;
  protected
    function GetLeftArgumentName(pJoin: TJoin): String;
    function GetRigthArgumentName(pJoin: TJoin): String;
  public
    constructor Create(pJoins: Array of TJoin); overload;
    function FindJoin(pRefObjectName: String; pRefColumnName: string = ''; pJoinColumnName: string = ''): TJoin;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.SQLMappers.SelectClause,
  AM.Freedom.SQLMappers.Arguments;

{ TJoin }

constructor TJoin.Create(const pArgument: TCustomArgument; pJoinOn: Array of TCriteria; const pJoinKind: TJoinKind);
var
  I: Integer;
begin
  Create;
  FKind := pJoinKind;
  Argument := pArgument;
  for I := Low(pJoinOn) to High(pJoinOn) do
  begin
    FJoinOn.ListCriterias.Add(pJoinOn[I]);
  end;
end;

destructor TJoin.Destroy;
begin
  FreeAndNil(FJoinOn);
  inherited;
end;

constructor TJoin.Create;
begin
  FKind := jkJoin;
  FJoinOn := TGroupCriteria.Create;
end;

function TJoin.GetAlias: String;
begin
  Result := '';
  if (Assigned(Argument)) then
  begin
    if (Argument.InheritsFrom(TSelectClause)) then
    begin
      Result := TSelectClause(Argument).Alias;
    end
    else if (Argument.InheritsFrom(TTableArgument)) then
    begin
      Result := TTableArgument(Argument).Alias;
    end;
  end;
end;

function TJoin.GetJoinKind: TJoinKind;
begin
  Result := FKind;
end;

function TJoin.GetJoinOn: TGroupCriteria;
begin
  Result := FJoinOn;
end;

function TJoin.JoinTable(pTableName, pAlias, pSchemaName: string): TJoin;
begin
  SetArgument(TTableArgument.Create(pTableName, pAlias, pSchemaName));
  Result := Self;
end;

procedure TJoin.SetArgument(const pArgument: TCustomArgument);
begin
  if (pArgument.ClassType = TSelectClause) or (pArgument.ClassType = TTableArgument) then
  begin
    inherited;
  end else
  begin
    raise Exception.Create('Argument type ' + pArgument.ClassName + ' is not valid for TFromClause');
  end;
end;

procedure TJoin.SetJoinKind(const pKind: TJoinKind);
begin
  FKind := pKind;
end;

{ TJoinClause }

constructor TJoinClause.Create(pJoins: array of TJoin);
var
  I: Integer;
begin
  inherited Create;
  for I := Low(pJoins) to High(pJoins) do
  begin
    Add(pJoins[I]);
  end;
end;

function TJoinClause.FindJoin(pRefObjectName: String; pRefColumnName, pJoinColumnName: string): TJoin;
var
  lJoin: TJoin;
begin
  Result := nil;
  for lJoin in Self do
  begin
    if lJoin.Argument.ClassType = TTableArgument then
    begin
      if SameText(TTableArgument(lJoin.Argument).Name, pRefObjectName)  then
      begin
        if (LeftArgumentIsNamed(pRefColumnName, lJoin) and
           RigthArgumentIsNamed(pJoinColumnName, lJoin)) or
           (LeftArgumentIsNamed(pJoinColumnName, lJoin) and
           RigthArgumentIsNamed(pRefColumnName, lJoin)) then
        begin
        Result := lJoin;
        Break;
        end;
      end;
    end;
  end;
end;

function TJoinClause.GetLeftArgumentName(pJoin: TJoin): String;
var
  lCriteria: TCriteria;
begin
  Result := '';
  lCriteria := pJoin.JoinOn.ListCriterias.First;
  if Assigned(lCriteria) then
  begin
    if lCriteria.LeftArgument.ClassType = TFieldArgument then
    begin
      Result := TFieldArgument(lCriteria.LeftArgument).Name;
    end;
  end;
end;

function TJoinClause.GetRigthArgumentName(pJoin: TJoin): String;
var
  lCriteria: TCriteria;
begin
  Result := '';
  lCriteria := pJoin.JoinOn.ListCriterias.First;
  if Assigned(lCriteria) then
  begin
    if lCriteria.RigthArgument.ClassType = TFieldArgument then
    begin
      Result := TFieldArgument(lCriteria.RigthArgument).Name;
    end;
  end;
end;

function TJoinClause.LeftArgumentIsNamed(pColumnName: String; pJoin: TJoin): Boolean;
var
  lName: String;
begin
  Result := pColumnName = '';
  if (not Result) then
  begin
    lName := GetLeftArgumentName(pJoin);
    Result := lName = pColumnName;
  end;
end;

function TJoinClause.RigthArgumentIsNamed(pColumnName: String; pJoin: TJoin): Boolean;
var
  lCriteria: TCriteria;
begin
  Result := pColumnName = '';
  if (not Result) then
  begin
    lCriteria := pJoin.JoinOn.ListCriterias.First;
    Result := not Assigned(lCriteria);
    if (not Result) then
    begin
      Result := lCriteria.RigthArgument.ClassType <> TFieldArgument;
      if (not Result) then
      begin
        Result := TFieldArgument(lCriteria.RigthArgument).Name = pColumnName;
      end;
    end;
  end;
end;

end.
