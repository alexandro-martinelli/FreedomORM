unit AM.Freedom.ObjectMapper.ConstraintMapper;

interface

uses
  System.Generics.Collections,
  AM.Freedom.ObjectMapper.CustomMapper,
  AM.Freedom.EnumerationTypes;

type
  TConstraintMapper = class(TCustomMapper)
  strict private
    FColumns: TList<String>;
    FSchemas: TList<String>;
  strict protected
    function GetConstraintType: TConstraintType; virtual; abstract;
    function GetColumnNames: String; virtual;
  public
    constructor Create(pListOfColumns: TList<String>; pListOfSchemas: TList<String>); virtual;
    destructor Destroy; override;
    property Columns: TList<string> read FColumns;
    property Schemas: TList<String> read FSchemas;
  end;

  TUniqueMapper = class(TConstraintMapper)
  strict protected
    function GetConstraintType: TConstraintType; override;
  end;

  TUniqueMappers = class(TObjectList<TUniqueMapper>)
  public
    function FindUnique(pName: String): TUniqueMapper;
    function FindSchema(pSchemaName: String): TUniqueMapper;
  end;

  TPrimaryMapper = class(TConstraintMapper)
  strict protected
    function GetConstraintType: TConstraintType; override;
  public
    function CreateNew: TPrimaryMapper;
  end;

  TPrimaryMappers = Class(TObjectList<TPrimaryMapper>)
  public
    function FindPrimary(pName: String): TPrimaryMapper;
    function FindSchema(pSchemaName: String): TPrimaryMapper;
  end;

  TForeignMapper = class(TConstraintMapper)
  strict private
    FReferencesTo: String;
    FReferencesColumns: TList<String>;
    FOnUpdate: TForeignOption;
    FOnDelete: TForeignOption;
  strict protected
    function GetConstraintType: TConstraintType; override;
  public
    constructor Create; reintroduce; overload;
    constructor Create(pListOfColumns, pListOfSchemas, pReferencesColumns: TList<string>;
      pReferencesTo: String; pOnUpdate: TForeignOption = NoAction; pOnDelete: TForeignOption = NoAction); reintroduce; overload;
    destructor Destroy; override;
    function CreateNew: TForeignMapper;
    procedure AddReferenceColumn(pColumnName: String);
    procedure Assign(pSource: TForeignMapper);
    property ReferencesTo: String read FReferencesTo write FReferencesTo;
    property ReferencesColumns: TList<String> read FReferencesColumns write FReferencesColumns;
    property OnUpdate: TForeignOption read FOnUpdate write FOnUpdate default NoAction;
    property OnDelete: TForeignOption read FOnDelete write FOnDelete default NoAction;
  end;

  TForeignMappers = Class(TObjectList<TForeignMapper>)
  public
    function FindForeign(pName: String): TForeignMapper;
    function FindSchema(pSchemaName: String): TForeignMapper;
  end;




implementation

uses
  System.StrUtils, System.SysUtils;

{ TUniqueMapper }

constructor TConstraintMapper.Create(pListOfColumns: TList<String>; pListOfSchemas: TList<String>);
var
  lName: String;
begin
  FColumns := TList<String>.Create;
  FSchemas := TList<String>.Create;
  if (Assigned(pListOfColumns)) then
  begin
    for lName in pListOfColumns do
    begin
      FColumns.Add(lName);
    end;
  end;
  if (Assigned(pListOfSchemas)) then
  begin
    for lName in pListOfSchemas do
    begin
      FSchemas.Add(LowerCase(lName));
    end;
  end;
  case GetConstraintType of
    PrimaryKey: Name := 'PK_%s';
    UniqueKey: Name := 'UQ_%s_' + GetColumnNames;
  end;
end;

destructor TConstraintMapper.Destroy;
begin
  FColumns.Free;
  FSchemas.Free;
  inherited;
end;

function TConstraintMapper.GetColumnNames: String;
var
  lColumnName: string;
begin
  Result := '';
  for lColumnName in FColumns do
  begin
    Result := Result + ifthen(Result <> '', '_') + lColumnName;
  end;
end;

{ TUniqueMapper }

function TUniqueMapper.GetConstraintType: TConstraintType;
begin
  Result := UniqueKey;
end;

{ TPrimaryMapper }

function TPrimaryMapper.CreateNew: TPrimaryMapper;
begin
  Result := TPrimaryMapper.Create(Self.Columns, Self.Schemas);
end;

function TPrimaryMapper.GetConstraintType: TConstraintType;
begin
  Result := PrimaryKey;
end;

{ TPrimaryMappers }

function TPrimaryMappers.FindPrimary(pName: String): TPrimaryMapper;
var
  lPrimary: TPrimaryMapper;
begin
  Result := nil;
  for lPrimary in self do
  begin
    if SameText(lPrimary.Name, pName) then
    begin
      Result := lPrimary;
      Break;
    end;
  end;
end;

function TPrimaryMappers.FindSchema(pSchemaName: String): TPrimaryMapper;
var
  lPrimary: TPrimaryMapper;
  lSchema: string;
begin
  Result := nil;
  for lPrimary in self do
  begin
    for lSchema in lPrimary.Schemas do
    begin
      if SameText(lSchema, pSchemaName) then
      begin
        Result := lPrimary;
        Break;
      end;
    end;
    if Assigned(Result) then
    begin
      Break;
    end;
  end;
end;

{ TUniqueMappers }

function TUniqueMappers.FindUnique(pName: String): TUniqueMapper;
var
  lUnique: TUniqueMapper;
begin
  Result := nil;
  for lUnique in self do
  begin
    if SameText(lUnique.Name, pName) then
    begin
      Result := lUnique;
      Break;
    end;
  end;
end;

function TUniqueMappers.FindSchema(pSchemaName: String): TUniqueMapper;
var
  lUnique: TUniqueMapper;
  lSchema: string;
begin
  Result := nil;
  for lUnique in self do
  begin
    for lSchema in lUnique.Schemas do
    begin
      if SameText(lSchema, pSchemaName) then
      begin
        Result := lUnique;
        Break;
      end;
    end;
    if Assigned(Result) then
    begin
      Break;
    end;
  end;
end;

{ TForeignMapper }

procedure TForeignMapper.AddReferenceColumn(pColumnName: String);
begin
  if (not FReferencesColumns.Contains(pColumnName)) then
  begin
    FReferencesColumns.Add(pColumnName);
  end;
end;

procedure TForeignMapper.Assign(pSource: TForeignMapper);
var
  lName: String;
begin
  Columns.Clear;
  Schemas.Clear;
  FReferencesColumns.Clear;
  for lName in pSource.Columns do
  begin
    Columns.Add(lName);
  end;
  for lName in pSource.Schemas do
  begin
    Schemas.Add(LowerCase(lName));
  end;
  FReferencesTo := pSource.ReferencesTo;
  for lName in pSource.ReferencesColumns do
  begin
    FReferencesColumns.Add(lName);
  end;
  Name := pSource.Name;
end;

constructor TForeignMapper.Create(pListOfColumns, pListOfSchemas, pReferencesColumns: TList<string>;
  pReferencesTo: String; pOnUpdate, pOnDelete: TForeignOption);
var
  lName: String;
begin
  inherited Create(pListOfColumns, pListOfSchemas);
  FReferencesColumns := TList<String>.Create;
  FOnUpdate := pOnUpdate;
  FOnDelete := pOnDelete;
  FReferencesTo := pReferencesTo;
  for lName in pReferencesColumns do
  begin
    FReferencesColumns.Add(lName);
  end;
  Name := 'FK_%s_' + pReferencesTo;
end;

constructor TForeignMapper.Create;
begin
  inherited Create(nil, nil);
  FReferencesColumns := TList<String>.Create;
  FOnUpdate := NoAction;
  FOnDelete := NoAction;
end;

function TForeignMapper.CreateNew: TForeignMapper;
begin
  Result := TForeignMapper.Create(Columns, Schemas, FReferencesColumns, FReferencesTo, FOnUpdate, FOnDelete);
end;

destructor TForeignMapper.Destroy;
begin
  FReferencesColumns.Free;
  inherited;
end;

function TForeignMapper.GetConstraintType: TConstraintType;
begin
  Result := TConstraintType.ForeignKey;
end;

{ TForeignMappers }

function TForeignMappers.FindForeign(pName: String): TForeignMapper;
var
  lForeign: TForeignMapper;
begin
  Result := nil;
  for lForeign in self do
  begin
    if SameText(lForeign.Name, pName) then
    begin
      Result := lForeign;
      Break;
    end;
  end;
end;

function TForeignMappers.FindSchema(pSchemaName: String): TForeignMapper;
var
  lForeign: TForeignMapper;
begin
  Result := nil;
  for lForeign in self do
  begin
    if lForeign.Schemas.Contains(LowerCase(pSchemaName)) then
    begin
      Result := lForeign;
      Break;
    end;
  end;
end;

end.
