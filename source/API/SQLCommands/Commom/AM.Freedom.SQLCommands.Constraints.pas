unit AM.Freedom.SQLCommands.Constraints;

interface

uses
  System.Generics.Collections,
  AM.Freedom.SQLMappers.NamedObject,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.EnumerationTypes;

type
  TFieldList = class(TList<String>)
  public
    function FindField(pFieldName: String): Boolean;
  end;

  TConstraintClass = class of TCustomConstraint;

  TCustomConstraint = class abstract(TNamedObject)
  strict private
    FFields: TFieldList;
  strict protected
    function GetConstraintType: TConstraintType; virtual; abstract;
  public
    constructor Create; overload; virtual;
    constructor Create(const pName: String; const pFields: Array of String); overload; virtual;
    destructor Destroy; override;
    function CreateNew: TCustomConstraint; virtual;
    procedure AddField(pFieldName: String);
    property Name;
    property Fields: TFieldList read FFields;
    property ConstraintType: TConstraintType read GetConstraintType;
  end;

  TPrimaryKey = class(TCustomConstraint)
  strict protected
    function GetConstraintType: TConstraintType; override;
  end;

  TUniqueKey = class(TCustomConstraint)
  strict protected
    function GetConstraintType: TConstraintType; override;
  end;

  TForeignKey = class(TCustomConstraint)
  strict private
    FReferences: String;
    FReferenceFields: TFieldList;
    FOnUpdate: TForeignOption;
    FOnDelete: TForeignOption;
    procedure CreateReferenceFields;
  strict protected
    function GetConstraintType: TConstraintType; override;
  public
    constructor Create; overload; override;
    constructor Create(const pName: String; const pFields: Array of String; const pReferences: String; const pReferenceFields: Array of String;
        const pOnUpdate: TForeignOption = NoAction; const pOnDelete: TForeignOption = NoAction); reintroduce; overload;
    destructor Destroy; override;
    function CreateNew: TCustomConstraint; override;

    function ReferencesTo(const pTableName: String): TForeignKey;
    function ReferencesFields(const pNames: Array of String): TForeignKey;
    function Update(const pOption: TForeignOption): TForeignKey;
    function Delete(const pOption: TForeignOption): TForeignKey;

    property References: String read FReferences write FReferences;
    property ReferenceFields: TFieldList read FReferenceFields;
    property OnUpdate: TForeignOption read FOnUpdate write FOnUpdate default NoAction;
    property OnDelete: TForeignOption read FOnDelete write FOnDelete default NoAction;
  end;



implementation

uses
  System.SysUtils;

{ TCustomConstraint }

constructor TCustomConstraint.Create;
begin
  FFields := TFieldList.Create;
end;

procedure TCustomConstraint.AddField(pFieldName: String);
begin
  if not FFields.Contains(pFieldName) then
  begin
    FFields.Add(pFieldName);
  end;
end;

constructor TCustomConstraint.Create(const pName: String; const pFields: array of String);
var
  I: Integer;
begin
  Create;
  Name := pName;
  for I := Low(pFields) to High(pFields) do
  begin
    if not FFields.Contains(pFields[I]) then
    begin
      FFields.Add(pFields[I]);
    end;
  end;
end;

function TCustomConstraint.CreateNew: TCustomConstraint;
var
  lField: String;
begin
  Result := TConstraintClass(Self.ClassType).Create;
  Result.Name := Name;
  for lField in Self.Fields do
  begin
    Result.Fields.Add(lField);
  end;
end;

destructor TCustomConstraint.Destroy;
begin
  FreeAndNil(FFields);
  inherited;
end;

{ TForeignKey }

constructor TForeignKey.Create;
begin
  inherited;
  CreateReferenceFields;
  FOnUpdate := TForeignOption.NoAction;
  FOnDelete := TForeignOption.NoAction;
end;

constructor TForeignKey.Create(const pName: String; const pFields: array of String; const pReferences: String;
  const pReferenceFields: array of String; const pOnUpdate, pOnDelete: TForeignOption);
var
  I: Integer;
begin
  inherited Create(pName, pFields);
  FReferences := pReferences;
  CreateReferenceFields;
  FOnUpdate := pOnUpdate;
  FOnDelete := pOnDelete;
  for I := Low(pReferenceFields) to High(pReferenceFields) do
  begin
    if not FReferenceFields.Contains(pReferenceFields[I]) then
    begin
      FReferenceFields.Add(pReferenceFields[I]);
    end;
  end;
end;

function TForeignKey.CreateNew: TCustomConstraint;
var
  lField: String;
begin
  Result := Inherited;
  TForeignKey(Result).ReferencesTo(Self.References);
  for lField in Self.ReferenceFields do
  begin
    TForeignKey(Result).ReferenceFields.Add(lField);
  end;
  TForeignKey(Result).Update(Self.OnUpdate).Delete(Self.OnDelete);
end;

function TForeignKey.Delete(const pOption: TForeignOption): TForeignKey;
begin
  Result := Self;
  FOnDelete := pOption;
end;

procedure TForeignKey.CreateReferenceFields;
begin
  if not Assigned(FReferenceFields) then
  begin
    FReferenceFields := TFieldList.Create;
  end;
end;

destructor TForeignKey.Destroy;
begin
  FreeAndNil(FReferenceFields);
  inherited;
end;

function TForeignKey.GetConstraintType: TConstraintType;
begin
  Result := TConstraintType.ForeignKey;
end;

function TForeignKey.ReferencesFields(const pNames: array of String): TForeignKey;
var
  I: Integer;
begin
  Result := Self;
  for I := Low(pNames) to High(pNames) do
  begin
    FReferenceFields.Add(pNames[I]);
  end;
end;

function TForeignKey.ReferencesTo(const pTableName: String): TForeignKey;
begin
  Result := Self;
  FReferences := pTableName;
end;

function TForeignKey.Update(const pOption: TForeignOption): TForeignKey;
begin
  Result := Self;
  FOnUpdate := pOption;
end;

{ TPrimaryKey }

function TPrimaryKey.GetConstraintType: TConstraintType;
begin
  Result := TConstraintType.PrimaryKey;
end;

{ TFieldList }

function TFieldList.FindField(pFieldName: String): Boolean;
var
  lFieldName: String;
begin
  Result := False;
  for lFieldName in Self do
  begin
    Result := SameText(lFieldName, pFieldName);
    if Result then
    begin
      Break;
    end;
  end;
end;

{ TUniqueKey }

function TUniqueKey.GetConstraintType: TConstraintType;
begin
  Result := UniqueKey;
end;

end.
