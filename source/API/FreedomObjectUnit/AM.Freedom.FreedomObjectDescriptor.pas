unit AM.Freedom.FreedomObjectDescriptor;

interface

uses
  System.Generics.Collections,
  AM.Freedom.ObjectMapper,
  AM.Freedom.ObjectMapper.CustomColumnMapper;

type
  TFreedomClassDescriptor = class;

  TFieldPropertyDescriptor = class
  private
    FFieldMapper: TCustomColumnMapper;
    FPropertyMapper: TCustomColumnMapper;
    FTypeName: string;
    FDetailClassListName: String;
    FTypeNameUnit: String;
    FHideNullable: Boolean;
    FUseNullableTypes: Boolean;
    FExtension: TFreedomClassDescriptor;
    function GetIsExtension: Boolean;
    procedure SetExtension(const Value: TFreedomClassDescriptor);
  public
    constructor Create;
    destructor Destroy; override;
    property FieldMapper: TCustomColumnMapper read FFieldMapper write FFieldMapper;
    property PropertyMapper: TCustomColumnMapper read FPropertyMapper write FPropertyMapper;
    property TypeName: string read FTypeName write FTypeName;
    property TypeNameUnit: String read FTypeNameUnit write FTypeNameUnit;
    property LazyClassName: String read FDetailClassListName write FDetailClassListName;
    property HideNullable: Boolean read FHideNullable write FHideNullable;
    property UseNullableTypes: Boolean read FUseNullableTypes write FUseNullableTypes;
    property Extension: TFreedomClassDescriptor read FExtension write SetExtension;
    property IsExtension: Boolean read GetIsExtension;
  end;

  TFieldPropertyList = class(TObjectList<TFieldPropertyDescriptor>)
  public
    function ContainsProperty(pPropertyName: String): Boolean;
    function ContainsField(pFieldName: String): Boolean;
  end;

  TFreedomClassDescriptor = class
  private
    FBaseClass: string;
    FNewClassName: string;
    FObjectMapper: TObjectMapper;
    FFieldPropertyList: TFieldPropertyList;
    FListClassName: String;
    FBaseClassListName: String;
    FNewBaseClassUnit: string;
    FBaseClassListUnit: string;
    FObjectCursorClassName: String;
    FInheritsObjectCursorClassName: String;
    FObjectCursorClassUnitName: String;
    function GetCompleteClassName: string;
    function GetCompleteListClassName: String;
    procedure SetObjectMapper(const Value: TObjectMapper);
    function GetIsEntity: Boolean;
    function GetIsCursor: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property NewBaseClass: string read FBaseClass write FBaseClass;
    property NewClassName: string read FNewClassName write FNewClassName;
    property NewBaseClassUnit: string read FNewBaseClassUnit write FNewBaseClassUnit;
    property BaseClassListUnit: string read FBaseClassListUnit write FBaseClassListUnit;
    property CompleteClassName: string read GetCompleteClassName;
    property ListClassName: String read FListClassName write FListClassName;
    property BaseClassListName: String read FBaseClassListName write FBaseClassListName;
    property CompleteListClassName: String read GetCompleteListClassName;
    property ObjectMapper: TObjectMapper read FObjectMapper write SetObjectMapper;
    property FieldPropertyList: TFieldPropertyList read FFieldPropertyList;
    property ObjectCursorClassName: String read FObjectCursorClassName write FObjectCursorClassName;
    property ObjectCursorClassUnitName: String read FObjectCursorClassUnitName write FObjectCursorClassUnitName;
    property InheritsObjectCursorClassName: String read FInheritsObjectCursorClassName write FInheritsObjectCursorClassName;
    property IsEntity: Boolean read GetIsEntity;
    property IsCursor: Boolean read GetIsCursor;
  end;

  TFreedomClassDescriptorList = class(TObjectList<TFreedomClassDescriptor>);

  TFreedomUnitDescriptor = class(TFreedomClassDescriptorList)
  private
    FNewUnitName: String;
  public
    property NewUnitName: String read FNewUnitName write FNewUnitName;
  end;

  TFreedomProjectDescriptor = class(TObjectList<TFreedomUnitDescriptor>)
  public
    procedure AddUnit(pUnitName: String);
    function Contains(pUnitName: String): Boolean;
    function FindUnit(pUnitName: String): TFreedomUnitDescriptor;
  end;

implementation

uses
  System.SysUtils;

{ TFreedomObjectDescriptor }

constructor TFreedomClassDescriptor.Create;
begin
  FBaseClass := 'TFreedomObject';
  FObjectMapper := TObjectMapper.Create;
  FFieldPropertyList := TFieldPropertyList.Create;
end;

destructor TFreedomClassDescriptor.Destroy;
begin
  FObjectMapper.Free;
  FFieldPropertyList.Free;
  inherited;
end;

function TFreedomClassDescriptor.GetCompleteClassName: string;
begin
  Result := FNewClassName + ' = class(' + FBaseClass + ')';
end;
function TFreedomClassDescriptor.GetCompleteListClassName: String;
begin
  if FListClassName <> '' then
  begin
    Result := Format('%s = class(%s<%s>)', [FListClassName, FBaseClassListName, FNewClassName]);
  end;
end;

function TFreedomClassDescriptor.GetIsCursor: Boolean;
begin
  Result := FObjectCursorClassName <> '';
end;

function TFreedomClassDescriptor.GetIsEntity: Boolean;
begin
  Result:= FNewClassName <> '';
end;

procedure TFreedomClassDescriptor.SetObjectMapper(const Value: TObjectMapper);
begin
  if FObjectMapper <> Value then
  begin
    FreeAndNil(FObjectMapper);
  end;
  FObjectMapper := Value;
end;

{ TFieldPropertyDescriptor }

constructor TFieldPropertyDescriptor.Create;
begin
  FExtension := TFreedomClassDescriptor.Create;
end;

destructor TFieldPropertyDescriptor.Destroy;
begin
  FPropertyMapper.Free;
  FExtension.Free;
  inherited;
end;

function TFieldPropertyDescriptor.GetIsExtension: Boolean;
begin
  Result := FExtension.NewClassName <> '';
end;

procedure TFieldPropertyDescriptor.SetExtension(const Value: TFreedomClassDescriptor);
begin
  if (Assigned(FExtension)) then
  begin
    FExtension.Free;
  end;
  FExtension := Value;
end;

{ TFieldPropertyList }

function TFieldPropertyList.ContainsField(pFieldName: String): Boolean;
var
  lFieldProp: TFieldPropertyDescriptor;
begin
  Result := False;
  for lFieldProp in Self do
  begin
    if Assigned(lFieldProp.FieldMapper) then
    begin
      Result := SameText(pFieldName, lFieldProp.FieldMapper.Name);
      if (Result) then
      begin
        Break;
      end;
    end;
  end;
end;

function TFieldPropertyList.ContainsProperty(pPropertyName: String): Boolean;
var
  lFieldProp: TFieldPropertyDescriptor;
begin
  Result := False;
  for lFieldProp in Self do
  begin
    if Assigned(lFieldProp.PropertyMapper) then
    begin
      Result := SameText(pPropertyName, lFieldProp.PropertyMapper.Name);
      if (Result) then
      begin
        Break;
      end;
    end;
  end;
end;

{ TFreedomProjectDescriptor }

procedure TFreedomProjectDescriptor.AddUnit(pUnitName: String);
var
  lUnit: TFreedomUnitDescriptor;
begin
  if (not Contains(pUnitName)) then
  begin
    lUnit := TFreedomUnitDescriptor.Create;
    lUnit.NewUnitName := pUnitName;
    Add(lUnit);
  end;
end;

function TFreedomProjectDescriptor.Contains(pUnitName: String): Boolean;
var
  lUnit: TFreedomUnitDescriptor;
begin
  Result := False;
  for lUnit in Self do
  begin
    Result := SameText(lUnit.NewUnitName, pUnitName);
    if (Result) then
    begin
      Break;
    end;
  end;
end;

function TFreedomProjectDescriptor.FindUnit(pUnitName: String): TFreedomUnitDescriptor;
var
  lUnit: TFreedomUnitDescriptor;
begin
  Result := nil;
  for lUnit in Self do
  begin
    if SameText(lUnit.NewUnitName, pUnitName) then
    begin
      Result := lUnit;
      Break;
    end;
  end;
end;

end.
