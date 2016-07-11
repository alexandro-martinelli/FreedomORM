unit AM.Freedom.TableSchemas;

interface

uses
  System.Generics.Collections,
  AM.Freedom.EnumerationTypes,
  System.Classes;

type
  TTableSchema = class
  private
    FTableName: String;
    FTableSchema: String;
  public
    property TableName: String read FTableName write FTableName;
    property TableSchema: String read FTableSchema write FTableSchema;
  end;

  TCapitalizationMode = (cmCamelCase, cmLowerCase, cmUpperCase, cmDontCapitalize);

  TParameters = class
  private
    FDataBaseType: TConnectorType;
    FConnectionParams: TStrings;
    FMappingCharCase: TCharCase;
    FSchemaCharCase: TCharCase;
    FRemoveUnderscoreInFields: Boolean;
    FRemoveUnderscoreInClass: Boolean;
    FCapitalizationMode: TCapitalizationMode;
  public
    constructor Create;
    destructor Destroy; override;
    property DataBaseType: TConnectorType read FDataBaseType write FDataBaseType;
    property ConnectionParams: TStrings read FConnectionParams write FConnectionParams;
    property MappingCharCase: TCharCase read FMappingCharCase write FMappingCharCase;
    property SchemaCharCase: TCharCase read FSchemaCharCase write FSchemaCharCase;
    property RemoveUnderscoreInFields: Boolean read FRemoveUnderscoreInFields write FRemoveUnderscoreInFields;
    property RemoveUnderscoreInClass: Boolean read FRemoveUnderscoreInClass write FRemoveUnderscoreInClass;
    property CapitalizationMode: TCapitalizationMode read FCapitalizationMode write FCapitalizationMode;
  end;

  TTableSchemaList = class(TObjectList<TTableSchema>)
  private
    FParameters: TParameters;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AddTableSchema(pTableName, pSchemaName: String);
    function FindTableSchema(pTableName, pTableSchema: String): TTableSchema;
    property Parameters: TParameters read FParameters write FParameters;
  end;

implementation

uses
  System.SysUtils;

{ TTableSchemaList }

procedure TTableSchemaList.AddTableSchema(pTableName, pSchemaName: String);
var
  lItem: TTableSchema;
begin
  lItem := TTableSchema.Create;
  lItem.TableName := pTableName;
  lItem.TableSchema := pSchemaName;
  Add(lItem);
end;

constructor TTableSchemaList.Create;
begin
  inherited Create(True);
  FParameters := TParameters.Create;
end;

destructor TTableSchemaList.Destroy;
begin
  FParameters.Free;
  inherited;
end;

function TTableSchemaList.FindTableSchema(pTableName, pTableSchema: String): TTableSchema;
var
  lTableSchema: TTableSchema;
begin
  Result := nil;
  for lTableSchema in Self do
  begin
    if SameText(lTableSchema.TableName, pTableName) and SameText(lTableSchema.TableSchema, pTableSchema) then
    begin
      Result := lTableSchema;
      Break;
    end;
  end;
end;

{ TParameters }

constructor TParameters.Create;
begin
  FDataBaseType := PostGree;
  FConnectionParams := TStringList.Create;
  FMappingCharCase := TCharCase.Upper;
  FSchemaCharCase := TCharCase.Lower;
  FRemoveUnderscoreInFields := False;
  FRemoveUnderscoreInClass := False;
end;

destructor TParameters.Destroy;
begin
  FConnectionParams.Free;
  inherited;
end;

end.
