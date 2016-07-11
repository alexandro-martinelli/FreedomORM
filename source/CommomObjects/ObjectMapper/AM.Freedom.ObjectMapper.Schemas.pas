unit AM.Freedom.ObjectMapper.Schemas;

interface

uses
  System.Generics.Collections;

type
  TSchemaItem = class sealed
  private
    FName: String;
    FDefault: Boolean;
  public
    constructor Create(pSchemaName: String; pDefault: Boolean);
    property Name: String read FName write FName;
    property Default: Boolean read FDefault write FDefault;
  end;

  TSchemas = class(TObjectList<TSchemaItem>)
  public
    function DefaultSchema: TSchemaItem;
    procedure SetFirstAsDefault;
    function FindSchema(pSchemaName: String): TSchemaItem;
    function AddSchema(pName: String; pDefault: Boolean): TSchemaItem;
    procedure AdjustSchemas;
    function ToListOfString: TList<String>;
  end;

implementation

uses
  System.SysUtils;

{ TSchemaItem }

constructor TSchemaItem.Create(pSchemaName: String; pDefault: Boolean);
begin
  FName := pSchemaName;
  FDefault := pDefault;
end;

{ TSchemas }

function TSchemas.AddSchema(pName: String; pDefault: Boolean): TSchemaItem;
var
  lDefault: TSchemaItem;
begin
  lDefault := FindSchema(pName);
  Result := nil;
  if not Assigned(lDefault) then
  begin
    if pDefault then
    begin
      lDefault := DefaultSchema;
    end;
    if Assigned(lDefault) then
    begin
      Result := TSchemaItem.Create(LowerCase(pName), False);
    end
    else
    begin
      Result := TSchemaItem.Create(LowerCase(pName), True);
    end;
    Add(Result);
  end;
end;

procedure TSchemas.AdjustSchemas;
var
  lDefault: TSchemaItem;
begin
  if Count > 0 then
  begin
    lDefault := DefaultSchema;
    if Assigned(lDefault) then
    begin
      Extract(lDefault);
      Insert(0, lDefault)
    end
    else
    begin
      SetFirstAsDefault;
    end;
  end;
end;

function TSchemas.DefaultSchema: TSchemaItem;
var
  lSchemaItem: TSchemaItem;
begin
  Result := nil;
  for lSchemaItem in Self do
  begin
    if lSchemaItem.Default then
    begin
      Result := lSchemaItem;
      Break;
    end;
  end;
end;

function TSchemas.FindSchema(pSchemaName: String): TSchemaItem;
var
  lSchemaItem: TSchemaItem;
begin
  Result := nil;
  for lSchemaItem in Self do
  begin
    if SameText(lSchemaItem.Name, pSchemaName) then
    begin
      Result := lSchemaItem;
      Break;
    end;
  end;
end;

procedure TSchemas.SetFirstAsDefault;
var
  lDefault: TSchemaItem;
begin
  lDefault := DefaultSchema;
  if not Assigned(lDefault) and (Count > 0) then
  begin
    Items[0].Default := True;
  end;
end;

function TSchemas.ToListOfString: TList<String>;
var
  lSchema: TSchemaItem;
begin
  Result := TList<String>.Create;
  for lSchema in Self do
  begin
    Result.Add(lSchema.Name)
  end;
end;

end.
