unit AM.Freedom.ObjectMapper.ObjectMapperToSchemaMapper;

interface

uses
  AM.Freedom.ObjectMapper,
  AM.Freedom.ObjectMapper.Schemas;

type
  TObjectMapperToSchemaMapper = class sealed
  strict private
    FObjectMapper: TObjectMapper;
    FSchema: TSchemaItem;
    FSchemaMapper: TObjectMapper;
    procedure CreateSchemaMapper;
    function DoExtract: TObjectMapper;
    procedure ExtractColumns;
    procedure ExtractMethods;
    procedure ExtractTriggers;
    procedure ExtractConstraints;
    procedure ExtractPrimaryKey;
    procedure ExtractUniques;
    procedure ExtractForeignKeys;
  public
    class function ExtractSchemaMapper(pObjectMapper: TObjectMapper; pSchema: TSchemaItem): TObjectMapper;
  end;

implementation

uses
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.ObjectMapper.MethodMapper,
  AM.Freedom.ObjectMapper.TriggerMapper,
  AM.Freedom.ObjectMapper.ConstraintMapper;

{ TObjectMapperToSchemaMapper }

procedure TObjectMapperToSchemaMapper.CreateSchemaMapper;
begin
  FSchemaMapper := TObjectMapper.Create(False);
  FSchemaMapper.Alias := FObjectMapper.Alias;
  FSchemaMapper.Name := FObjectMapper.Name;
  FSchemaMapper.IsList := FObjectMapper.IsList;
  FSchemaMapper.MetaClassType :=  FObjectMapper.MetaClassType;
  FSchemaMapper.RttiOptions := FObjectMapper.RttiOptions.CreateNew;
  if (Assigned(FSchema)) then
  begin
    FSchemaMapper.CurrentSchema := FSchema.Name;
  end
  else
  begin
    FSchemaMapper.CurrentSchema := '';
  end;
end;

function TObjectMapperToSchemaMapper.DoExtract: TObjectMapper;
begin
  CreateSchemaMapper;
  ExtractColumns;
  ExtractMethods;
  ExtractTriggers;
  ExtractConstraints;
  Result := FSchemaMapper;
end;

procedure TObjectMapperToSchemaMapper.ExtractColumns;
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in FObjectMapper.Columns do
  begin
    if (not lColumn.IsExtension) and (lColumn.Schemas.FindSchema(FSchemaMapper.CurrentSchema) <> nil) or (FSchemaMapper.CurrentSchema = '') then
    begin
      FSchemaMapper.Columns.Add(lColumn);
    end;
  end;
end;

procedure TObjectMapperToSchemaMapper.ExtractConstraints;
begin
  ExtractPrimaryKey;
  ExtractUniques;
  ExtractForeignKeys;
end;

procedure TObjectMapperToSchemaMapper.ExtractForeignKeys;
var
  lForeign: TForeignMapper;
begin
  if FSchemaMapper.CurrentSchema <> '' then
  begin
    for lForeign in FObjectMapper.Foreigns do
    begin
      if lForeign.Schemas.Contains(FSchemaMapper.CurrentSchema) then
      begin
        FSchemaMapper.Foreigns.Add(lForeign);
      end;
    end;
  end
  else
  begin
    for lForeign in FObjectMapper.Foreigns do
    begin
      if lForeign.Schemas.Count = 0 then
      begin
        FSchemaMapper.Foreigns.Add(lForeign);
      end;
    end;
  end;
end;

procedure TObjectMapperToSchemaMapper.ExtractMethods;
var
  lMethod: TMethodMapper;
begin
  for lMethod in FObjectMapper.Methods do
  begin
    FSchemaMapper.Methods.Add(lMethod);
  end;
end;

procedure TObjectMapperToSchemaMapper.ExtractPrimaryKey;
begin
  if FObjectMapper.Primarys.Count > 0 then
  begin
    if FSchemaMapper.CurrentSchema <> '' then
    begin
      if FObjectMapper.Primarys.FindSchema(FSchemaMapper.CurrentSchema) <> nil then
      begin
        FSchemaMapper.Primarys.Add(FObjectMapper.Primarys.FindSchema(FSchemaMapper.CurrentSchema));
      end;
    end
    else
    begin
      FSchemaMapper.Primarys.Add(FObjectMapper.Primarys.Items[0]);
    end;
  end;
end;

class function TObjectMapperToSchemaMapper.ExtractSchemaMapper(pObjectMapper: TObjectMapper; pSchema: TSchemaItem): TObjectMapper;
var
  lExtracter: TObjectMapperToSchemaMapper;
begin
  lExtracter := TObjectMapperToSchemaMapper.Create;
  try
    lExtracter.FObjectMapper := pObjectMapper;
    lExtracter.FSchema := pSchema;
    Result := lExtracter.DoExtract;
  finally
    lExtracter.Free;
  end;
end;

procedure TObjectMapperToSchemaMapper.ExtractTriggers;
var
  lTrigger: TTriggerMapper;
begin
  for lTrigger in FObjectMapper.Triggers do
  begin
    FSchemaMapper.Triggers.Add(lTrigger);
  end;
end;

procedure TObjectMapperToSchemaMapper.ExtractUniques;
var
  lUnique: TUniqueMapper;
begin
  if FSchemaMapper.CurrentSchema <> '' then
  begin
    for lUnique in FObjectMapper.Uniques do
    begin
      if lUnique.Schemas.Contains(FSchemaMapper.CurrentSchema) then
      begin
        FSchemaMapper.Uniques.Add(lUnique);
      end;
    end;
  end
  else
  begin
    for lUnique in FObjectMapper.Uniques do
    begin
      if lUnique.Schemas.Count = 0 then
      begin
        FSchemaMapper.Uniques.Add(lUnique);
      end;
    end;
  end;
end;

end.
