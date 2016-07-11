unit AM.Freedom.ObjectMapper.MapperToSelectClause;

interface

uses
  AM.Freedom.ObjectMapper,
  AM.Freedom.GroupCriteria,
  AM.Freedom.SQLMappers.SelectClause,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.SQLMappers.JoinClause,
  AM.Freedom.ObjectMapper.ObjectMapperToSchemaMapper;

type
  TMapperToSelectClause = class
  strict private
    FObjectMapper: TObjectMapper;
    FGroupCriteria: TGroupCriteria;
    FSelect: TSelectClause;
    function DoExtract: TSelectClause;
  public
    class function ExtractSelect(pObjectMapper: TObjectMapper; pGroupCriteria: TGroupCriteria): TSelectClause;
  end;

implementation

uses
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLMappers.OrderByClause,
  AM.Freedom.SQLMappers.WhereClause,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.Schemas;
{ TMapperToSelectClause }

function TMapperToSelectClause.DoExtract: TSelectClause;
var
  lSchema: TSchemaItem;
begin
  if FObjectMapper.CurrentSchema = '' then
  begin
    lSchema := nil;
    if Assigned(FGroupCriteria) and (FGroupCriteria.SchemaName <> '') then
    begin
      lSchema := FObjectMapper.Schemas.FindSchema(FGroupCriteria.SchemaName);
    end;
    if (not Assigned(lSchema) and (FObjectMapper.Schemas.Count > 0)) then
    begin
      lSchema := FObjectMapper.Schemas.DefaultSchema;
    end;
    if (Assigned(lSchema)) then
    begin
      FObjectMapper.CurrentSchema := lSchema.Name;
    end;
  end;
  FSelect := TSelectClause.CreateFromMapper(FObjectMapper);
  try
    FSelect.WhereClause := TWhereClause(FGroupCriteria);
    Result := FSelect;
  except
    FSelect.Free;
    raise;
  end;
end;

class function TMapperToSelectClause.ExtractSelect(pObjectMapper: TObjectMapper; pGroupCriteria: TGroupCriteria): TSelectClause;
var
  lMapperToSelectClause: TMapperToSelectClause;
begin
  lMapperToSelectClause := TMapperToSelectClause.Create;
  try
    lMapperToSelectClause.FObjectMapper := pObjectMapper;
    lMapperToSelectClause.FGroupCriteria := pGroupCriteria;
    Result := lMapperToSelectClause.DoExtract;
  finally
    lMapperToSelectClause.Free;
  end;
end;

end.
