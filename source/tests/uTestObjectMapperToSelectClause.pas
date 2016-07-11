unit uTestObjectMapperToSelectClause;

interface

uses
  TestFramework,
  AM.Freedom.ObjectMapper.MapperToSelectClause,
  AM.Freedom.GroupCriteria,
  AM.Freedom.SQLMappers.JoinClause,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.SQLMappers.SelectClause,
  AM.Freedom.ObjectMapper,
  AM.Freedom.ObjectMapper.ObjectMapperToSchemaMapper,
  AM.Freedom.ObjectMapper.ObjectToMapper;

type
  TestTMapperToSelectClause = class(TTestCase)
  strict private
    FObjectMapper: TObjectMapper;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExtractSelect;
  end;

implementation

uses
  uObjectsTests,
  AM.Freedom.TextGenerator.MasterTextGenerator;

procedure TestTMapperToSelectClause.SetUp;
var
  lParams: TObjectToMapperParams;
begin
  lParams := TObjectToMapperParams.Create;
  lParams.MetaClassType := TProduto;
  FObjectMapper := TObjectToMapper.ObjectToMapper(lParams);
end;

procedure TestTMapperToSelectClause.TearDown;
begin
  TObjectToMapper.UnlockMapper(FObjectMapper.GetHashCode);
end;

procedure TestTMapperToSelectClause.TestExtractSelect;
var
  lSelect: TSelectClause;
begin
  lSelect := TMapperToSelectClause.ExtractSelect(FObjectMapper, TGroupCriteria.Create);
  lSelect.Free;
end;

initialization
  RegisterTest(TestTMapperToSelectClause.Suite);

end.
