unit uTestIndexCommands;

interface

uses
  TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  AM.Freedom.SQLMappers.NamedObject,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLCommands.IndexCommands, AM.Freedom.IndexColumn;

type
  TestTCreateIndexCommand = class(TTestCase)
  strict private
    FCreateIndexCommand: TCreateIndexCommand;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
    procedure TestAddColumnWithObject;
    procedure TestAddColumn;
  end;

  TestTDropIndexCommand = class(TTestCase)
  strict private
    FDropIndexCommand: TDropIndexCommand;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

implementation

procedure TestTCreateIndexCommand.SetUp;
begin
  FCreateIndexCommand := TCreateIndexCommand.Create;
end;

procedure TestTCreateIndexCommand.TearDown;
begin
  FCreateIndexCommand.Free;
  FCreateIndexCommand := nil;
end;

procedure TestTCreateIndexCommand.TestAddColumn;
var
  lNullOption: TNullOption;
  lSortType: TSortType;
  lColumnName: string;
begin
  lNullOption := noFirst;
  lSortType := TSortType.Desc;
  lColumnName := 'DATA_CADASTRO';
  FCreateIndexCommand.Columns.Clear;
  FCreateIndexCommand.AddColumn(lColumnName, lSortType, lNullOption);
  CheckEquals(1, FCreateIndexCommand.Columns.Count);
  CheckEqualsString('DATA_CADASTRO', FCreateIndexCommand.Columns.First.Name);
  Check(FCreateIndexCommand.Columns.First.SortType = lSortType, 'Invalid sort type');
  Check(FCreateIndexCommand.Columns.First.NullOption = lNullOption, 'Invalid null option');
end;

procedure TestTCreateIndexCommand.TestAddColumnWithObject;
var
  lColumn: TIndexColumn;
begin
  lColumn := TIndexColumn.Create('DATA_CADASTRO', TSortType.Desc, noLast);
  FCreateIndexCommand.Columns.Clear;
  FCreateIndexCommand.AddColumn(lColumn);
  CheckEquals(1, FCreateIndexCommand.Columns.Count);
  CheckEqualsString('DATA_CADASTRO', lColumn.Name);
  Check(lColumn.SortType = TSortType.Desc, 'Invalid sort type');
  Check(lColumn.NullOption = noLast, 'Invalid null option');
end;

procedure TestTCreateIndexCommand.TestProperties;
begin
  FCreateIndexCommand.Schema := 'dbo';
  FCreateIndexCommand.Name := 'IDX_PRODUTO_DATA_CADASTRO';
  FCreateIndexCommand.OnTable := 'PRODUTO';
  FCreateIndexCommand.Option := ioClustered;
  Check(Assigned(FCreateIndexCommand.Columns), 'Columns not Created');
  Check(FCreateIndexCommand.CommandType = TCommandType.Create, 'Invalid column type');
  CheckEqualsString('dbo', FCreateIndexCommand.Schema);
  CheckEqualsString('IDX_PRODUTO_DATA_CADASTRO', FCreateIndexCommand.Name);
  CheckEqualsString('PRODUTO', FCreateIndexCommand.OnTable);
  Check(FCreateIndexCommand.Option = ioClustered, 'Invalid Option test');
end;

procedure TestTDropIndexCommand.SetUp;
begin
  FDropIndexCommand := TDropIndexCommand.Create;
end;

procedure TestTDropIndexCommand.TearDown;
begin
  FDropIndexCommand.Free;
  FDropIndexCommand := nil;
end;

procedure TestTDropIndexCommand.TestProperties;
begin
  FDropIndexCommand.Schema := 'dbo';
  FDropIndexCommand.Name := 'IDX_PRODUTO_DATA_CADASTRO';
  FDropIndexCommand.OnTable := 'PRODUTO';
  Check(FDropIndexCommand.CommandType = TCommandType.Drop, 'Invalid command type');
  CheckEqualsString('dbo', FDropIndexCommand.Schema);
  CheckEqualsString('IDX_PRODUTO_DATA_CADASTRO', FDropIndexCommand.Name);
  CheckEqualsString('PRODUTO', FDropIndexCommand.OnTable);
end;

initialization
  RegisterTest(TestTCreateIndexCommand.Suite);
  RegisterTest(TestTDropIndexCommand.Suite);

end.
