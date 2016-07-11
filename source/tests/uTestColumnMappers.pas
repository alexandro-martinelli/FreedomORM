unit uTestColumnMappers;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.ColumnMappers;

type
  TestTColumnMapper = class(TTestCase)
  strict private
    FColumnMapper: TCustomColumnMapper;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTBooleanColumnMapper = class(TTestCase)
  strict private
    FBooleanColumnMapper: TBooleanColumnMapper;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTEnumerationColumnMapper = class(TTestCase)
  strict private
    FEnumerationColumnMapper: TEnumerationColumnMapper;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTJoinColumnMapper = class(TTestCase)
  strict private
    FJoinColumnMapper: TJoinedColumnMapper;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTDetailColumnMapper = class(TTestCase)
  strict private
    FDetailColumnMapper: TDetailColumnMapper;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

implementation

uses
  System.SysUtils;

procedure TestTColumnMapper.SetUp;
begin
  FColumnMapper := TCustomColumnMapper.Create;
end;

procedure TestTColumnMapper.TearDown;
begin
  FColumnMapper.Free;
  FColumnMapper := nil;
end;

procedure TestTColumnMapper.TestProperties;
begin
  FColumnMapper.Name := 'NOME';
  FColumnMapper.Size := 150;
  FColumnMapper.Scale := 0;
  FColumnMapper.ColumnOptions := [Required];

  CheckEqualsString('NOME', FColumnMapper.Name);
  CheckEquals(150, FColumnMapper.Size);
  CheckEquals(0, FColumnMapper.Scale);
  Check(FColumnMapper.ColumnOptions = [Required]);

  FColumnMapper.Name := 'VALOR';
  FColumnMapper.Size := 15;
  FColumnMapper.Scale := 4;
  FColumnMapper.ColumnOptions := [];

  CheckEqualsString('VALOR', FColumnMapper.Name);
  CheckEquals(15, FColumnMapper.Size);
  CheckEquals(4, FColumnMapper.Scale);
  Check(FColumnMapper.ColumnOptions = []);
end;

procedure TestTBooleanColumnMapper.SetUp;
begin
  FBooleanColumnMapper := TBooleanColumnMapper.Create;
end;

procedure TestTBooleanColumnMapper.TearDown;
begin
  FBooleanColumnMapper.Free;
  FBooleanColumnMapper := nil;
end;

procedure TestTBooleanColumnMapper.TestProperties;
begin
  FBooleanColumnMapper.Name := 'ATIVO';
  FBooleanColumnMapper.ValueTrue := 1;
  FBooleanColumnMapper.ValueFalse := 0;

  CheckEqualsString('ATIVO', FBooleanColumnMapper.Name);
  CheckEquals(1, FBooleanColumnMapper.ValueTrue);
  CheckEquals(0, FBooleanColumnMapper.ValueFalse);

  FBooleanColumnMapper.Name := 'ATIVO';
  FBooleanColumnMapper.ValueTrue := 'S';
  FBooleanColumnMapper.ValueFalse := 'N';

  CheckEqualsString('ATIVO', FBooleanColumnMapper.Name);
  CheckEqualsString('S', FBooleanColumnMapper.ValueTrue);
  CheckEqualsString('N', FBooleanColumnMapper.ValueFalse);

  FBooleanColumnMapper.Name := 'ATIVO';
  FBooleanColumnMapper.ValueTrue := True;
  FBooleanColumnMapper.ValueFalse := False;

  CheckEqualsString('ATIVO', FBooleanColumnMapper.Name);
  CheckTrue(FBooleanColumnMapper.ValueTrue);
  CheckFalse(False, FBooleanColumnMapper.ValueFalse);
end;

procedure TestTEnumerationColumnMapper.SetUp;
begin
  FEnumerationColumnMapper := TEnumerationColumnMapper.Create;
end;

procedure TestTEnumerationColumnMapper.TearDown;
begin
  FEnumerationColumnMapper.Free;
  FEnumerationColumnMapper := nil;
end;

procedure TestTEnumerationColumnMapper.TestProperties;
begin
  FEnumerationColumnMapper.Name := 'SEXO';
  FEnumerationColumnMapper.EnumType := emByte;

  CheckEqualsString('SEXO', FEnumerationColumnMapper.Name);
  CheckTrue(emByte = FEnumerationColumnMapper.EnumType);
  CheckEquals(0, Length(FEnumerationColumnMapper.EnumCharOf));
end;

procedure TestTJoinColumnMapper.SetUp;
begin
  FJoinColumnMapper := TJoinedColumnMapper.Create;
end;

procedure TestTJoinColumnMapper.TearDown;
begin
  FJoinColumnMapper.Free;
  FJoinColumnMapper := nil;
end;

procedure TestTJoinColumnMapper.TestProperties;
begin
  FJoinColumnMapper.Name := 'CODCIDADE';
  FJoinColumnMapper.JoinKind := jkInner;
  FJoinColumnMapper.RefColumnName := 'CODCIDADE';
  FJoinColumnMapper.RefObjectAlias := 'CID';
  FJoinColumnMapper.OriginalRefObjectAlias := 'CID';

  CheckEqualsString('CODCIDADE', FJoinColumnMapper.Name);
  Check(jkInner = FJoinColumnMapper.JoinKind);
  CheckEqualsString('CODCIDADE', FJoinColumnMapper.RefColumnName);
  CheckEqualsString('CID', FJoinColumnMapper.RefObjectAlias);
  CheckEqualsString('CID', FJoinColumnMapper.OriginalRefObjectAlias);
end;

procedure TestTDetailColumnMapper.SetUp;
begin
  FDetailColumnMapper := TDetailColumnMapper.Create;
end;

procedure TestTDetailColumnMapper.TearDown;
begin
  FreeAndNil(FDetailColumnMapper);
end;

procedure TestTDetailColumnMapper.TestProperties;
begin
  FDetailColumnMapper.Name := 'ID';
  FDetailColumnMapper.RefColumnName := 'ID_PESSOA';

  CheckEqualsString('ID', FDetailColumnMapper.Name);
  CheckEqualsString('ID_PESSOA', FDetailColumnMapper.RefColumnName);
end;

initialization
  RegisterTest(TestTColumnMapper.Suite);
  RegisterTest(TestTBooleanColumnMapper.Suite);
  RegisterTest(TestTEnumerationColumnMapper.Suite);
  RegisterTest(TestTJoinColumnMapper.Suite);
  RegisterTest(TestTDetailColumnMapper.Suite);

end.

