unit uTestConstraintsTextGenerator;

{$I FreedomORM.inc}

interface

uses
  TestFramework, AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.TextGenerator.CustomConstraintTextGenerator,
  AM.Freedom.SQLCommands.Constraints,
  AM.Freedom.TextGenerator.CustomTextGenerator,
  uTestContraints,
  AM.Freedom.TextGenerator.ForeignKeyTextGenerator;

type
  TestTCustomConstraintTextGenerator = class(TestTPrimaryKey)
  strict private
    FCustomConstraintTextGenerator: TCustomConstraintTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties; override;
  end;

  TestTForeignKeyTextGenerator = class(TestTForeignKey)
  strict private
    FForeignKeyTextGenerator: TForeignKeyTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties; override;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.SQLMappers.MSSQLMapper;

procedure TestTCustomConstraintTextGenerator.SetUp;
begin
  inherited;
  FCustomConstraintTextGenerator := TCustomConstraintTextGenerator.Create;
end;

procedure TestTCustomConstraintTextGenerator.TearDown;
begin
  inherited;
  FCustomConstraintTextGenerator.Free;
  FCustomConstraintTextGenerator := nil;
end;

procedure TestTCustomConstraintTextGenerator.TestProperties;
var
  lReturnValue: string;
begin
  inherited TestProperties;
  lReturnValue := FCustomConstraintTextGenerator.GenerateText(GetPrimaryKey);
  CheckEqualsString('constraint PK_CLIENTES primary key (ID)', lReturnValue);
end;

{ TestTForeignKeyTextGenerator }

procedure TestTForeignKeyTextGenerator.SetUp;
begin
  inherited;
  FForeignKeyTextGenerator := TForeignKeyTextGenerator.Create;
end;

procedure TestTForeignKeyTextGenerator.TearDown;
begin
  inherited;
  FreeAndNil(FForeignKeyTextGenerator);
end;

procedure TestTForeignKeyTextGenerator.TestProperties;
var
  lReturnValue: string;
begin
  inherited TestProperties;
  lReturnValue := FForeignKeyTextGenerator.GenerateText(GetForeignKey);
  CheckEqualsString('constraint FK_CLIENTES_CIDADES foreign key (ID_CIDADE)' +
    ' references CIDADE (ID) on update cascade on delete set null', lReturnValue);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTCustomConstraintTextGenerator.Suite);
  RegisterTest(TestTForeignKeyTextGenerator.Suite);


end.

