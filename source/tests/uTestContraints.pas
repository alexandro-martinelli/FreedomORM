unit uTestContraints;

{$I FreedomORM.inc}

interface

uses
  TestFramework, AM.Freedom.SQLCommands.Constraints,
  AM.Freedom.SQLMappers.Arguments, AM.Freedom.SQLMappers.NamedObject,
  System.Generics.Collections, AM.Freedom.EnumerationTypes;

type
  TestTPrimaryKey = class(TTestCase)
  strict private
    FPrimaryKey: TPrimaryKey;
  strict protected
    function GetPrimaryKey: TPrimaryKey;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties; virtual;
  end;

  TestTForeignKey = class(TTestCase)
  strict private
    FForeignKey: TForeignKey;
  strict protected
    function GetForeignKey: TForeignKey;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties; virtual;
  end;

implementation

uses
  System.SysUtils;

function TestTPrimaryKey.GetPrimaryKey: TPrimaryKey;
begin
  Result := FPrimaryKey;
end;

procedure TestTPrimaryKey.SetUp;
begin
  FPrimaryKey := TPrimaryKey.Create;
end;

procedure TestTPrimaryKey.TearDown;
begin
  FreeAndNil(FPrimaryKey);
end;

procedure TestTPrimaryKey.TestProperties;
begin
  FPrimaryKey.AddField('ID');
  FPrimaryKey.Name := 'PK_CLIENTES';
  CheckEqualsString('PK_CLIENTES', FPrimaryKey.Name);
  CheckEquals(1, FPrimaryKey.Fields.Count);
  CheckEqualsString('ID', FPrimaryKey.Fields.Items[0]);
end;

function TestTForeignKey.GetForeignKey: TForeignKey;
begin
  Result := FForeignKey;
end;

procedure TestTForeignKey.SetUp;
begin
  FForeignKey := TForeignKey.Create;
end;

procedure TestTForeignKey.TearDown;
begin
  FreeAndnil(FForeignKey);
end;

procedure TestTForeignKey.TestProperties;
begin
  FForeignKey.AddField('ID_CIDADE');
  FForeignKey.ReferencesTo('CIDADE').ReferencesFields(['ID']).
    Update(TForeignOption.Cascade).Delete(TForeignOption.SetNull).Name := 'FK_CLIENTES_CIDADES';
  CheckEqualsString('FK_CLIENTES_CIDADES', FForeignKey.Name);
  CheckEquals(1, FForeignKey.Fields.Count);
  CheckEquals(1, FForeignKey.ReferenceFields.Count);
  CheckEqualsString('ID_CIDADE', FForeignKey.Fields.Items[0]);
  CheckEqualsString('ID', FForeignKey.ReferenceFields.Items[0]);
  Check(TForeignOption.Cascade = FForeignKey.OnUpdate, 'OnUpdate Options');
  Check(TForeignOption.SetNull = FForeignKey.OnDelete, 'OnDelete Options');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTPrimaryKey.Suite);
  RegisterTest(TestTForeignKey.Suite);
end.

