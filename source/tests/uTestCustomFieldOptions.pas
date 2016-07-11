unit uTestCustomFieldOptions;

{$I FreedomORM.inc}

interface

uses
  TestFramework, AM.Freedom.SQLMappers.NamedObject,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLCommands.FieldOptions;

type
  TestTCustomFieldOptions = class(TTestCase)
  strict private
    FFieldOptions: TFieldOptions;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

implementation

uses
  System.SysUtils;

procedure TestTCustomFieldOptions.SetUp;
begin
  FFieldOptions := TFieldOptions.Create;
end;

procedure TestTCustomFieldOptions.TearDown;
begin
  FFieldOptions.Free;
  FFieldOptions := nil;
end;

procedure TestTCustomFieldOptions.TestProperties;
begin
  FFieldOptions.Nullable := nNotNull;
  FFieldOptions.Default := TValueArgument.CreateAs<Integer>(10);
  Check(FFieldOptions.Nullable = nNotNull);
  CheckEquals(TValueArgument, FFieldOptions.Default.ClassType);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTCustomFieldOptions.Suite);
end.

