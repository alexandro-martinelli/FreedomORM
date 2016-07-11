unit uTestCustomFieldOptionsTextGenerator;

{$I FreedomORM.inc}

interface

uses
  TestFramework, AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.TextGenerator.CustomFieldOptionsTextGenerator,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLCommands.FieldOptions;

type
  TestTCustomFieldOptionsTextGenerator = class(TTestCase)
  strict private
    FFieldOptionsTextGenerator: TFieldOptionsTextGenerator;
    FFieldOptions: TFieldOptions;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNullable;
    procedure TestDefault;
    procedure TestIdentity;
  end;

implementation

uses
  System.SysUtils;

procedure TestTCustomFieldOptionsTextGenerator.SetUp;
begin
  FFieldOptionsTextGenerator := TFieldOptionsTextGenerator.Create;
  FFieldOptions := TFieldOptions.Create;
end;

procedure TestTCustomFieldOptionsTextGenerator.TearDown;
begin
  FreeAndNil(FFieldOptionsTextGenerator);
  FreeAndNil(FFieldOptions);
end;

procedure TestTCustomFieldOptionsTextGenerator.TestDefault;
var
  lReturnValue: string;
begin
  FFieldOptions.Default := nil;
  lReturnValue := FFieldOptionsTextGenerator.GenerateText(FFieldOptions);
  CheckEqualsString('', lReturnValue);

  FFieldOptions.Default := TLiteralArgument.Create('CURRENT_DATE');
  lReturnValue := FFieldOptionsTextGenerator.GenerateText(FFieldOptions);
  CheckEqualsString('default CURRENT_DATE', lReturnValue);
end;

procedure TestTCustomFieldOptionsTextGenerator.TestIdentity;
var
  lReturnValue: string;
begin
  FFieldOptions.Identity := False;
  lReturnValue := FFieldOptionsTextGenerator.GenerateText(FFieldOptions);
  CheckEqualsString('', lReturnValue);

  FFieldOptions.Identity := True;
  lReturnValue := FFieldOptionsTextGenerator.GenerateText(FFieldOptions);
  CheckEqualsString('identity', lReturnValue);
end;

procedure TestTCustomFieldOptionsTextGenerator.TestNullable;
var
  lReturnValue: string;
begin
  FFieldOptions.Nullable := nNull;
  lReturnValue := FFieldOptionsTextGenerator.GenerateText(FFieldOptions);
  CheckEqualsString('', lReturnValue);

  FFieldOptions.Nullable := nNotNull;
  lReturnValue := FFieldOptionsTextGenerator.GenerateText(FFieldOptions);
  CheckEqualsString('not null', lReturnValue);
end;

initialization
  RegisterTest(TestTCustomFieldOptionsTextGenerator.Suite);

end.

