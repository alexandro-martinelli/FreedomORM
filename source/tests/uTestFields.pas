unit uTestFields;

{$I FreedomORM.inc}

interface

uses
  TestFramework, AM.Freedom.SQLCommands.SizedFieldCommand,
  AM.Freedom.SQLCommands.Fields, AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLCommands.CustomFieldCommand, AM.Freedom.SQLMappers.Arguments;

type
  TCustomTestFieldCommand = class(TTestCase)
  strict private
    FFieldCommand: TCustomFieldCommand;
  strict protected
    function GetFieldCommandClass: TFieldCommandClass; virtual; abstract;
  public
    procedure SetUp; override;
    procedure TearDown; override;
    property Field: TCustomFieldCommand read FFieldCommand;
  published
    procedure TestProperties; virtual;
  end;

  TestTVarcharFieldCommand = class(TCustomTestFieldCommand)
  strict protected
    function GetFieldCommandClass: TFieldCommandClass; override;
  published
    procedure TestProperties; override;
  end;

  TestTCharFieldCommand = class(TCustomTestFieldCommand)
  strict protected
    function GetFieldCommandClass: TFieldCommandClass; override;
  published
    procedure TestProperties; override;
  end;

  TestTSmallintFieldCommand = class(TCustomTestFieldCommand)
  strict protected
    function GetFieldCommandClass: TFieldCommandClass; override;
  end;

  TestTIntegerFieldCommand = class(TCustomTestFieldCommand)
  strict protected
    function GetFieldCommandClass: TFieldCommandClass; override;
  end;

  TestTInt64FieldCommand = class(TCustomTestFieldCommand)
  strict protected
    function GetFieldCommandClass: TFieldCommandClass; override;
  end;

  TestTFloatFieldCommand = class(TCustomTestFieldCommand)
  strict protected
    function GetFieldCommandClass: TFieldCommandClass; override;
  end;

implementation

uses
  System.SysUtils, AM.Freedom.SQLCommands.FieldOptions;

{ TCustomTestFieldCommand }

procedure TCustomTestFieldCommand.SetUp;
begin
  inherited;
  FFieldCommand := GetFieldCommandClass.Create;
end;

procedure TCustomTestFieldCommand.TearDown;
begin
  FreeAndNil(FFieldCommand);
end;

procedure TCustomTestFieldCommand.TestProperties;
begin
  FFieldCommand.Name := 'NOME';
  CheckEquals(GetFieldCommandClass, FFieldCommand.ClassType);
  CheckEqualsString('NOME', FFieldCommand.Name);
  Check(Assigned(FFieldCommand.FieldOptions));
  CheckEquals(TFieldOptions, FFieldCommand.FieldOptions.ClassType)
end;

{ TestTVarcharFieldCommand }

function TestTVarcharFieldCommand.GetFieldCommandClass: TFieldCommandClass;
begin
  Result := TVarcharFieldCommand;
end;

procedure TestTVarcharFieldCommand.TestProperties;
begin
  inherited;
  TVarcharFieldCommand(Field).Size := 150;
  CheckEquals(150, TVarcharFieldCommand(Field).Size);
end;

{ TestTCharFieldCommand }

function TestTCharFieldCommand.GetFieldCommandClass: TFieldCommandClass;
begin
  Result := TCharFieldCommand;
end;

procedure TestTCharFieldCommand.TestProperties;
begin
  inherited;
  TCharFieldCommand(Field).Size := 1;
  CheckEquals(1, TCharFieldCommand(Field).Size);
end;

{ TestTSmallintFieldCommand }

function TestTSmallintFieldCommand.GetFieldCommandClass: TFieldCommandClass;
begin
  Result := TSmallintFieldCommand;
end;

{ TestTIntegerFieldCommand }

function TestTIntegerFieldCommand.GetFieldCommandClass: TFieldCommandClass;
begin
  Result := TIntegerFieldCommand;
end;

{ TestTInt64FieldCommand }

function TestTInt64FieldCommand.GetFieldCommandClass: TFieldCommandClass;
begin
  Result := TInt64FieldCommand;
end;

{ TestTFloatFieldCommand }

function TestTFloatFieldCommand.GetFieldCommandClass: TFieldCommandClass;
begin
  Result := TFloatFieldCommand;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTVarcharFieldCommand.Suite);
  RegisterTest(TestTCharFieldCommand.Suite);
  RegisterTest(TestTSmallintFieldCommand.Suite);
  RegisterTest(TestTIntegerFieldCommand.Suite);
  RegisterTest(TestTInt64FieldCommand.Suite);
  RegisterTest(TestTFloatFieldCommand.Suite);

end.

