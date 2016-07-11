unit TestMethodMember;

interface

uses
  TestFramework, AM.UnitReader.Members.ArgumentMember, AM.UnitReader.Enumerations,
  AM.UnitReader.Members.CustomMember, AM.UnitReader.Members.MethodMember;

type
  TestTMethodMember = class(TTestCase)
  strict private
    FMethodMember: TMethodMember;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestArguments;
  end;

implementation

procedure TestTMethodMember.SetUp;
begin
  FMethodMember := TMethodMember.Create('CriarListaDeDataSets');
end;

procedure TestTMethodMember.TearDown;
begin
  FMethodMember.Free;
  FMethodMember := nil;
end;

procedure TestTMethodMember.TestArguments;
var
  lArgument: TArgumentMember;
begin
  FMethodMember.Arguments.Add(TArgumentMember.Create('aDataSet'));
  FMethodMember.Arguments.Add(TArgumentMember.Create('aModo'));
  FMethodMember.Arguments.Add(TArgumentMember.Create('aList'));

  lArgument := FMethodMember.Arguments.Items[0];
  CheckEqualsString('aDataSet', lArgument.Name);

  lArgument := FMethodMember.Arguments.Items[1];
  CheckEqualsString('aModo', lArgument.Name);

  lArgument := FMethodMember.Arguments.Items[2];
  CheckEqualsString('aList', lArgument.Name);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTMethodMember.Suite);
end.

