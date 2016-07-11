unit TestCustomMember;

interface

uses
  TestFramework, System.Generics.Defaults, AM.UnitReader.Enumerations,
  System.Generics.Collections, AM.UnitReader.Members.CustomMember;

type
  // Test methods for class TCustomMember

  TestTCustomMember = class(TTestCase)
  strict private
    FCustomMember: TCustomMember;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

implementation

procedure TestTCustomMember.SetUp;
begin
  FCustomMember := TCustomMember.Create('FValueType', vsStrictPrivate);
end;

procedure TestTCustomMember.TearDown;
begin
  FCustomMember.Free;
  FCustomMember := nil;
end;

procedure TestTCustomMember.TestProperties;
begin
  CheckEqualsString('FValueType', FCustomMember.Name);
  Check(vsStrictPrivate = FCustomMember.Visibility);

  FCustomMember.Name := 'Valor';
  FCustomMember.Visibility := TVisibilityScope.vsPublic;
  CheckEqualsString('Valor', FCustomMember.Name);
  Check(vsPublic = FCustomMember.Visibility);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTCustomMember.Suite);
end.

