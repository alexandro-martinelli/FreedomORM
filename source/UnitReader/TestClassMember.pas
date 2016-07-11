unit TestClassMember;

interface

uses
  TestFramework,
  AM.UnitReader.Members.FieldMember,
  System.Generics.Collections,
  AM.UnitReader.Members.PropertyMember,
  AM.UnitReader.Members.MethodMember,
  AM.UnitReader.Enumerations,
  AM.UnitReader.Members.CustomMember,
  AM.UnitReader.Members.ConstantMember,
  AM.UnitReader.Members.ClassMember;

type
  TestTClassMember = class(TTestCase)
  strict private
    FClassMember: TClassMember;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestPropertiesAssigned;
    procedure TestConstants;
    procedure TestFields;
    procedure TestMethods;
    procedure TestProperties;
    procedure TestClasses;
  end;

implementation

procedure TestTClassMember.SetUp;
begin
  FClassMember := TClassMember.Create('');
end;

procedure TestTClassMember.TearDown;
begin
  FClassMember.Free;
  FClassMember := nil;
end;

procedure TestTClassMember.TestClasses;
begin
  FClassMember.Classes.Add(TClassMember.Create('THomem'));
  CheckEquals(1, FClassMember.Classes.Count);
  CheckIs(FClassMember.Classes.Items[0], TClassMember);
end;

procedure TestTClassMember.TestConstants;
begin
  FClassMember.Constants.Add(TConstantMember.Create('cSexo'));
  CheckEquals(1, FClassMember.Constants.Count);
  CheckIs(FClassMember.Constants.Items[0], TConstantMember);
end;

procedure TestTClassMember.TestFields;
begin
  FClassMember.Fields.Add(TFieldMember.Create('FSexo'));
  CheckEquals(1, FClassMember.Fields.Count);
  CheckIs(FClassMember.Fields.Items[0], TFieldMember);
end;

procedure TestTClassMember.TestMethods;
begin
  FClassMember.Methods.Add(TMethodMember.Create('DefinirSexo'));
  CheckEquals(1, FClassMember.Methods.Count);
  CheckIs(FClassMember.Methods.Items[0], TMethodMember);
end;

procedure TestTClassMember.TestProperties;
begin
  FClassMember.Properties.Add(TPropertyMember.Create('Sexo', 'TSexo'));
  CheckEquals(1, FClassMember.Properties.Count);
  CheckIs(FClassMember.Properties.Items[0], TPropertyMember);
end;

procedure TestTClassMember.TestPropertiesAssigned;
begin
  Check(Assigned(FClassMember.Constants));
  Check(Assigned(FClassMember.Fields));
  Check(Assigned(FClassMember.Methods));
  Check(Assigned(FClassMember.Properties));
  Check(Assigned(FClassMember.Classes));
end;

initialization

// Register any test cases with the test runner
RegisterTest(TestTClassMember.Suite);

end.
