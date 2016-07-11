unit TestCustomMemberList;

interface

uses
  TestFramework,
  System.Generics.Defaults,
  AM.UnitReader.Enumerations,
  System.Generics.Collections,
  AM.UnitReader.Members.CustomMember;

type
  TestTCustomMemberList = class(TTestCase)
  strict private
    FCustomMemberList: TCustomMemberList<TCustomMember>;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestItemAdded;
  end;

implementation

procedure TestTCustomMemberList.SetUp;
begin
  FCustomMemberList := TCustomMemberList<TCustomMember>.Create(nil);
end;

procedure TestTCustomMemberList.TearDown;
begin
  FCustomMemberList.Free;
  FCustomMemberList := nil;
end;

procedure TestTCustomMemberList.TestAdd;
begin
  FCustomMemberList.Add(TCustomMember.Create('aDataSet', vsStrictPrivate));
  CheckEquals(1, FCustomMemberList.Count);
end;

procedure TestTCustomMemberList.TestItemAdded;
var
  lMember: TCustomMember;
begin
  FCustomMemberList.Add(TCustomMember.Create('aDataSet', vsStrictPrivate));
  CheckEquals(1, FCustomMemberList.Count);
  lMember := FCustomMemberList.Items[0];
  Check(Assigned(lMember));
  CheckEqualsString('aDataSet', lMember.Name);
  Check(vsStrictPrivate = lMember.Visibility);
end;

initialization

// Register any test cases with the test runner
RegisterTest(TestTCustomMemberList.Suite);

end.
