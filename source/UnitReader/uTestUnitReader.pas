unit uTestUnitReader;

interface

uses
  TestFramework,
  AM.UnitReader.Readers.CustomReader,
  AM.UnitReader.Members.FieldMember,
  AM.UnitReader.Members.ClassMember,
  AM.UnitReader.PrefixOptions,
  AM.UnitReader.Members.MethodMember,
  AM.UnitReader.Enumerations,
  AM.UnitReader.Members.CustomMember,
  AM.UnitReader.Members.UnitMember,
  AM.UnitReader,
  AM.UnitReader.Readers.ClassReader,
  AM.UnitReader.Members.ConstantMember,
  AM.UnitReader.Helper.MemberVisibility,
  System.Classes,
  AM.UnitReader.Utils.StrUtils;

type
  // Test methods for class TUnitReader
  TestTUnitReader = class(TTestCase)
  strict private
    FUnitMember: TUnitMember;
  public
    procedure TearDown; override;
  published
    procedure TestRead;
    procedure TestReaduProduto;
    procedure TestReaduObjectsTests;
  end;

implementation

uses
  AM.UnitReader.Members.PropertyMember;

procedure TestTUnitReader.TearDown;
begin
  FUnitMember.Free;
end;

procedure TestTUnitReader.TestRead;
var
  lClass: TClassMember;
  lProperty: TPropertyMember;
begin
  FUnitMember := TUnitReader.UnitMemberFromUnitPas('D:\UEntidade.pas');
  CheckEquals(2, FUnitMember.Classes.Count);
  lClass := FUnitMember.Classes.Items[0];
  CheckEqualsString('TEntidadeList', lClass.Name);
  CheckEquals(2, lClass.Properties.Count);
  lProperty := lClass.Properties.Items[0];
  CheckEqualsString('Count', lProperty.Name);
  lProperty := lClass.Properties.Items[1];
  CheckEqualsString('Entidade', lProperty.Name);
  lClass := FUnitMember.Classes.Items[1];
  CheckEqualsString('TEntidade', lClass.Name);
  CheckEquals(7, lClass.Properties.Count);

  lProperty := lClass.Properties.Items[0];
  CheckEqualsString('Versao', lProperty.Name);

  lProperty := lClass.Properties.Items[1];
  CheckEqualsString('Params', lProperty.Name);

  lProperty := lClass.Properties.Items[2];
  CheckEqualsString('Modulo', lProperty.Name);

  lProperty := lClass.Properties.Items[3];
  CheckEqualsString('KeyFields', lProperty.Name);

  lProperty := lClass.Properties.Items[4];
  CheckEqualsString('TableAlias', lProperty.Name);

  lProperty := lClass.Properties.Items[5];
  CheckEqualsString('ImageType', lProperty.Name);


end;

procedure TestTUnitReader.TestReaduObjectsTests;
var
  lClass: TClassMember;
  lProperty: TPropertyMember;
begin
  FUnitMember := TUnitReader.UnitMemberFromUnitPas('D:\uObjectsTests.pas');

  CheckEquals(11, FUnitMember.Classes.Count);
  lClass := FUnitMember.Classes.Items[8];
  CheckEqualsString('TProduto', lClass.Name);
  CheckEqualsString('TDBObject', lClass.ParentName);
  CheckEquals(15, lClass.Properties.Count);
  lProperty := lClass.Properties.Items[8];
  CheckEqualsString('MateriasPrimas', lProperty.Name);
  Check(lProperty.PropertyType = mtUnknow);
end;

procedure TestTUnitReader.TestReaduProduto;
var
  lClass: TClassMember;
  lProperty: TPropertyMember;
begin
  FUnitMember := TUnitReader.UnitMemberFromUnitPas('D:\uProduto.pas');

  CheckEquals(1, FUnitMember.Classes.Count);
  lClass := FUnitMember.Classes.Items[0];
  CheckEqualsString('TProduto', lClass.Name);
  CheckEqualsString('TFreedomObject', lClass.ParentName);
  CheckEquals(7, lClass.Properties.Count);
  lProperty := lClass.Properties.Items[5];
  Check(lProperty.PropertyType = mtTStrings);
end;

initialization

RegisterTest(TestTUnitReader.Suite);

end.
