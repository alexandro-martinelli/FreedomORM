program TestUnitReader;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

{$R *.RES}

uses
  DUnitTestRunner,
  TestClassMember in '..\..\TestClassMember.pas',
  TestClassReader in '..\..\TestClassReader.pas',
  TestCustomMember in '..\..\TestCustomMember.pas',
  TestCustomMemberList in '..\..\TestCustomMemberList.pas',
  TestMethodMember in '..\..\TestMethodMember.pas',
  TestMethodReader in '..\..\TestMethodReader.pas',
  TestStrUtilsClass in '..\..\TestStrUtilsClass.pas',
  uTestUnitReader in '..\..\uTestUnitReader.pas',
  AM.UnitReader.Members.CustomMember in '..\CommomObjects\AM.UnitReader.Members.CustomMember.pas',
  AM.UnitReader.PrefixOptions in '..\CommomObjects\AM.UnitReader.PrefixOptions.pas',
  AM.UnitReader.Readers.CustomReader in '..\CommomObjects\AM.UnitReader.Readers.CustomReader.pas',
  AM.UnitReader.WordVerificator in '..\CommomObjects\AM.UnitReader.WordVerificator.pas',
  AM.UnitReader.Enumerations in '..\Commom\AM.UnitReader.Enumerations.pas',
  AM.UnitReader.Utils.StrUtils in '..\Commom\AM.UnitReader.Utils.StrUtils.pas',
  AM.UnitReader.Helper.MemberType in '..\Commom\Helpers\AM.UnitReader.Helper.MemberType.pas',
  AM.UnitReader.Helper.MemberVisibility in '..\Commom\Helpers\AM.UnitReader.Helper.MemberVisibility.pas',
  AM.UnitReader.Helper.MethodType in '..\Commom\Helpers\AM.UnitReader.Helper.MethodType.pas',
  AM.UnitReader.Helper.WordType in '..\Commom\Helpers\AM.UnitReader.Helper.WordType.pas',
  AM.UnitReader.Members.ArgumentMember in '..\API\Members\AM.UnitReader.Members.ArgumentMember.pas',
  AM.UnitReader.Members.AttributeMember in '..\API\Members\AM.UnitReader.Members.AttributeMember.pas',
  AM.UnitReader.Members.ClassMember in '..\API\Members\AM.UnitReader.Members.ClassMember.pas',
  AM.UnitReader.Members.ConstantMember in '..\API\Members\AM.UnitReader.Members.ConstantMember.pas',
  AM.UnitReader.Members.FieldMember in '..\API\Members\AM.UnitReader.Members.FieldMember.pas',
  AM.UnitReader.Members.MethodMember in '..\API\Members\AM.UnitReader.Members.MethodMember.pas',
  AM.UnitReader.Members.PropertyMember in '..\API\Members\AM.UnitReader.Members.PropertyMember.pas',
  AM.UnitReader.Members.UnitMember in '..\API\Members\AM.UnitReader.Members.UnitMember.pas',
  AM.UnitReader.Readers.ClassReader in '..\API\Readers\AM.UnitReader.Readers.ClassReader.pas',
  AM.UnitReader.Readers.MethodReader in '..\API\Readers\AM.UnitReader.Readers.MethodReader.pas',
  AM.UnitReader in '..\API\AM.UnitReader.pas',
  uTestSQLMapper in '..\..\..\tests\uTestSQLMapper.pas',
  AM.Freedom.SQLMappers.CustomSQLMapper in '..\..\..\CommomObjects\SQLMapers\AM.Freedom.SQLMappers.CustomSQLMapper.pas',
  uObjectsTests in '..\..\..\tests\uObjectsTests.pas',
  AM.Freedom.ObjectMapper.MapperToSelectClause in '..\..\..\API\ObjectMappers\Readers\AM.Freedom.ObjectMapper.MapperToSelectClause.pas',
  AM.Freedom.JSONFreedomObject in '..\..\..\CommomObjects\AM.Freedom.JSONFreedomObject.pas';

begin
  DUnitTestRunner.RunRegisteredTests;
end.

