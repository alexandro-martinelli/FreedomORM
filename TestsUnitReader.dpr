program TestsUnitReader;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

{$R *.RES}

uses
  DUnitTestRunner,
  TestCustomMember in 'TestCustomMember.pas',
  TestMethodMember in 'TestMethodMember.pas',
  TestCustomMemberList in 'TestCustomMemberList.pas',
  TestClassMember in 'TestClassMember.pas' {,
  AM.UnitReader.WordVerificator in 'source\CommomObjects\AM.UnitReader.WordVerificator.pas',
  AM.UnitReader.Readers.CustomReader in 'source\CommomObjects\AM.UnitReader.Readers.CustomReader.pas',
  AM.UnitReader.PrefixOptions in 'source\CommomObjects\AM.UnitReader.PrefixOptions.pas',
  AM.UnitReader.Members.CustomMember in 'source\CommomObjects\AM.UnitReader.Members.CustomMember.pas',
  AM.UnitReader.Enumerations in 'source\Commom\AM.UnitReader.Enumerations.pas',
  AM.UnitReader.Helper.WordType in 'source\Commom\Helpers\AM.UnitReader.Helper.WordType.pas',
  AM.UnitReader.Helper.MemberVisibility in 'source\Commom\Helpers\AM.UnitReader.Helper.MemberVisibility.pas',
  AM.UnitReader.Members.UnitMember in 'source\API\Members\AM.UnitReader.Members.UnitMember.pas',
  AM.UnitReader.Members.PropertyMember in 'source\API\Members\AM.UnitReader.Members.PropertyMember.pas',
  AM.UnitReader.Members.MethodMember in 'source\API\Members\AM.UnitReader.Members.MethodMember.pas',
  AM.UnitReader.Members.FieldMember in 'source\API\Members\AM.UnitReader.Members.FieldMember.pas',
  AM.UnitReader.Members.ConstantMember in 'source\API\Members\AM.UnitReader.Members.ConstantMember.pas',
  AM.UnitReader.Members.ClassMember in 'source\API\Members\AM.UnitReader.Members.ClassMember.pas',
  AM.UnitReader.Members.ArgumentMember in 'source\API\Members\AM.UnitReader.Members.ArgumentMember.pas',
  AM.UnitReader.Readers.MethodReader in 'source\API\Readers\AM.UnitReader.Readers.MethodReader.pas',
  AM.UnitReader in 'source\API\AM.UnitReader.pas',
  AM.UnitReader.Helper.MethodType in 'source\Commom\Helpers\AM.UnitReader.Helper.MethodType.pas',
  AM.UnitReader.Readers.ClassReader in 'source\API\Readers\AM.UnitReader.Readers.ClassReader.pas',
  TestMethodReader in 'TestMethodReader.pas',
  TestClassReader in 'TestClassReader.pas',
  AM.UnitReader.Utils.StrUtils in 'source\Commom\AM.UnitReader.Utils.StrUtils.pas',
  TestStrUtilsClass in 'TestStrUtilsClass.pas',
  uTestUnitReader in 'uTestUnitReader.pas',
  AM.UnitReader.Helper.MemberType in 'source\Commom\Helpers\AM.UnitReader.Helper.MemberType.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := True;
  DUnitTestRunner.RunRegisteredTests;
end.

