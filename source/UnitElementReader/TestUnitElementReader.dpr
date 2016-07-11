program TestUnitElementReader;

uses
  DUnitTestRunner,
  UnitReader.Elements in 'Source\UnitReader.Elements.pas',
  uTestLineElement in 'Source\Tests\uTestLineElement.pas',
  uTestBlockElement in 'Source\Tests\uTestBlockElement.pas',
  UnitReader.UnitElement in 'Source\UnitReader.UnitElement.pas',
  uTestUnitElement in 'Source\Tests\uTestUnitElement.pas',
  UnitReader.ClassElement in 'Source\UnitReader.ClassElement.pas',
  UnitReader.TokenTypeHelper in 'Source\UnitReader.TokenTypeHelper.pas',
  UnitReader.EnumerationTypes in 'Source\UnitReader.EnumerationTypes.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := True;
  DUnitTestRunner.RunRegisteredTests;
end.

