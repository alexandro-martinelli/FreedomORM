unit uTestUnitElement;

interface

uses
  TestFramework,
  System.Classes,
  UnitReader.Elements,
  UnitReader.UnitElement,
  System.SysUtils;

type
  TestTUnitElement = class(TTestCase)
  strict private
    FUnitElement: TUnitElement;
    function GetFileNameForTest: String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadFromFile;
    procedure TestTokenTypes;
    procedure TestLoadFreedomAttributes;
    procedure TestLoadFreedomObjectList;
    procedure TestLoadFreedomKeyBindBoard;
    procedure TestVariableExistsFreedomKeyBindBoard;
    procedure TestBeginMethodImplementationFreedomBindKeyBoard;
    procedure TestReadExtractVariableKeyExecutor;
    procedure TestReadExtractConstantKeyExecutor;
    procedure TestReadExtractConstantKeyExecutorFromText;
    procedure TestConstantDeclarationInExtractConstantKeyExecutor;
    procedure TestConstantClassDeclarationInExtractConstantKeyExecutor;
    procedure TestConstantDeclarationExistsInExtractConstantKeyExecutor;
    procedure TestClassDeclarationOnLineInfrmFindMethod;
    procedure TestReadDclFreedomORMConfig;
  end;

implementation

uses
  AM.UnitReader.Enumerations,
  UnitReader.EnumerationTypes, UnitReader.ClassElement;

function TestTUnitElement.GetFileNameForTest: String;
begin
  Result := 'D:\Alex\Delphi\FreedomORM\branches\source\CommomObjects\AM.Freedom.CustomDBPersistent.pas';
end;

procedure TestTUnitElement.SetUp;
begin
  FUnitElement := TUnitElement.Create;
end;

procedure TestTUnitElement.TearDown;
begin
  FUnitElement.Free;
  FUnitElement := nil;
end;

procedure TestTUnitElement.TestBeginMethodImplementationFreedomBindKeyBoard;
var
  lLineNumber: Integer;
begin
  FUnitElement.LoadFromFile('D:\Alex\Delphi\FreedomORM\branches\source\API\BindKeyBoard\AM.Freedom.FreedomBindKeyBoard.pas');
  lLineNumber := FUnitElement.BeginMethodImplementationBeforeLineIndex(106);
  CheckEquals(74, lLineNumber);
end;

procedure TestTUnitElement.TestClassDeclarationOnLineInfrmFindMethod;
var
  lClassDeclaration: TClassElement;
begin
  FUnitElement.LoadFromFile('D:\Alex\Delphi\FreedomORM\branches\source\API\Designers\AM.Freedom.frmFindMethod.pas');
  lClassDeclaration := FUnitElement.ClassDeclarationOnLineNumber(57);
  try
    CheckTrue(Assigned(lClassDeclaration));
  finally
    lClassDeclaration.Free;
  end;
end;

procedure TestTUnitElement.TestConstantClassDeclarationInExtractConstantKeyExecutor;
var
  lConst: TConstantDeclarationResult;
begin
  FUnitElement.LoadFromFile('D:\Alex\Delphi\FreedomORM\branches\source\API\BindKeyBoard\AM.Freedom.ExtractConstantKeyExecutor.pas');
  lConst := FUnitElement.ConstantClassDeclaration(56, vsStrictProtected);
  try
    CheckEquals(22, lConst.LineNumber);
    CheckTrue(lConst.IsDeclaration);
  finally
    lConst.Free;
  end;
end;

procedure TestTUnitElement.TestConstantDeclarationExistsInExtractConstantKeyExecutor;
var
  lExists: Boolean;
begin
  FUnitElement.LoadFromFile('D:\Alex\Delphi\FreedomORM\branches\source\API\BindKeyBoard\AM.Freedom.ExtractConstantKeyExecutor.pas');
  lExists := FUnitElement.ConstantDeclarationExistsBeforeLineIndex(58, 'cEmptyStr');
  CheckTrue(lExists);
end;

procedure TestTUnitElement.TestConstantDeclarationInExtractConstantKeyExecutor;
var
  lConst: TConstantDeclarationResult;
begin
  FUnitElement.LoadFromFile('D:\Alex\Delphi\FreedomORM\branches\source\API\BindKeyBoard\AM.Freedom.ExtractConstantKeyExecutor.pas');
  lConst := FUnitElement.ConstantDeclarationBeforeLineIndex(99);
  try
    CheckEquals(97, lConst.LineNumber);
    CheckFalse(lConst.IsDeclaration);
  finally
    lConst.Free;
  end;

  lConst := FUnitElement.ConstantDeclarationBeforeLineIndex(52);
  try
    CheckEquals(44, lConst.LineNumber);
    CheckTrue(lConst.IsDeclaration);
  finally
    lConst.Free;
  end;


end;

procedure TestTUnitElement.TestLoadFreedomAttributes;
var
  lLine: TLineElement;
begin
  FUnitElement.LoadFromFile('D:\Alex\Delphi\FreedomORM\branches\source\API\ObjectMappers\AM.Freedom.Attributes.pas');
  CheckEquals(416, FUnitElement.Count);
  lLine := FUnitElement.LineByLineNumber(1);
  CheckTrue(lLine.TokenType = tkUnitSection);
  lLine := FUnitElement.LineByLineNumber(3);
  CheckTrue(lLine.TokenType = tkInterfaceSection);
  lLine := FUnitElement.LineByLineNumber(5);
  CheckTrue(lLine.TokenType = tkUsesSection);
  lLine := FUnitElement.LineByLineNumber(16);
  CheckTrue(lLine.TokenType = tkTypeSection);
  lLine := FUnitElement.LineByLineNumber(23);
  CheckTrue(lLine.TokenType = tkClassDeclaration);
  CheckTrue(lLine.Parent.TokenType = tkTypeSection);
  lLine := FUnitElement.LineByLineNumber(25);
  CheckTrue(lLine.TokenType = tkClassDeclaration);
  CheckTrue(lLine.Parent.TokenType = tkTypeSection);
  lLine := FUnitElement.LineByLineNumber(27);
  CheckTrue(lLine.TokenType = tkClassDeclaration);
  CheckTrue(lLine.Parent.TokenType = tkTypeSection);
  lLine := FUnitElement.LineByLineNumber(29);
  CheckTrue(lLine.TokenType = tkClassDeclaration);
  CheckTrue(lLine.Parent.TokenType = tkTypeSection);
  lLine := FUnitElement.LineByLineNumber(31);
  CheckTrue(lLine.TokenType = tkClassDeclaration);
  CheckTrue(lLine.Parent.TokenType = tkTypeSection);

end;

procedure TestTUnitElement.TestLoadFreedomKeyBindBoard;
var
  lVariable: TVariableDeclarationResult;
begin
  FUnitElement.LoadFromFile('D:\Alex\Delphi\FreedomORM\branches\source\API\BindKeyBoard\AM.Freedom.FreedomBindKeyBoard.pas');
//  CheckEquals(173, FUnitElement.Count);
//  lVariable := FUnitElement.VariableDeclarationBeforeLineIndex(100);
//  try
//    CheckEquals(90, lVariable.LineNumber);
//  finally
//    lVariable.Free;
//  end;
  lVariable := FUnitElement.VariableDeclarationBeforeLineIndex(20);
  try
    CheckEquals(0, lVariable.LineNumber);
  finally
    lVariable.Free;
  end;


end;

procedure TestTUnitElement.TestLoadFreedomObjectList;
var
  lLine: TLineElement;
begin
  FUnitElement.LoadFromFile('D:\Alex\Delphi\FreedomORM\branches\source\API\ObjectMappers\AM.Freedom.FreedomObjectList.pas');
  CheckEquals(1393, FUnitElement.Count);
  lLine := FUnitElement.LineByLineNumber(1);
  CheckTrue(lLine.TokenType = tkUnitSection);
  lLine := FUnitElement.LineByLineNumber(3);
  CheckTrue(lLine.TokenType = tkInterfaceSection);
  lLine := FUnitElement.LineByLineNumber(5);
  CheckTrue(lLine.TokenType = tkUsesSection);
  lLine := FUnitElement.LineByLineNumber(31);
  CheckTrue(lLine.TokenType = tkTypeSection);
  lLine := FUnitElement.LineByLineNumber(34);
  CheckTrue(lLine.TokenType = tkClassDeclaration);
  CheckTrue(lLine.Parent.TokenType = tkClassDeclaration);

end;

procedure TestTUnitElement.TestLoadFromFile;
begin
  FUnitElement.LoadFromFile(GetFileNameForTest);
  CheckEqualsString(FUnitElement.LoadedUnitName, 'AM.Freedom.CustomDBPersistent.pas');
  CheckEquals(1191, FUnitElement.Count);
  FUnitElement.SaveToFile(ExtractFilePath(ParamStr(0)) + 'TestUnitReader.txt');
end;

procedure TestTUnitElement.TestReadDclFreedomORMConfig;
begin
  FUnitElement.LoadFromFile('D:\Alex\Delphi\FreedomORM\branches\source\API\Designers\AM.Freedom.dclFreedomORMConfig.pas');
end;

procedure TestTUnitElement.TestReadExtractConstantKeyExecutor;
begin
  FUnitElement.LoadFromFile('D:\Alex\Delphi\FreedomORM\branches\source\API\BindKeyBoard\AM.Freedom.ExtractConstantKeyExecutor.pas');
end;

procedure TestTUnitElement.TestReadExtractConstantKeyExecutorFromText;
var
  lStrings: TStrings;
begin
  lStrings := TStringList.Create;
  try
    lStrings.Add('unit AM.Freedom.ExtractConstantKeyExecutor;');
    lStrings.Add('');
    lStrings.Add('interface');
    lStrings.Add('');
    lStrings.Add('uses');
    lStrings.Add('  System.SysUtils,');
    lStrings.Add('  System.Classes,');
    lStrings.Add('  AM.Freedom.FreedomBindKeyBoard,');
    lStrings.Add('  AM.Freedom.frmDeclareVariable,');
    lStrings.Add('  UnitReader.UnitElement,');
    lStrings.Add('  ToolsAPI;');
    lStrings.Add('');
    lStrings.Add('type');
    lStrings.Add('  TExtractConstantKeyExecutor = class(TCustomKeyExecutor)');
    lStrings.Add('  private');
    lStrings.Add('    FLine: Integer;');
    lStrings.Add('    FStartPos: TOTAEditPos;');
    lStrings.Add('    FContext: IOTAKeyContext;');
    lStrings.Add('    FUnitElement: TUnitElement;');
    lStrings.Add('    FEditPosition: IOTAEditPosition;');
    lStrings.Add('  protected');
    lStrings.Add('    procedure Execute(const pContext: IOTAKeyContext); override;');
    lStrings.Add('    function GetShortCut: TShortCut; override;');
    lStrings.Add('  end;');
    lStrings.Add('');
    lStrings.Add('');
    lStrings.Add('implementation');
    lStrings.Add('');
    lStrings.Add('{ TExtractConstantKeyExecutor }');
    lStrings.Add('');
    lStrings.Add('procedure TExtractConstantKeyExecutor.Execute(const pContext: IOTAKeyContext);');
    lStrings.Add('begin');
    lStrings.Add('  FContext := pContext;');
    lStrings.Add('  FEditPosition := FContext.EditBuffer.EditPosition;');
    lStrings.Add('  FStartPos := FContext.EditBuffer.TopView.CursorPos;');
    lStrings.Add('  try');
    lStrings.Add('    lConstValue := ExtractIdentifierAtCursor(FContext, lCanContinue);');
    lStrings.Add('');
    lStrings.Add('  finally');
    lStrings.Add('    GoBackToPos(FContext, FStartPos);');
    lStrings.Add('  end;');
    lStrings.Add('end;');
    lStrings.Add('');
    lStrings.Add('function TExtractConstantKeyExecutor.GetShortCut: TShortCut;');
    lStrings.Add('begin');
    lStrings.Add('');
    lStrings.Add('end;');
    lStrings.Add('');
    lStrings.Add('end.');
    FUnitElement.LoadFromText(lStrings.Text);

  finally
    lStrings.Free;
  end;
end;

procedure TestTUnitElement.TestReadExtractVariableKeyExecutor;
var
  lVariableDeclaration: TVariableDeclarationResult;
begin
  FUnitElement.LoadFromFile('D:\Alex\Delphi\FreedomORM\branches\source\API\BindKeyBoard\AM.Freedom.ExtractVariableKeyExecutor.pas');
  lVariableDeclaration := FUnitElement.VariableDeclarationBeforeLineIndex(144);
  try
    CheckEquals(129, lVariableDeclaration.LineNumber);
    CheckTrue(lVariableDeclaration.IsDeclaration);
  finally
    lVariableDeclaration.Free;
  end;
end;

procedure TestTUnitElement.TestTokenTypes;
var
  lLine: TLineElement;
begin
  FUnitElement.LoadFromFile(GetFileNameForTest);
  lLine := FUnitElement.LineByLineNumber(1);
  CheckTrue(lLine.TokenType = tkUnitSection);
  lLine := FUnitElement.LineByLineNumber(3);
  CheckTrue(lLine.TokenType = tkInterfaceSection);
  lLine := FUnitElement.LineByLineNumber(5);
  CheckTrue(lLine.TokenType = tkUsesSection);
  lLine := FUnitElement.LineByLineNumber(43);
  CheckTrue(lLine.TokenType = tkTypeSection);
  lLine := FUnitElement.LineByLineNumber(50);
  CheckTrue(lLine.TokenType = tkVisibilityEscope);

  lLine := FUnitElement.LineByLineNumber(51);
  CheckTrue(lLine.TokenType = tkField);
  lLine := FUnitElement.LineByLineNumber(52);
  CheckTrue(lLine.TokenType = tkField);
  lLine := FUnitElement.LineByLineNumber(53);
  CheckTrue(lLine.TokenType = tkField);
  lLine := FUnitElement.LineByLineNumber(54);
  CheckTrue(lLine.TokenType = tkField);
  lLine := FUnitElement.LineByLineNumber(55);
  CheckTrue(lLine.TokenType = tkField);
  lLine := FUnitElement.LineByLineNumber(56);
  CheckTrue(lLine.TokenType = tkField);
  lLine := FUnitElement.LineByLineNumber(57);
  CheckTrue(lLine.TokenType = tkField);

  lLine := FUnitElement.LineByLineNumber(58);
  CheckTrue(lLine.TokenType = tkProcedureDeclaration);
  lLine := FUnitElement.LineByLineNumber(72);
  CheckTrue(lLine.TokenType = tkProcedureDeclaration);

  lLine := FUnitElement.LineByLineNumber(91);
  CheckTrue(lLine.TokenType = tkVisibilityEscope);
  lLine := FUnitElement.LineByLineNumber(106);
  CheckTrue(lLine.TokenType = tkVisibilityEscope);
  lLine := FUnitElement.LineByLineNumber(113);
  CheckTrue(lLine.TokenType = tkProcedureDeclaration);
  lLine := FUnitElement.LineByLineNumber(117);
  CheckTrue(lLine.TokenType = tkVisibilityEscope);

  lLine := FUnitElement.LineByLineNumber(118);
  CheckTrue(lLine.TokenType = tkConstructorDeclaration);
  lLine := FUnitElement.LineByLineNumber(119);
  CheckTrue(lLine.TokenType = tkDestrutorDeclaration);
  lLine := FUnitElement.LineByLineNumber(132);
  CheckTrue(lLine.TokenType = tkProperty);
  lLine := FUnitElement.LineByLineNumber(135);
  CheckTrue(lLine.TokenType = tkProperty);
  lLine := FUnitElement.LineByLineNumber(138);
  CheckTrue(lLine.TokenType = tkImplementationSection);
  lLine := FUnitElement.LineByLineNumber(142);
  CheckTrue(lLine.TokenType = tkUsesSection);


  lLine := FUnitElement.LineByLineNumber(161);
  CheckTrue(lLine.TokenType = tkFunctionImplementation);
  lLine := FUnitElement.LineByLineNumber(171);
  CheckTrue(lLine.TokenType = tkFunctionImplementation);

  lLine := FUnitElement.LineByLineNumber(166);
  CheckTrue(lLine.TokenType = tkProcedureImplementation);
  lLine := FUnitElement.LineByLineNumber(318);
  CheckTrue(lLine.TokenType = tkProcedureImplementation);
end;

procedure TestTUnitElement.TestVariableExistsFreedomKeyBindBoard;
var
  lExists: Boolean;
begin
  FUnitElement.LoadFromFile('D:\Alex\Delphi\FreedomORM\branches\source\API\BindKeyBoard\AM.Freedom.FreedomBindKeyBoard.pas');
  lExists := FUnitElement.VariableDeclarationExistsBeforeLineIndex(20, 'cStringDelimiters');
  CheckTrue(lExists);
end;

initialization
  RegisterTest(TestTUnitElement.Suite);

end.
