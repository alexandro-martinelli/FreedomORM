unit uTestBlockElement;

interface

uses
  TestFramework,
  System.Classes,
  UnitReader.Elements,
  System.Generics.Collections,
  UnitReader.EnumerationTypes,
  System.SysUtils;

type
  TestTBlockElement = class(TTestCase)
  strict private
    FBlock: TBlockElement;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddLineFunc;
    procedure TestAddLineProc;
    procedure TestAddBlockFunc;
    procedure TestAddBlockProc;
  end;

implementation

procedure TestTBlockElement.SetUp;
begin
  FBlock := TBlockElement.Create;
end;

procedure TestTBlockElement.TearDown;
begin
  FBlock.Free;
  FBlock := nil;
end;

procedure TestTBlockElement.TestAddLineFunc;
begin
  CheckEquals(0, FBlock.Lines.Count);
  FBlock.AddLine;
  CheckEquals(1, FBlock.Lines.Count);
end;

procedure TestTBlockElement.TestAddLineProc;
var
  lLine: TLineElement;
begin
  lLine := TLineElement.Create;
  CheckEquals(0, FBlock.Lines.Count);
  FBlock.AddLine(lLine);
  CheckEquals(1, FBlock.Lines.Count);
end;

procedure TestTBlockElement.TestAddBlockFunc;
begin
  CheckEquals(0, FBlock.Lines.Count);
  FBlock.AddBlock;
  CheckEquals(1, FBlock.Lines.Count);
end;

procedure TestTBlockElement.TestAddBlockProc;
var
  lBlock: TBlockElement;
begin
  lBlock := TBlockElement.Create;
  CheckEquals(0, FBlock.Lines.Count);
  FBlock.AddBlock(lBlock);
  CheckEquals(1, FBlock.Lines.Count);
end;

initialization
  RegisterTest(TestTBlockElement.Suite);

end.
