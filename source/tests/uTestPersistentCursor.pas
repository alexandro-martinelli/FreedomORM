unit uTestPersistentCursor;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  System.Generics.Collections,
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.EnumerationTypes;

type
  TestTPersistentCursor = class(TTestCase)
  strict private
    FPersistentCursor: TPersistentCursor;
    procedure PreencherCursor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddColumn;
    procedure TestFindColumn;
    procedure TestColumnByName;
    procedure TestFirst;
    procedure TestAddRecord;
    procedure TestNext;
    procedure TestRecordCount;
    procedure TestEof;
    procedure TestBof;
    procedure TestValuesByColumnName;
    procedure TestValuesByColumnIndex;
  end;

implementation

uses
  AM.Freedom.Helper.Variant;

procedure TestTPersistentCursor.PreencherCursor;
begin
  FPersistentCursor.AddColumn(TCursorColumn.Create('ID', ctyInteger, 0, 0));
  FPersistentCursor.AddColumn(TCursorColumn.Create('NOME', ctyString, 150, 1));
  FPersistentCursor.AddColumn(TCursorColumn.Create('SOBRENOME', ctyString, 150, 2));
  FPersistentCursor.AddRecord(TRecord.Create([1, 'Alexandro', 'Martinelli'], 0));
  FPersistentCursor.AddRecord(TRecord.Create([2, 'Dorival', 'Martinelli'], 1));
  FPersistentCursor.AddRecord(TRecord.Create([3, 'Ana', 'D. Martinelli'], 2));
end;

procedure TestTPersistentCursor.SetUp;
begin
  FPersistentCursor := TPersistentCursor.Create;
end;

procedure TestTPersistentCursor.TearDown;
begin
  FPersistentCursor.Free;
  FPersistentCursor := nil;
end;

procedure TestTPersistentCursor.TestFindColumn;
var
  pColumn: TCursorColumn;
begin
  pColumn := TCursorColumn.Create('NOME', ctyString, 150, 1);
  FPersistentCursor.AddColumn(pColumn);
  Check(FPersistentCursor.FindColumn(pColumn));
end;

procedure TestTPersistentCursor.TestColumnByName;
begin
  FPersistentCursor.AddColumn(TCursorColumn.Create('NOME', ctyString, 150, 1));
  Check(Assigned(FPersistentCursor.ColumnByName('NOME')));
end;

procedure TestTPersistentCursor.TestEof;
var
  lCount: Integer;
begin
  PreencherCursor;
  FPersistentCursor.First;
  lCount := 0;
  while not FPersistentCursor.Eof do begin
    Inc(lCount);
    FPersistentCursor.Next;
  end;
  Check(FPersistentCursor.Eof);
  CheckEquals(3, lCount);
end;

procedure TestTPersistentCursor.TestAddRecord;
begin
  PreencherCursor;
  FPersistentCursor.First;
  CheckEquals(1, FPersistentCursor.Values[0]);
  CheckEquals('Alexandro', FPersistentCursor.Values[1]);
  CheckEquals('Martinelli', FPersistentCursor.Values[2]);
end;

procedure TestTPersistentCursor.TestBof;
begin
  PreencherCursor;
  FPersistentCursor.First;
  Check(FPersistentCursor.Bof);
  CheckEquals(1, FPersistentCursor.RecNo);
end;

procedure TestTPersistentCursor.TestAddColumn;
begin
  CheckEquals(0, FPersistentCursor.ColumnCount);
  FPersistentCursor.AddColumn(TCursorColumn.Create('FONE', ctyString, 20, 1));
  CheckEquals(1, FPersistentCursor.ColumnCount);
end;

procedure TestTPersistentCursor.TestFirst;
begin
  PreencherCursor;
  FPersistentCursor.First;
  CheckEquals(1, FPersistentCursor.RecNo);
end;

procedure TestTPersistentCursor.TestNext;
var
  lRecNo: Word;
begin
  PreencherCursor;
  FPersistentCursor.First;
  lRecNo := FPersistentCursor.RecNo;
  FPersistentCursor.Next;
  CheckEquals(lRecNo + 1, FPersistentCursor.RecNo);
end;

procedure TestTPersistentCursor.TestRecordCount;
begin
  PreencherCursor;
  CheckEquals(3, FPersistentCursor.RecordCount);
end;

procedure TestTPersistentCursor.TestValuesByColumnIndex;
var
  lNome: String;
  lId: Integer;
begin
  PreencherCursor;
  FPersistentCursor.First;
  lNome := FPersistentCursor.Values[1];
  lId := FPersistentCursor.Values[0];
  CheckEqualsString('Alexandro', lNome);
  CheckEquals(1, lId);

  FPersistentCursor.Next;
  lNome := FPersistentCursor.Values[1];
  lId := FPersistentCursor.Values[0];
  CheckEqualsString('Dorival', lNome);
  CheckEquals(2, lId);

  FPersistentCursor.Next;
  lNome := FPersistentCursor.Values[1];
  lId := FPersistentCursor.Values[0];
  CheckEqualsString('Ana', lNome);
  CheckEquals(3, lId);
end;

procedure TestTPersistentCursor.TestValuesByColumnName;
var
  lNome, lSobreNome: String;
  lId: Integer;
begin
  PreencherCursor;
  FPersistentCursor.First;
  lNome := FPersistentCursor.Values['NOME'];
  lSobreNome := FPersistentCursor.Values['SOBRENOME'];
  lId := FPersistentCursor.Values['ID'];
  CheckEqualsString('Alexandro', lNome);
  CheckEqualsString('Martinelli', lSobreNome);
  CheckEquals(1, lId);

  FPersistentCursor.Next;
  lNome := FPersistentCursor.Values['NOME'];
  lSobreNome := FPersistentCursor.Values['SOBRENOME'];
  lId := FPersistentCursor.Values['ID'];
  CheckEqualsString('Dorival', lNome);
  CheckEqualsString('Martinelli', lSobreNome);
  CheckEquals(2, lId);

  FPersistentCursor.Next;
  lNome := FPersistentCursor.Values['NOME'].ToString;
  lSobreNome := FPersistentCursor.Values['SOBRENOME'].ToString;
  lId := FPersistentCursor.Values['ID'];
  CheckEqualsString('Ana', lNome);
  CheckEqualsString('D. Martinelli', lSobreNome);
  CheckEquals(3, lId);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTPersistentCursor.Suite);
end.

