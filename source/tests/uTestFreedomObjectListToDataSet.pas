unit uTestFreedomObjectListToDataSet;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  System.SysUtils,
  AM.Freedom.FreedomObjectList,
  System.Generics.Collections,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.FreedomObjectListToDataSet,
  DataSnap.DBClient,
  AM.Freedom.FreedomObject,
  AM.Freedom.ObjectMapper,
  Data.DB,
  uTestPermissao;

type
  TestTFreedomObjectListToDataSet = class(TTestCase)
  private
    {$IFDEF FIREBIRD}
    procedure DoOpenDataSet(pDataSet: TDataSet);
    {$IFEND}
  published
    procedure TestObjectListToDataSet;
  end;

implementation

uses
  ufrmTestFreedomObjectListToDataSet;

{$IFDEF FIREBIRD}
procedure TestTFreedomObjectListToDataSet.DoOpenDataSet(pDataSet: TDataSet);
begin
  TClientDataSet(pDataSet).CreateDataSet;
end;
{$IFEND}

procedure TestTFreedomObjectListToDataSet.TestObjectListToDataSet;
{$IFDEF FIREBIRD}
var
  lDataSet: TClientDataSet;
  lListaObjetos: TListaPermissoes;
{$IFEND}
begin
  {$IFDEF FIREBIRD}
    lListaObjetos := TListaPermissoes.Create;
    lDataSet := nil;
    try
      lDataSet := TClientDataSet.Create(nil);
      lListaObjetos.GetAllObjects;
      TFreedomObjectListToDataSet<TPermissao>.ObjectListToDataSet(lListaObjetos, lDataSet, DoOpenDataSet);
      lDataSet.IndexFieldNames := 'IdPermissao';
      TfrmTestFreedomObjectListToDataSet.ShowData(lDataSet);
    finally
      lListaObjetos.Free;
      FreeAndNil(lDataSet);
    end;
  {$IFEND}
end;

initialization
  // Register any test cases with the test runner
 // RegisterTest(TestTFreedomObjectListToDataSet.Suite);
end.

