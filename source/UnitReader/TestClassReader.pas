unit TestClassReader;

interface

uses
  TestFramework,
  AM.UnitReader.Readers.CustomReader,
  AM.UnitReader.Members.FieldMember,
  AM.UnitReader.Members.ClassMember,
  AM.UnitReader.Members.MethodMember,
  AM.UnitReader.Enumerations,
  AM.UnitReader.Members.CustomMember,
  AM.UnitReader.Readers.ClassReader,
  AM.UnitReader.Members.ConstantMember,
  AM.UnitReader.Helper.MemberVisibility,
  System.Classes;

type
  TestTClassReader = class(TTestCase)
  strict private
    FClassMember: TClassMember;
    FClassSource: TStrings;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRead;
    procedure TestName;
    procedure TestVisibility;
    procedure TestFields;
    procedure TestMehods;
    procedure TestProperties;
  end;

implementation

procedure TestTClassReader.SetUp;
begin
  inherited;
  FClassSource := TStringList.Create;
  FClassSource.LoadFromFile('D:\UEntidadeClass.pas');
  FClassMember := TClassReader.ClassFromStr(FClassSource.Text, vsPublished);
end;

procedure TestTClassReader.TearDown;
begin
  FClassMember.Free;
  FClassMember := nil;
  FClassSource.Free;
end;

procedure TestTClassReader.TestFields;
begin
  CheckEquals(12, FClassMember.Fields.Count);
end;

procedure TestTClassReader.TestMehods;
begin
  CheckEquals(58, FClassMember.Methods.Count);

  CheckTrue(Assigned(FClassMember.Methods.MemberByName('CDSDadosNewRecord')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('CDSDadosReconcileError')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('CDSDadosBeforeGetRecords')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('GetVersao')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('GetKeyFields')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('LinkClientsProvider')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('PrepararBuscarPor')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('RemoverCamposRequeridos')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('RecolocarCamposRequeridos')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('GetDBEntidadeStr')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('GetTableAlias')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('GetModulo')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('GetImageType')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('GetKeyField')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('DoAfterConstruction')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('DoLinkClientsProvider')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('CriaNode')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('DoCreate')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('Buscar')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('Create')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('Instance')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('Destroy')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('NewCommand')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('Novo')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('Clonar')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('Editar')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('Consultar')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('Salvar')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('Cancelar')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('Atualizar')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('AtualizarRegistro')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('Excluir')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('PodeInserir')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('PodeEditar')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('PodeExcluir')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('PodeCancelar')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('PerguntaExcluir')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('Validar')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('AplicarAlteracoes')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('CancelaAlteracoes')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('BuscarPor')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('BuscarPrimeiros')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('BuscarLookup')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('BuscarTodos')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('BuscarAtivos')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('BuscarPorCodigo')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('NovoCodigo')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('NovoCodigo')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('IsKeyField')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('TemFilhos')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('MontaTree')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('AtualizaFilhos')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('FetchBlobs')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('FetchDetails')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('AddToTrash')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('RemoverImagem')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('AdicionarImagem')));
  CheckTrue(Assigned(FClassMember.Methods.MemberByName('RemoveFilters')));




end;

procedure TestTClassReader.TestName;
begin
  CheckEqualsString('TEntidade', FClassMember.Name);
end;

procedure TestTClassReader.TestProperties;
begin
  CheckEquals(7, FClassMember.Properties.Count);
end;

procedure TestTClassReader.TestRead;
begin
  CheckTrue(vsPublished = FClassMember.Visibility);
end;

procedure TestTClassReader.TestVisibility;
begin
  CheckTrue(vsPublished = FClassMember.Visibility);
end;

initialization

// Register any test cases with the test runner
RegisterTest(TestTClassReader.Suite);

end.
