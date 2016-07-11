unit uObjectsTests;

interface

{$HINTS OFF}
{$WARNINGS OFF}

{$I FreedomORM.inc}

uses
  System.Generics.Collections,
  System.Classes,
  System.SysUtils,
  CodeSiteLogging,
  AM.Freedom.ObjectMapper.CustomMethodControl,
  AM.Freedom.ObjectMapper.CustomTrigger,
  AM.Freedom.Attributes,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Consts,
  AM.Freedom.Lazy,
  AM.Freedom.Nullable,
  AM.Freedom.FreedomObject,
  AM.Freedom.FreedomObjectList,
  AM.Freedom.LazyList,
  AM.Freedom.ObjectMapper.CustomDBObjectCursor,
  AM.Freedom.SQLMapper.CustomArgument, AM.Freedom.XML;

const
  {$IFDEF POSTGRE}
  cDefaultSchema = 'public';
  {$ELSE}
    {$IFDEF MSSQL}
    cDefaultSchema = 'dbo';
    {$ELSE}
    cDefaultSchema = '';
    {$IFEND}
  {$IFEND}

type
  TAssignControl = class(TCustomMethodControl)
  public
    procedure AfterExecute(Sender: TObject); override;
    procedure BeforeExecute(Sender: TObject); override;
    procedure OnExecuteError(Sender: TObject; const E: Exception); override;
  end;

  TTriggerDBObject = class(TCustomTrigger)
  public
    procedure BeforeDelete(AObject: TObject); override;
    procedure BeforeInsert(AObject: TObject); override;
    procedure BeforeUpdate(AObject: TObject); override;
  end;

  TTriggerProdutos = class(TCustomTrigger)
  public
    procedure AfterDelete(AObject: TObject); override;
    procedure AfterInsert(AObject: TObject); override;
    procedure AfterUpdate(AObject: TObject); override;
    procedure BeforeDelete(AObject: TObject); override;
    procedure BeforeInsert(AObject: TObject); override;
    procedure BeforeUpdate(AObject: TObject); override;
  end;

  [Trigger(TTriggerDBObject, [BeforeInsert, BeforeUpdate, BeforeDelete])]
  {$IFNDEF FIREBIRD}
  [Schema(cDefaultSchema, True)]
  [Schema('auditoria', False)]
  [Primary('ID_AUDITORIA', 'auditoria')]
  {$ENDIF}
  TDBObject = class(TFreedomObject)
  strict private
    {$IFNDEF FIREBIRD}
    [Schema('auditoria')]
    {$IFNDEF POSTGRE}
    [Domain('D_BIGID')]
    {$ENDIF}
    [Id(Identity)]
    [Column('ID_AUDITORIA', [Required])]
    FID: Int64;
    [Schema('auditoria')]
    [Domain('D_CODIGO')]
    [Column('USUARIO_ID', [Required])]
    FCodUsuario: Integer;
    [Schema('auditoria')]
    [Domain('D_DATAHORA')]
    [Column('DATA_AUDITORIA', [Required])]
    FDataAuditoria: TDateTime;
    [Schema('auditoria')]
    [Domain('D_ENUMERADO')]
    [EnumerationColumn('OPERACAO', [Required])]
    FStatus: TObjectState;
  public
    property CodUsuario: Integer read FCodUsuario write FCodUsuario;
    property DataAuditoria: TDateTime read FDataAuditoria write FDataAuditoria;
    property Status: TObjectState read FStatus write FStatus;
    {$IFEND}
  end;

  [Entity('CIDADE', 'CID')]
  [Primary('ID_CIDADE', cDefaultSchema)]
  TCidade = class(TDBObject)
  strict private
    [Column('ID_CIDADE', [Required])]
    {$IFNDEF FIREBIRD}
    [Id(Identity, '', cDefaultSchema)]
    {$ELSE}
    [Id(Sequence, 'SEQ_CIDADE')]
    {$IFEND}
    FId: Integer;
    [Column('DESCRICAO', [Required], 150)]
    [Domain('D_DESCRICAO')]
    FDescricao: String;
  public
    property Id: Integer read FId write FId;
    property Descricao: String read FDescricao write FDescricao;
  end;

  TListaCidades = class(TFreedomObjectList<TCidade>);

  [Entity('UNIDADES', 'UN')]
  [Primary('CODUNIDADE', cDefaultSchema)]
  [Unique('DESCRICAO', cDefaultSchema)]
  [Unique('RESUMIDA', cDefaultSchema)]
  TUnidade = class(TDBObject)
  private
    [Domain('D_ID')]
    {$IFDEF MSSQL}
    [Id(Identity, '', 'dbo')]
    {$ELSE}
    [Id(Sequence, 'SEQ_UNIDADES', cDefaultSchema)]
   {$ENDIF}
    [Column('CODUNIDADE', [Required])]
    FID: Integer;
    [Domain('D_DESCRICAO')]
    [Column('DESCRICAO', [Required], 150)]
    FDescricao: string;
    [Column('RESUMIDA', [Required], 5)]
    FAbreviacao: string;
    [BooleanColumn('ATIVO', 1, 0)]
    [DefaultValue(True)]
    FAtivo: Boolean;
  public
    property Id: Integer read FID write FID;
    property Descricao: string read FDescricao write FDescricao;
    property Abreviacao: string read FAbreviacao write FAbreviacao;
    property Ativo: Boolean read FAtivo write FAtivo;
  end;

  // TUnidades = class(TEntityList<TUnidade>);

  TTipoProduto = (tpProduto, tpServico);

  [Entity('TIPOSNEGOCIACAO', 'TP')]
  TTipoNegociacao = class(TFreedomObject)
  private
    [Order(1)]
    [Column('CODTIPONEGOCIACAO', [Required])]
    {$IFDEF MSSQL}
    [Id(Identity)]
    {$ELSE}
    [Id(Sequence, 'SEQ_TIPOSNEGOCIACAO')]
   {$ENDIF}
    FID: Integer;
    [Domain('D_DESCRICAO')]
    [Column('DESCRICAO', [Required], 150)]
    FDescricao: string;
  public
    property Id: Integer read FID write FID;
    property Descricao: string read FDescricao write FDescricao;
  end;

  TProduto = class;

  [Entity('PRODUTOS_MATERIAS_PRIMAS', 'MP')]
  [Primary('ID', cDefaultSchema)]
  TMateriaPrima = class(TDBObject)
  private
    [Domain('D_ID')]
    {$IFDEF MSSQL}
    [Id(Identity, '', cDefaultSchema)]
    {$ELSE}
    [Id(Sequence, 'SEQ_PRODUTOS_MATERIAS_PRIMAS', cDefaultSchema)]
    {$ENDIF}
    [Column('ID', [Required])]
    FID: Integer;
    [Column('CODPRODUTO', [Required])]
    FIDProduto: Integer;
    [Column('QTDE')]
    FQtde: Double;
    [JoinedColumn('CODMATERIA_PRIMA', 'CODPRODUTO', [NoDelete, Required], jkInner)]
    FMateriaPrima: TLazy<TProduto>;
    [JoinedColumn('CODTIPO_PRODUTO', 'CODPRODUTO', 'PRODUTOS', 'PR', '', [NoDelete, Required], jkInner)]
    FTipoProduto: TTipoProduto;
    function GetMateriaPrima: TProduto;
    procedure SetMateriaPrima(const Value: TProduto);
  public
    constructor Create; override;
    destructor Destroy; override;
    property Id: Integer read FID write FID;
    property IDProduto: Integer read FIDProduto write FIDProduto;
    property Qtde: Double read FQtde write FQtde;
    property MateriaPrima: TProduto read GetMateriaPrima write SetMateriaPrima;
  end;

  TMateriasPrimas = class(TFreedomObjectList<TMateriaPrima>);

  TEnderecoCobranca = class
  private
    [Column('ENDERECO_COB', [], 250)]
    FEndereco: String;
    [Column('NUMERO_COB')]
    [Domain('D_INTEIRO')]
    FNumero: Integer;
    [Column('ID_BAIRRO_COB')]
    [Domain('D_CHAVE_ESTRANGEIRA')]
    FIdBairro: Integer;
    [Column('ID_CIDADE_COB')]
    [Domain('D_CHAVE_ESTRANGEIRA')]
    FIdCidade: Integer;
    [JoinedColumn('ID_CIDADE_COB', 'ID_CIDADE')]
    FCidade: TLazy<TCidade>;
    function GetCidade: TCidade;
  public
    constructor Create;
    destructor Destroy; override;
    property Endereco: String read FEndereco write FEndereco;
    property Numero: Integer read FNumero write FNumero;
    property IdBairro: Integer read FIdBairro write FIdBairro;
    property IdCidade: Integer read FIdCidade write FIdCidade;
    property Cidade: TCidade read GetCidade;
  end;

  TEnderecoEntega = class
  private
    [Column('ENDERECO_ENT', [], 250)]
    FEndereco: String;
    [Column('NUMERO_ENT')]
    [Domain('D_INTEIRO')]
    FNumero: Integer;
    [Column('ID_BAIRRO_ENT')]
    [Domain('D_CHAVE_ESTRANGEIRA')]
    FIdBairro: Integer;
    [Column('ID_CIDADE_ENT')]
    [Domain('D_CHAVE_ESTRANGEIRA')]
    FIdCidade: Integer;
    [JoinedColumn('ID_CIDADE_ENT', 'ID_CIDADE')]
    FCidade: TLazy<TCidade>;
    function GetCidade: TCidade;
  public
    constructor Create;
    destructor Destroy; override;
    property Endereco: String read FEndereco write FEndereco;
    property Numero: Integer read FNumero write FNumero;
    property IdBairro: Integer read FIdBairro write FIdBairro;
    property IdCidade: Integer read FIdCidade write FIdCidade;
    property Cidade: TCidade read GetCidade;
  end;

  TEnderecos = class sealed
  private
    [Extension]
    FEnderecoEntega: TEnderecoEntega;
    [Extension]
    FEnderecoCobranca: TEnderecoCobranca;
  public
    constructor Create;
    destructor Destroy; override;
    property EnderecoEntega: TEnderecoEntega read FEnderecoEntega;
    property EnderecoCobranca: TEnderecoCobranca read FEnderecoCobranca;
  end;

  [Entity('PRODUTOS', 'P')]
  [Trigger(TTriggerProdutos, TConsts.cTriggerAll)]
  [Primary('CODPRODUTO', cDefaultSchema)]
  [Unique('DESCRICAO', cDefaultSchema)]
  [Foreign('CODUNIDADE', 'UNIDADES')]
  TProduto = class(TDBObject)
  private
    {$IFNDEF POSTGRE}
    [Domain('D_ID')]
    {$ENDIF}
    {$IFNDEF FIREBIRD}
    [Id(Identity, '', cDefaultSchema)]
    {$ELSE}
    [Id(Sequence, 'SEQ_PRODUTOS')]
    {$ENDIF}
    [Column('CODPRODUTO', [Required])]
    FCodigo: Integer;
    [Domain('D_DESCRICAO')]
    [Order(1)]
    [Column('DESCRICAO', [Required], 150)]
    FDescricao: string;
    [Column('PRECO_VENDA')]
    [Rounded(2)]
    FPrecoVenda: Double;
    [DefaultNowValue]
    [Column('DATA_CADASTRO')]
    FDataCadastro: TDate;
    [DefaultNowValue]
    [Column('HORACADASTRO')]
    FHoraCadastro: TTime;
    [Column('DATAHORACADASTRO')]
    [DefaultNowValue]
    FDataHoraCadastro: TDateTime;
    [BooleanColumn('ATIVO', 1, 0, [Required])]
    [DefaultValue(True)]
    FAtivo: Boolean;
    [EnumerationColumn('CODTIPO_PRODUTO', [Required])]
    FTipo: TTipoProduto;
    [DetailColumn('CODPRODUTO')]
    FMateriasPrimas: TLazyList<TMateriaPrima>;
    [BlobColumn('OBS', [])]
    FObservacao: TStrings;

    [JoinedColumn('CODUNIDADE', '', [NoDelete], '')]
    FUnidade: TLazy<TUnidade>;
    [BooleanColumn('ATIVO_STR', 'S', 'N', [Required])]
    [DefaultValue(True)]
    FAtivoStr: Boolean;
    {$IFNDEF FIREBIRD}
    [BooleanColumn('ATIVO_BOOL', [Required])]
    [DefaultValue(True)]
    FAtivoBool: Boolean;
    {$IFEND}
    [EnumerationColumn('TIPO_PRODUTO', 'P,S', [Required])]
    FProdutoStr: TTipoProduto;
    [JoinedColumn('RESUMIDA', 'CODUNIDADE', TUnidade)]
    FUnidadeMedida: String;
    [JoinedColumn('DESCRICAO', 'CODUNIDADE', TUnidade, '', [], jkLeft, 'DESCRICAO_UNIDADE')]
    FDescricaoUnidade: String;

    [Column('CODUNIDADE')]
    FIdUnidade: Integer;

    [Column('PRECO_VENDA_NULLABLE')]
    FPrecoVendaNullable: TNullable<Double>;
    [DefaultNowValue]
    [Column('DATA_CADASTRO_NULLABLE')]
    FDataCadastroNullable: TNullable<TDate>;
    [DefaultNowValue]
    [Column('HORACADASTRO_NULLABLE')]
    FHoraCadastroNullable: TNullable<TTime>;
    [Column('DATAHORACADASTRO_NULLABLE')]
    [DefaultNowValue]
    FDataHoraCadastroNullable: TNullable<TDateTime>;
    [BooleanColumn('ATIVO_NULLABLE', 1, 0)]
    [DefaultValue(True)]
    FAtivoNullable: TNullable<Boolean>;
    [BooleanColumn('ATIVO_STR_NULLABLE', 'S', 'N')]
    [DefaultValue(True)]
    FAtivoStrNullable: TNullable<Boolean>;
    [Column('CODUNIDADE_NULLABLE')]
    FIdUnidadeNullable: TNullable<Integer>;
    [Extension]
    FEnderecos: TEnderecos;
    [JoinedColumn('ID_CIDADE_COB', 'ID_CIDADE')]
    FCidade: TCidade;


    function GetUnidade: TUnidade;
    procedure SetUnidade(const pUnidade: TUnidade);
    function GetMateriasPrimas: TMateriasPrimas;
  public
    constructor Create; override;
    destructor Destroy; override;
    [VirtualMethodControl(TAssignControl, TConsts.cMethodAll)]
    procedure Assign(pSource: TProduto); virtual;
    property Codigo: Integer read FCodigo write FCodigo;
    property Descricao: string read FDescricao write FDescricao;
    property PrecoVenda: Double read FPrecoVenda write FPrecoVenda;
    property DataCadastro: TDate read FDataCadastro write FDataCadastro;
    property HoraCadastro: TTime read FHoraCadastro write FHoraCadastro;
    property DataHoraCadastro: TDateTime read FDataHoraCadastro write FDataHoraCadastro;
    property Ativo: Boolean read FAtivo write FAtivo;
    property Tipo: TTipoProduto read FTipo write FTipo;
    property MateriasPrimas: TMateriasPrimas read GetMateriasPrimas;
    property Observacao: TStrings read FObservacao write FObservacao;
    property IdUnidade: Integer read FIdUnidade write FIdUnidade;
    property Unidade: TUnidade read GetUnidade write SetUnidade;
    property UnidadeMedida: String read FUnidadeMedida write FUnidadeMedida;
    property AtivoStr: Boolean read FAtivoStr write FAtivoStr;
    {$IFNDEF FIREBIRD}
    property AtivoBool: Boolean read FAtivoBool write FAtivoBool;
    {$IFEND}

    property ProdutoStr: TTipoProduto read FProdutoStr write FProdutoStr;
    property PrecoVendaNullable: TNullable<Double> read FPrecoVendaNullable;
    property DataCadastroNullable: TNullable<TDate> read FDataCadastroNullable;
    property HoraCadastroNullable: TNullable<TTime> read FHoraCadastroNullable;
    property DataHoraCadastroNullable: TNullable<TDateTime> read FDataHoraCadastroNullable;
    property AtivoNullable: TNullable<Boolean> read FAtivoNullable;
    property AtivoStrNullable: TNullable<Boolean> read FAtivoStrNullable;
    property IdUnidadeNullable: TNullable<Integer> read FIdUnidadeNullable;
    property DescricaoUnidade: String read FDescricaoUnidade write FDescricaoUnidade;
    property Enderecos: TEnderecos read FEnderecos;
    property Cidade: TCidade read FCidade;
  end;

  TListaProdutos = class(TFreedomObjectList<TProduto>);


  [Entity('PRODUTOS', 'P')]
  [Trigger(TTriggerProdutos, TConsts.cTriggerAll)]
  [Primary('CODPRODUTO', cDefaultSchema)]
  [Unique('DESCRICAO', cDefaultSchema)]
  [Foreign('CODUNIDADE', 'UNIDADES')]
  TUnssignedProduto = class(TDBObject)
  private
    {$IFNDEF POSTGRE}
    [Domain('D_ID')]
    {$ENDIF}
    {$IFNDEF FIREBIRD}
    [Id(Identity, '', cDefaultSchema)]
    {$ELSE}
    [Id(Sequence, 'SEQ_PRODUTOS')]
    {$ENDIF}
    [Column('CODPRODUTO', [Required])]
    FCodigo: Integer;
    [Domain('D_DESCRICAO')]
    [Order(1)]
    [Column('DESCRICAO', [Required], 150)]
    FDescricao: string;
    [Column('PRECO_VENDA')]
    [Rounded(2)]
    FPrecoVenda: Extended;
    [Extension]
    FEnderecos: TEnderecos;
    [JoinedColumn('ID_CIDADE_COB', 'ID_CIDADE')]
    FCidade: TCidade;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Codigo: Integer read FCodigo write FCodigo;
    property Descricao: string read FDescricao write FDescricao;
    property PrecoVenda: Extended read FPrecoVenda write FPrecoVenda;
    property Enderecos: TEnderecos read FEnderecos;
    property Cidade: TCidade read FCidade;
  end;

  [Entity('PESSOA', 'PES')]
  [Primary('ID', cDefaultSchema)]
  [Unique('NOME', cDefaultSchema)]
  TPessoa = class(TDBObject)
  strict private
    [Column('ID', [Required])]
    {$IFDEF MSSQL}
    [Id(Identity, '', 'dbo')]
    {$ELSE}
    [Id(Sequence, 'SEQ_PESSOA', cDefaultSchema)]
    {$ENDIF}
    FID: Integer;
    [Column('NOME', [], 150)]
    FNome: String;
    [JoinedColumn('ID_CIDADE', '', [Required])]
    FCidade: TLazy<TCidade>;
  private
    function GetCidade: TCidade;
    procedure SetCidade(const Value: TCidade);
  public
    constructor Create; override;
    destructor Destroy; override;
    property ID: Integer read FID write FID;
    property Nome: String read FNome write FNome;
    property Cidade: TCidade read GetCidade write SetCidade;
  end;

  TCursorProdutoVenda = class(TCustomDBObjectCursor)
  strict protected
    function GetSQLCommand: TCustomCommand; override;
  end;

  [Cursor(TCursorProdutoVenda)]
  TProdutoVenda = class(TFreedomObject)
  strict private
    [Id]
    [Column('CODPRODUTO')]
    FCodigo: Integer;
    [Column('DESCRICAO')]
    FDescricao: String;
    [Column('PRECO_VENDA')]
    FPrecoVenda: Extended;
    [Column('UNIDADE')]
    FUnidade: String;
  public
    property Codigo: Integer read FCodigo write FCodigo;
    property Descricao: String read FDescricao write FDescricao;
    property PrecoVenda: Extended read FPrecoVenda write FPrecoVenda;
    property Unidade: String read FUnidade write FUnidade;
  end;

  [Entity('MOVIMENTO_HISTORICO', 'MOV_HIST')]
  [Primary('MOVIMENTO_HISTORICO_ID', cDefaultSchema)]
  TDBHistoricoMovimento = class(TDBObject)
  strict private
    [Column('MOVIMENTO_HISTORICO_ID', [Required])]
    [Id(Identity, '', 'dbo')]
    [Domain('D_ID')]
    FId: Integer;
    [Column('DATA_HORA', [Required])]
    [Domain('D_DATAHORA')]
    FDataHora: TDateTime;
    [Column('ID_USUARIO', [Required])]
    [Domain('D_CHAVE_ESTRANGEIRA')]
    FIdUsuario: Integer;
    [BlobColumn('ARQUIVO_XML', [Required])]
    FArquivoXML: TXML;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Id: Integer read FId write FId;
    property DataHora: TDateTime read FDataHora write FDataHora;
    property IdUsuario: Integer read FIdUsuario write FIdUsuario;
    property ArquivoXML: TXML read FArquivoXML write FArquivoXML;
  end;

implementation

uses
  AM.Freedom.SQLMappers.SelectClause,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMappers.Arguments;

{ TProduto }

procedure TProduto.Assign(pSource: TProduto);
begin
  // raise Exception.Create('Error Message');
end;

constructor TProduto.Create;
begin
  inherited Create;
  FObservacao := TStringList.Create;
  FUnidade := TLazy<TUnidade>.Create;
  FMateriasPrimas := TLazyList<TMateriaPrima>.Create;

  FPrecoVendaNullable := TNullable<Double>.Create;
  FDataCadastroNullable := TNullable<TDate>.Create;
  FHoraCadastroNullable := TNullable<TTime>.Create;
  FDataHoraCadastroNullable := TNullable<TDateTime>.Create;
  FAtivoNullable := TNullable<Boolean>.Create;
  FAtivoStrNullable := TNullable<Boolean>.Create;
  FIdUnidadeNullable := TNullable<Integer>.Create;
  FEnderecos := TEnderecos.Create;
  FCidade := TCidade.Create;
end;

destructor TProduto.Destroy;
begin
  FreeAndNil(FObservacao);
  FreeAndNil(FMateriasPrimas);
  FreeAndNil(FUnidade);
  FPrecoVendaNullable.Free;
  FDataCadastroNullable.Free;
  FHoraCadastroNullable.Free;
  FDataHoraCadastroNullable.Free;
  FAtivoNullable.Free;
  FAtivoStrNullable.Free;
  FIdUnidadeNullable.Free;
  FEnderecos.Free;
  FCidade.Free;
  inherited;
end;

function TProduto.GetMateriasPrimas: TMateriasPrimas;
begin
  Result := TMateriasPrimas(FMateriasPrimas.Value);
end;

function TProduto.GetUnidade: TUnidade;
begin
  Result := FUnidade.Value;
end;

procedure TProduto.SetUnidade(const pUnidade: TUnidade);
begin
  FUnidade.Value := pUnidade;
end;
{ TMateriaPrima }

constructor TMateriaPrima.Create;
begin
  inherited;
  FMateriaPrima := TLazy<TProduto>.Create;
end;

destructor TMateriaPrima.Destroy;
begin
  FreeAndNil(FMateriaPrima);
  inherited;
end;

function TMateriaPrima.GetMateriaPrima: TProduto;
begin
  Result := FMateriaPrima.Value;
end;

procedure TMateriaPrima.SetMateriaPrima(const Value: TProduto);
begin
  FMateriaPrima.Value := Value;
end;
{ TAssignControl }

procedure TAssignControl.AfterExecute(Sender: TObject);
begin
  // CodeSite.Send('After Execute');
end;

procedure TAssignControl.BeforeExecute(Sender: TObject);
begin
  // CodeSite.Send('Before Execute');
end;

procedure TAssignControl.OnExecuteError(Sender: TObject; const E: Exception);
begin
  // CodeSite.Send('On Execute Error ' + E.Message);
end;
{ TTriggerProdutos }

procedure TTriggerProdutos.AfterDelete(AObject: TObject);
begin
  // CodeSite.Send('After Delete');
end;

procedure TTriggerProdutos.AfterInsert(AObject: TObject);
begin
  // CodeSite.Send('After Insert');
end;

procedure TTriggerProdutos.AfterUpdate(AObject: TObject);
begin
  // CodeSite.Send('After Update');
end;

procedure TTriggerProdutos.BeforeDelete(AObject: TObject);
begin
  inherited;
  // CodeSite.Send('Before Delete');
end;

procedure TTriggerProdutos.BeforeInsert(AObject: TObject);
begin
  inherited;
  // CodeSite.Send('Before Insert');
end;

procedure TTriggerProdutos.BeforeUpdate(AObject: TObject);
begin
  inherited;
  // CodeSite.Send('Before Update');
end;

{ TTriggerDBObject }

procedure TTriggerDBObject.BeforeDelete(AObject: TObject);
begin
  inherited;
  {$IFNDEF FIREBIRD}
  TDBObject(AObject).CodUsuario := 1;
  TDBObject(AObject).DataAuditoria := Now;
  TDBObject(AObject).Status := TObjectState.Deleted;
  {$IFEND}
end;

procedure TTriggerDBObject.BeforeInsert(AObject: TObject);
begin
  inherited;
  {$IFNDEF FIREBIRD}
  TDBObject(AObject).CodUsuario := 1;
  TDBObject(AObject).DataAuditoria := Now;
  TDBObject(AObject).Status := TObjectState.Inserted;
  {$IFEND}
end;

procedure TTriggerDBObject.BeforeUpdate(AObject: TObject);
begin
  inherited;
  {$IFNDEF FIREBIRD}
  TDBObject(AObject).CodUsuario := 1;
  TDBObject(AObject).DataAuditoria := Now;
  TDBObject(AObject).Status := TObjectState.Clean;
  {$IFEND}
end;

{ TEnderecoCobranca }

constructor TEnderecoCobranca.Create;
begin
  FCidade := TLazy<TCidade>.Create;
end;

destructor TEnderecoCobranca.Destroy;
begin
  FCidade.Free;
  inherited;
end;

function TEnderecoCobranca.GetCidade: TCidade;
begin
  Result := FCidade.Value;
end;

{ TEnderecos }

constructor TEnderecos.Create;
begin
  FEnderecoEntega := TEnderecoEntega.Create;
  FEnderecoCobranca := TEnderecoCobranca.Create;
end;

destructor TEnderecos.Destroy;
begin
  FEnderecoCobranca.Free;
  FEnderecoEntega.Free;
  inherited;
end;

{ TEnderecoEntega }

constructor TEnderecoEntega.Create;
begin
  FCidade := TLazy<TCidade>.Create;
end;

destructor TEnderecoEntega.Destroy;
begin
  FCidade.Free;
  inherited;
end;

function TEnderecoEntega.GetCidade: TCidade;
begin
  Result := FCidade.Value;
end;

{ TUnssignedProduto }

constructor TUnssignedProduto.Create;
begin
  inherited;
  FEnderecos := TEnderecos.Create;
  FCidade := TCidade.Create;
end;

destructor TUnssignedProduto.Destroy;
begin
  FEnderecos.Free;
  FCidade.Free;
  inherited;
end;

{ TCursorProdutoVenda }

function TCursorProdutoVenda.GetSQLCommand: TCustomCommand;
var
  lSelect: TSelectClause;
begin
  lSelect := TSelectClause.CreateFromTable('PRODUTOS', 'P');
  lSelect.Field('CODPRODUTO', 'P');
  lSelect.Field('DESCRICAO', 'P');
  lSelect.Field('PRECO_VENDA', 'P');
  lSelect.Field('RESUMIDA', 'UN', 'UNIDADE');
  lSelect.JoinTable('UNIDADES', [TCriteria.CreateAsEqual(TFieldArgument.Create('CODUNIDADE', 'UN'), TFieldArgument.Create('CODUNIDADE', 'P'))], 'UN', jkLeft);
  if (GroupCriteria <> nil) then
  begin
    lSelect.AssignWhere(GroupCriteria);
  end;
  lSelect.Where(TCriteria.CreateAsEqual(TFieldArgument.Create('ATIVO', 'P'), TValueArgument.CreateAsInteger(1)));
  lSelect.Where(TCriteria.CreateAsGreaterThan(TFieldArgument.Create('PRECO_VENDA', 'P'), TValueArgument.CreateAsExtended(0)));
  lSelect.OrderBy('P.CODPRODUTO', 1);
  Result := lSelect;
end;

{ TDBHistoricoMovimento }

constructor TDBHistoricoMovimento.Create;
begin
  inherited;
  FArquivoXML := TXML.Create;
end;

destructor TDBHistoricoMovimento.Destroy;
begin
  FArquivoXML.Free;
  inherited;
end;

{ TPessoa }

constructor TPessoa.Create;
begin
  inherited;
  FCidade := TLazy<TCidade>.Create;
end;

destructor TPessoa.Destroy;
begin
  FCidade.Free;
  inherited;
end;

function TPessoa.GetCidade: TCidade;
begin
  Result := FCidade.Value;
end;

procedure TPessoa.SetCidade(const Value: TCidade);
begin
  FCidade.Value := Value;
end;

end.

