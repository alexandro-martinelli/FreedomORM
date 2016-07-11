unit uTestPermissao;

interface

uses
  AM.Freedom.Attributes,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.FreedomObject,
  AM.Freedom.FreedomObjectList;

type
  [Entity('PERMISSAO')]
  [Primary('ID_PERMISSAO')]
  TPermissao = class(TFreedomObject)
  strict private
    [Column('ID_PERMISSAO', [Required])]
    [Id(None)]
    [Domain('D_CODIGONN')]
    FIdPermissao: Integer;
    [Column('NOME', [], 150)]
    [Domain('D_NOMES')]
    FNome: String;
    [Column('DESCRICAO', [], 150)]
    [Domain('D_NOMES')]
    FDescricao: String;
    [Column('INDICE')]
    [Domain('D_TIPOS')]
    FIndice: Smallint;
    [Column('ID_PAI')]
    [Domain('D_CODIGO')]
    FIdPai: Integer;
    [BooleanColumn('ATIVO', 1, 0, [Required])]
    [Domain('D_ATIVO')]
    FAtivo: Boolean;
  public
    property IdPermissao: Integer read FIdPermissao write FIdPermissao;
    property Nome: String read FNome write FNome;
    property Descricao: String read FDescricao write FDescricao;
    property Indice: Smallint read FIndice write FIndice;
    property IdPai: Integer read FIdPai write FIdPai;
    property Ativo: Boolean read FAtivo write FAtivo;
  end;

  TListaPermissoes = class(TFreedomObjectList<TPermissao>);

implementation

end.