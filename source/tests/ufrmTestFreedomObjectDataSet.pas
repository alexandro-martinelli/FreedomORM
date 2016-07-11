unit ufrmTestFreedomObjectDataSet;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Data.DB,
  AM.Freedom.FreedomObjectDataset,
  uObjectsTests,
  Vcl.StdCtrls,
  Vcl.Mask,
  Vcl.DBCtrls,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FireDAC.Stan.Async,
  FireDAC.DApt,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  FireDAC.UI.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Phys,
  FireDAC.Phys.MSSQL,
  FireDAC.Phys.MSSQLDef,
  Data.DBXMSSQL,
  Data.SqlExpr,
  AM.Freedom.GroupFilterCriteria;

type
  TfrmTestFreedomObjectDataSet = class(TForm)
    odsProdutos: TFreedomObjectDataset;
    dtsProdutos: TDataSource;
    dtsMateriasPrimas: TDataSource;
    odsMateriasPrimas: TFreedomObjectDataset;
    odsMateriasPrimasId: TIntegerField;
    odsMateriasPrimasIDProduto: TIntegerField;
    odsMateriasPrimasQtde: TFloatField;
    pgcData: TPageControl;
    tsGrids: TTabSheet;
    tsDataAware: TTabSheet;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    Panel1: TPanel;
    btnFirst: TButton;
    btnPrior: TButton;
    btnNext: TButton;
    btnLast: TButton;
    btnRefresh: TButton;
    btnOpen: TButton;
    btnClose: TButton;
    Label1: TLabel;
    cxDBSpinEdit1: TDBEdit;
    Label2: TLabel;
    cxDBTextEdit1: TDBEdit;
    Label3: TLabel;
    cxDBCalcEdit1: TDBEdit;
    Label4: TLabel;
    cxDBDateEdit1: TDBEdit;
    Label5: TLabel;
    cxDBTimeEdit1: TDBEdit;
    Label6: TLabel;
    cxDBDateEdit2: TDBEdit;
    cxDBCheckBox1: TDBCheckBox;
    Label7: TLabel;
    DBEdit2: TDBEdit;
    Label8: TLabel;
    cxDBMemo1: TDBMemo;
    Label9: TLabel;
    cxDBSpinEdit2: TDBEdit;
    Label10: TLabel;
    cxDBTextEdit2: TDBEdit;
    cxDBCheckBox2: TDBCheckBox;
    Label11: TLabel;
    DBEdit1: TDBEdit;
    btnPost: TButton;
    btnCancel: TButton;
    btnCloneCurrentRecord: TButton;
    btnCancelUpdates: TButton;
    btnDelete: TButton;
    btnApplyUpdates: TButton;
    btnEdit: TButton;
    btnInsert: TButton;
    btnAppend: TButton;
    Panel2: TPanel;
    Label12: TLabel;
    edtFiltroDescricao: TEdit;
    FDQuery1: TFDQuery;
    FDConnection1: TFDConnection;
    FDQuery1arquivo_XML: TFDXMLField;
    odsProdutosCodigo: TIntegerField;
    odsProdutosDescricao: TStringField;
    odsProdutosPrecoVenda: TFloatField;
    odsProdutosDataCadastro: TDateField;
    odsProdutosHoraCadastro: TTimeField;
    odsProdutosDataHoraCadastro: TDateTimeField;
    odsProdutosAtivo: TBooleanField;
    odsProdutosObservacao: TWideMemoField;
    odsProdutosIdUnidade: TIntegerField;
    odsProdutosUnidadeMedida: TStringField;
    odsProdutosAtivoStr: TBooleanField;
    odsProdutosAtivoBool: TBooleanField;
    odsProdutosDescricaoUnidade: TStringField;
    odsProdutosTipo: TIntegerField;
    odsProdutosMateriasPrimas: TDataSetField;
    odsProdutosProdutoStr: TIntegerField;
    odsProdutosPrecoVendaNullable: TFloatField;
    odsProdutosDataCadastroNullable: TDateField;
    odsProdutosHoraCadastroNullable: TTimeField;
    odsProdutosDataHoraCadastroNullable: TDateTimeField;
    odsProdutosAtivoNullable: TBooleanField;
    odsProdutosAtivoStrNullable: TBooleanField;
    odsProdutosIdUnidadeNullable: TIntegerField;
    odsProdutosEnderecos: TDataSetField;
    procedure btnOpenClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnPriorClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAppendClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnPostClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnApplyUpdatesClick(Sender: TObject);
    procedure btnCancelUpdatesClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnCloneCurrentRecordClick(Sender: TObject);
    procedure dtsProdutosStateChange(Sender: TObject);
    procedure edtFiltroDescricaoChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    class procedure DoTest;
  end;

implementation

{$R *.dfm}

procedure TfrmTestFreedomObjectDataSet.btnAppendClick(Sender: TObject);
begin
  odsProdutos.Append;
end;

procedure TfrmTestFreedomObjectDataSet.btnApplyUpdatesClick(Sender: TObject);
begin
  odsProdutos.ApplyUpdates;
end;

procedure TfrmTestFreedomObjectDataSet.btnCancelClick(Sender: TObject);
begin
  odsProdutos.Cancel;
end;

procedure TfrmTestFreedomObjectDataSet.btnCancelUpdatesClick(Sender: TObject);
begin
  odsProdutos.CancelUpdates;
end;

procedure TfrmTestFreedomObjectDataSet.btnCloneCurrentRecordClick(Sender: TObject);
begin
  odsProdutos.CloneCurrentRecord([], True);
end;

procedure TfrmTestFreedomObjectDataSet.btnCloseClick(Sender: TObject);
begin
  odsProdutos.Close;
end;

procedure TfrmTestFreedomObjectDataSet.btnDeleteClick(Sender: TObject);
begin
  odsProdutos.Delete;
end;

procedure TfrmTestFreedomObjectDataSet.btnEditClick(Sender: TObject);
begin
  odsProdutos.Edit;
end;

procedure TfrmTestFreedomObjectDataSet.btnOpenClick(Sender: TObject);
begin
  odsProdutos.Open;
end;

procedure TfrmTestFreedomObjectDataSet.btnPostClick(Sender: TObject);
begin
  odsProdutos.Post;
end;

procedure TfrmTestFreedomObjectDataSet.btnPriorClick(Sender: TObject);
begin
  odsProdutos.Prior;
end;

procedure TfrmTestFreedomObjectDataSet.btnRefreshClick(Sender: TObject);
begin
  odsProdutos.Refresh;
end;

class procedure TfrmTestFreedomObjectDataSet.DoTest;
var
  lfrmTestFreedomObjectDataSet: TfrmTestFreedomObjectDataSet;
begin
  lfrmTestFreedomObjectDataSet := TfrmTestFreedomObjectDataSet.Create(nil);
  try
    lfrmTestFreedomObjectDataSet.ShowModal;
  finally
    lfrmTestFreedomObjectDataSet.Free;
  end;
end;

procedure TfrmTestFreedomObjectDataSet.dtsProdutosStateChange(Sender: TObject);
begin
  if (dtsProdutos.State in dsEditModes) then
  begin
    tsDataAware.Show;
  end
  else
  begin
    tsGrids.Show;
  end;
end;

procedure TfrmTestFreedomObjectDataSet.edtFiltroDescricaoChange(Sender: TObject);
begin
  odsProdutos.Filter := 'Descricao like(%' + edtFiltroDescricao.Text + '%)';
  odsProdutos.Filtered := edtFiltroDescricao.Text <> '';
end;

procedure TfrmTestFreedomObjectDataSet.FormCreate(Sender: TObject);
begin
  odsProdutos.ObjectClass := TProduto;
end;

procedure TfrmTestFreedomObjectDataSet.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  begin
    Close;
  end;
end;

procedure TfrmTestFreedomObjectDataSet.btnFirstClick(Sender: TObject);
begin
  odsProdutos.First;
end;

procedure TfrmTestFreedomObjectDataSet.btnInsertClick(Sender: TObject);
begin
  odsProdutos.Insert;
end;

procedure TfrmTestFreedomObjectDataSet.btnLastClick(Sender: TObject);
begin
  odsProdutos.Last;
end;

procedure TfrmTestFreedomObjectDataSet.btnNextClick(Sender: TObject);
begin
  odsProdutos.Next;
end;

end.
