object frmTestFreedomObjectDataSet: TfrmTestFreedomObjectDataSet
  Left = 0
  Top = 0
  Caption = 'Test FreedomObjectDataSet'
  ClientHeight = 601
  ClientWidth = 1348
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 120
  TextHeight = 16
  object pgcData: TPageControl
    Left = 0
    Top = 37
    Width = 1348
    Height = 564
    ActivePage = tsGrids
    Align = alClient
    TabOrder = 0
    object tsGrids: TTabSheet
      Caption = 'Grids'
      object DBGrid1: TDBGrid
        Left = 0
        Top = 41
        Width = 1340
        Height = 335
        Align = alClient
        DataSource = dtsProdutos
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -13
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'Codigo'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Descricao'
            Width = 250
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'PrecoVenda'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'DataCadastro'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'HoraCadastro'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'DataHoraCadastro'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Ativo'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Tipo'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'MateriasPrimas'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Observacao'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'IdUnidade'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'UnidadeMedida'
            Width = 50
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'AtivoStr'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'ProdutoStr'
            Visible = True
          end>
      end
      object DBGrid2: TDBGrid
        Left = 0
        Top = 376
        Width = 1340
        Height = 157
        Align = alBottom
        DataSource = dtsMateriasPrimas
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -13
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'Id'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'IDProduto'
            Width = 89
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Qtde'
            Width = 234
            Visible = True
          end>
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 1340
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        object Label12: TLabel
          Left = 7
          Top = 10
          Width = 55
          Height = 16
          Caption = 'Descri'#231#227'o'
        end
        object edtFiltroDescricao: TEdit
          Left = 68
          Top = 7
          Width = 269
          Height = 24
          TabOrder = 0
          OnChange = edtFiltroDescricaoChange
        end
      end
    end
    object tsDataAware: TTabSheet
      Caption = 'Data Aware'
      ImageIndex = 1
      DesignSize = (
        1340
        533)
      object Label1: TLabel
        Left = 5
        Top = 35
        Width = 39
        Height = 16
        Caption = 'Codigo'
        FocusControl = cxDBSpinEdit1
      end
      object Label2: TLabel
        Left = 5
        Top = 83
        Width = 55
        Height = 16
        Caption = 'Descricao'
        FocusControl = cxDBTextEdit1
      end
      object Label3: TLabel
        Left = 5
        Top = 131
        Width = 68
        Height = 16
        Caption = 'PrecoVenda'
        FocusControl = cxDBCalcEdit1
      end
      object Label4: TLabel
        Left = 232
        Top = 35
        Width = 77
        Height = 16
        Caption = 'DataCadastro'
        FocusControl = cxDBDateEdit1
      end
      object Label5: TLabel
        Left = 359
        Top = 35
        Width = 78
        Height = 16
        Caption = 'HoraCadastro'
        FocusControl = cxDBTimeEdit1
      end
      object Label6: TLabel
        Left = 486
        Top = 35
        Width = 104
        Height = 16
        Caption = 'DataHoraCadastro'
        FocusControl = cxDBDateEdit2
      end
      object Label7: TLabel
        Left = 5
        Top = 179
        Width = 25
        Height = 16
        Caption = 'Tipo'
        FocusControl = DBEdit2
      end
      object Label8: TLabel
        Left = 3
        Top = 406
        Width = 67
        Height = 16
        Anchors = [akLeft, akBottom]
        Caption = 'Observacao'
        FocusControl = cxDBMemo1
      end
      object Label9: TLabel
        Left = 132
        Top = 131
        Width = 57
        Height = 16
        Caption = 'IdUnidade'
        FocusControl = cxDBSpinEdit2
      end
      object Label10: TLabel
        Left = 259
        Top = 131
        Width = 87
        Height = 16
        Caption = 'UnidadeMedida'
        FocusControl = cxDBTextEdit2
      end
      object Label11: TLabel
        Left = 175
        Top = 179
        Width = 61
        Height = 16
        Caption = 'ProdutoStr'
        FocusControl = DBEdit1
      end
      object cxDBSpinEdit1: TDBEdit
        Left = 5
        Top = 51
        Width = 121
        Height = 24
        DataField = 'Codigo'
        DataSource = dtsProdutos
        TabOrder = 0
      end
      object cxDBTextEdit1: TDBEdit
        Left = 5
        Top = 99
        Width = 602
        Height = 24
        DataField = 'Descricao'
        DataSource = dtsProdutos
        TabOrder = 1
      end
      object cxDBCalcEdit1: TDBEdit
        Left = 5
        Top = 147
        Width = 121
        Height = 24
        DataField = 'PrecoVenda'
        DataSource = dtsProdutos
        TabOrder = 2
      end
      object cxDBDateEdit1: TDBEdit
        Left = 232
        Top = 51
        Width = 121
        Height = 24
        DataField = 'DataCadastro'
        DataSource = dtsProdutos
        TabOrder = 3
      end
      object cxDBTimeEdit1: TDBEdit
        Left = 359
        Top = 51
        Width = 121
        Height = 24
        DataField = 'HoraCadastro'
        DataSource = dtsProdutos
        TabOrder = 4
      end
      object cxDBDateEdit2: TDBEdit
        Left = 486
        Top = 51
        Width = 121
        Height = 24
        DataField = 'DataHoraCadastro'
        DataSource = dtsProdutos
        TabOrder = 5
      end
      object cxDBCheckBox1: TDBCheckBox
        Left = 152
        Top = 55
        Width = 57
        Height = 17
        Caption = 'Ativo'
        DataField = 'Ativo'
        DataSource = dtsProdutos
        TabOrder = 6
      end
      object DBEdit2: TDBEdit
        Left = 5
        Top = 195
        Width = 164
        Height = 24
        DataField = 'Tipo'
        DataSource = dtsProdutos
        TabOrder = 7
      end
      object cxDBMemo1: TDBMemo
        Left = 0
        Top = 426
        Width = 1340
        Height = 107
        Align = alBottom
        DataField = 'Observacao'
        DataSource = dtsProdutos
        ScrollBars = ssVertical
        TabOrder = 8
      end
      object cxDBSpinEdit2: TDBEdit
        Left = 132
        Top = 147
        Width = 121
        Height = 24
        DataField = 'IdUnidade'
        DataSource = dtsProdutos
        TabOrder = 9
      end
      object cxDBTextEdit2: TDBEdit
        Left = 259
        Top = 147
        Width = 121
        Height = 24
        DataField = 'UnidadeMedida'
        DataSource = dtsProdutos
        TabOrder = 10
      end
      object cxDBCheckBox2: TDBCheckBox
        Left = 345
        Top = 199
        Width = 121
        Height = 17
        Caption = 'AtivoStr'
        DataField = 'AtivoStr'
        DataSource = dtsProdutos
        TabOrder = 11
      end
      object DBEdit1: TDBEdit
        Left = 175
        Top = 195
        Width = 164
        Height = 24
        DataField = 'ProdutoStr'
        DataSource = dtsProdutos
        TabOrder = 12
      end
      object btnPost: TButton
        Left = 3
        Top = 3
        Width = 75
        Height = 25
        Caption = 'Post'
        TabOrder = 13
        OnClick = btnPostClick
      end
      object btnCancel: TButton
        Left = 84
        Top = 3
        Width = 75
        Height = 25
        Caption = 'Cancel'
        TabOrder = 14
        OnClick = btnCancelClick
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1348
    Height = 37
    Align = alTop
    TabOrder = 1
    object btnFirst: TButton
      Left = 166
      Top = 6
      Width = 75
      Height = 25
      Caption = 'First'
      TabOrder = 0
      OnClick = btnFirstClick
    end
    object btnPrior: TButton
      Left = 244
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Prior'
      TabOrder = 1
      OnClick = btnPriorClick
    end
    object btnNext: TButton
      Left = 322
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Next'
      TabOrder = 2
      OnClick = btnNextClick
    end
    object btnLast: TButton
      Left = 400
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Last'
      TabOrder = 3
      OnClick = btnLastClick
    end
    object btnRefresh: TButton
      Left = 478
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 4
      OnClick = btnRefreshClick
    end
    object btnOpen: TButton
      Left = 10
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Open'
      TabOrder = 5
      OnClick = btnOpenClick
    end
    object btnClose: TButton
      Left = 88
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 6
      OnClick = btnCloseClick
    end
    object btnCloneCurrentRecord: TButton
      Left = 556
      Top = 6
      Width = 157
      Height = 25
      Caption = 'Clone Current Record'
      TabOrder = 7
      OnClick = btnCloneCurrentRecordClick
    end
    object btnCancelUpdates: TButton
      Left = 1142
      Top = 6
      Width = 113
      Height = 25
      Caption = 'Cancel Updates'
      TabOrder = 8
      OnClick = btnCancelUpdatesClick
    end
    object btnDelete: TButton
      Left = 950
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 9
      OnClick = btnDeleteClick
    end
    object btnApplyUpdates: TButton
      Left = 1028
      Top = 6
      Width = 111
      Height = 25
      Caption = 'Apply Updates'
      TabOrder = 10
      OnClick = btnApplyUpdatesClick
    end
    object btnEdit: TButton
      Left = 872
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Edit'
      TabOrder = 11
      OnClick = btnEditClick
    end
    object btnInsert: TButton
      Left = 794
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Insert'
      TabOrder = 12
      OnClick = btnInsertClick
    end
    object btnAppend: TButton
      Left = 716
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Append'
      TabOrder = 13
      OnClick = btnAppendClick
    end
  end
  object odsProdutos: TFreedomObjectDataset
    FieldDefs = <
      item
        Name = 'Codigo'
        DataType = ftInteger
      end
      item
        Name = 'Descricao'
        DataType = ftString
        Size = 250
      end
      item
        Name = 'PrecoVenda'
        DataType = ftFloat
      end
      item
        Name = 'DataCadastro'
        DataType = ftDate
      end
      item
        Name = 'HoraCadastro'
        DataType = ftTime
      end
      item
        Name = 'DataHoraCadastro'
        DataType = ftDateTime
      end
      item
        Name = 'Ativo'
        DataType = ftBoolean
      end
      item
        Name = 'Observacao'
        DataType = ftWideMemo
      end
      item
        Name = 'IdUnidade'
        DataType = ftInteger
      end
      item
        Name = 'UnidadeMedida'
        DataType = ftString
        Size = 250
      end
      item
        Name = 'AtivoStr'
        DataType = ftBoolean
      end
      item
        Name = 'AtivoBool'
        DataType = ftBoolean
      end
      item
        Name = 'DescricaoUnidade'
        DataType = ftString
        Size = 250
      end
      item
        Name = 'Tipo'
        DataType = ftInteger
      end
      item
        Name = 'MateriasPrimas'
        DataType = ftDataSet
      end
      item
        Name = 'ProdutoStr'
        DataType = ftInteger
      end
      item
        Name = 'PrecoVendaNullable'
        DataType = ftFloat
      end
      item
        Name = 'DataCadastroNullable'
        DataType = ftDate
      end
      item
        Name = 'HoraCadastroNullable'
        DataType = ftTime
      end
      item
        Name = 'DataHoraCadastroNullable'
        DataType = ftDateTime
      end
      item
        Name = 'AtivoNullable'
        DataType = ftBoolean
      end
      item
        Name = 'AtivoStrNullable'
        DataType = ftBoolean
      end
      item
        Name = 'IdUnidadeNullable'
        DataType = ftInteger
      end
      item
        Name = 'Enderecos'
        DataType = ftDataSet
      end>
    ObjectClassName = 'uObjectsTests.TProduto'
    Left = 776
    Top = 304
    object odsProdutosCodigo: TIntegerField
      FieldName = 'Codigo'
    end
    object odsProdutosDescricao: TStringField
      FieldName = 'Descricao'
      Size = 250
    end
    object odsProdutosPrecoVenda: TFloatField
      FieldName = 'PrecoVenda'
    end
    object odsProdutosDataCadastro: TDateField
      FieldName = 'DataCadastro'
    end
    object odsProdutosHoraCadastro: TTimeField
      FieldName = 'HoraCadastro'
    end
    object odsProdutosDataHoraCadastro: TDateTimeField
      FieldName = 'DataHoraCadastro'
    end
    object odsProdutosAtivo: TBooleanField
      FieldName = 'Ativo'
    end
    object odsProdutosObservacao: TWideMemoField
      FieldName = 'Observacao'
      BlobType = ftWideMemo
    end
    object odsProdutosIdUnidade: TIntegerField
      FieldName = 'IdUnidade'
    end
    object odsProdutosUnidadeMedida: TStringField
      FieldName = 'UnidadeMedida'
      Size = 250
    end
    object odsProdutosAtivoStr: TBooleanField
      FieldName = 'AtivoStr'
    end
    object odsProdutosAtivoBool: TBooleanField
      FieldName = 'AtivoBool'
    end
    object odsProdutosDescricaoUnidade: TStringField
      FieldName = 'DescricaoUnidade'
      Size = 250
    end
    object odsProdutosTipo: TIntegerField
      FieldName = 'Tipo'
    end
    object odsProdutosMateriasPrimas: TDataSetField
      FieldName = 'MateriasPrimas'
    end
    object odsProdutosProdutoStr: TIntegerField
      FieldName = 'ProdutoStr'
    end
    object odsProdutosPrecoVendaNullable: TFloatField
      FieldName = 'PrecoVendaNullable'
    end
    object odsProdutosDataCadastroNullable: TDateField
      FieldName = 'DataCadastroNullable'
    end
    object odsProdutosHoraCadastroNullable: TTimeField
      FieldName = 'HoraCadastroNullable'
    end
    object odsProdutosDataHoraCadastroNullable: TDateTimeField
      FieldName = 'DataHoraCadastroNullable'
    end
    object odsProdutosAtivoNullable: TBooleanField
      FieldName = 'AtivoNullable'
    end
    object odsProdutosAtivoStrNullable: TBooleanField
      FieldName = 'AtivoStrNullable'
    end
    object odsProdutosIdUnidadeNullable: TIntegerField
      FieldName = 'IdUnidadeNullable'
    end
    object odsProdutosEnderecos: TDataSetField
      FieldName = 'Enderecos'
    end
  end
  object dtsProdutos: TDataSource
    AutoEdit = False
    DataSet = odsProdutos
    OnStateChange = dtsProdutosStateChange
    Left = 808
    Top = 304
  end
  object dtsMateriasPrimas: TDataSource
    DataSet = odsMateriasPrimas
    Left = 872
    Top = 304
  end
  object odsMateriasPrimas: TFreedomObjectDataset
    FieldDefs = <
      item
        Name = 'Id'
        DataType = ftInteger
      end
      item
        Name = 'IDProduto'
        DataType = ftInteger
      end
      item
        Name = 'Qtde'
        DataType = ftFloat
      end>
    ObjectClassName = 'TMateriaPrima'
    Left = 840
    Top = 304
    object odsMateriasPrimasId: TIntegerField
      FieldName = 'Id'
    end
    object odsMateriasPrimasIDProduto: TIntegerField
      FieldName = 'IDProduto'
    end
    object odsMateriasPrimasQtde: TFloatField
      FieldName = 'Qtde'
    end
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select *'
      'from produtos')
    Left = 728
    Top = 304
    object FDQuery1arquivo_XML: TFDXMLField
      FieldName = 'arquivo_XML'
      Origin = 'arquivo_XML'
      BlobType = ftDBaseOle
    end
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=alexandro'
      'User_Name=sa'
      'Password=masterkey'
      'MetaCaseIns=True'
      'Server=.'
      'DriverID=MSSQL')
    LoginPrompt = False
    Left = 724
    Top = 344
  end
end
