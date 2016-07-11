inherited frmNewForeign: TfrmNewForeign
  BorderStyle = bsSingle
  Caption = 'Foreign Constraint'
  ClientHeight = 318
  ClientWidth = 537
  ExplicitWidth = 543
  ExplicitHeight = 353
  PixelsPerInch = 120
  TextHeight = 17
  object lblColumnNames: TLabel [0]
    Left = 35
    Top = 13
    Width = 104
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Column Names:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblRefColumnNames: TLabel [1]
    Left = 8
    Top = 81
    Width = 131
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Reference Columns:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblOnDelete: TLabel [2]
    Left = 303
    Top = 114
    Width = 70
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'On Delete:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblReferenceTo: TLabel [3]
    Left = 38
    Top = 46
    Width = 101
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'References To:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblOnUpdate: TLabel [4]
    Left = 63
    Top = 115
    Width = 76
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'On Update:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblForeignSQL: TLabel [5]
    Left = 56
    Top = 179
    Width = 83
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Foreign SQL:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblSchemaNames: TLabel [6]
    Left = 31
    Top = 149
    Width = 108
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Schema Names:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object cbxColumnNames: TComboBox [7]
    Left = 146
    Top = 9
    Width = 380
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TextHint = 'Typ the name of columns with coma values(;,)'
    OnChange = cbxColumnNamesChange
  end
  object cbxReferencesTo: TComboBox [8]
    Left = 146
    Top = 43
    Width = 380
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    TextHint = 'Typ the name of Schemas(blank for all)'
    OnChange = cbxColumnNamesChange
  end
  object cbxReferenceColumns: TComboBox [9]
    Left = 146
    Top = 77
    Width = 380
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    TextHint = 'Typ the name of columns with coma values(;,)'
    OnChange = cbxColumnNamesChange
  end
  object cbxOnUpdate: TComboBox [10]
    Left = 146
    Top = 111
    Width = 145
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    TextHint = 'Typ the name of Schemas(blank for all)'
    OnChange = cbxColumnNamesChange
  end
  object cbxOnDelete: TComboBox [11]
    Left = 381
    Top = 111
    Width = 145
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    TextHint = 'Typ the name of columns with coma values(;)'
    OnChange = cbxColumnNamesChange
  end
  object FlowPanel1: TFlowPanel [12]
    AlignWithMargins = True
    Left = 4
    Top = 271
    Width = 529
    Height = 43
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'FlowPanel1'
    FlowStyle = fsRightLeftTopBottom
    ShowCaption = False
    TabOrder = 7
    object btnCancel: TButton
      Left = 398
      Top = 0
      Width = 131
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object btnOK: TButton
      Left = 268
      Top = 0
      Width = 130
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&OK'
      Default = True
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnOptions: TButton
      Left = 137
      Top = 0
      Width = 131
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'O&ptions'
      TabOrder = 0
      OnClick = btnOptionsClick
    end
  end
  object mmoSQL: TMemo [13]
    Left = 146
    Top = 177
    Width = 380
    Height = 89
    TabStop = False
    Lines.Strings = (
      'Foreign Key Definition')
    ParentColor = True
    ReadOnly = True
    TabOrder = 6
  end
  object cbxSchemas: TComboBox [14]
    Left = 146
    Top = 145
    Width = 380
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    TextHint = 'Typ the name of Schemas(blank for all)'
  end
  inherited BalloonHint: TBalloonHint
    Left = 20
    Top = 183
  end
  inherited ImageList1: TImageList
    Left = 56
    Top = 184
  end
end
