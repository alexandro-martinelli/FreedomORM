inherited frmClassUnitName: TfrmClassUnitName
  BorderStyle = bsSingle
  Caption = 'Unit Name'
  ClientHeight = 179
  ClientWidth = 506
  PixelsPerInch = 120
  TextHeight = 17
  object lblUnitName: TLabel [0]
    Left = 17
    Top = 84
    Width = 63
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Unit Name'
  end
  object lblExplain: TLabel [1]
    Left = 10
    Top = 10
    Width = 317
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Type the name of then unit for class "%s".'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblExplain2: TLabel [2]
    Left = 10
    Top = 42
    Width = 472
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      'The wizard will be declared the unit name in Interface uses list' +
      '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object cbxClassUnitName: TComboBox [3]
    Left = 89
    Top = 80
    Width = 299
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
    TextHint = 'Typ the name of Unit'
  end
  object FlowPanel1: TFlowPanel [4]
    AlignWithMargins = True
    Left = 4
    Top = 133
    Width = 498
    Height = 46
    Margins.Left = 4
    Margins.Top = 0
    Margins.Right = 4
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    FlowStyle = fsRightLeftTopBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object btnCancel: TButton
      Left = 367
      Top = 0
      Width = 131
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Cancel = True
      Caption = '&Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ModalResult = 2
      ParentFont = False
      TabOrder = 1
    end
    object btnFinish: TButton
      Left = 237
      Top = 0
      Width = 130
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&Ok'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnFinishClick
    end
  end
  inherited BalloonHint: TBalloonHint
    Left = 28
    Top = 127
  end
  inherited ImageList1: TImageList
    Left = 64
    Top = 128
  end
end
