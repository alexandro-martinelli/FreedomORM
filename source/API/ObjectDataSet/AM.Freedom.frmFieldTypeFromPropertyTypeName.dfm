inherited frmFieldTypeFromPropertyTypeName: TfrmFieldTypeFromPropertyTypeName
  ActiveControl = cbxFieldTypeNames
  BorderStyle = bsSingle
  Caption = 'Field type selector'
  ClientHeight = 188
  ClientWidth = 619
  PixelsPerInch = 120
  TextHeight = 17
  object lblUnitName: TLabel [0]
    Left = 17
    Top = 108
    Width = 61
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Field Type'
  end
  object lblExplain: TLabel [1]
    Left = 10
    Top = 10
    Width = 600
    Height = 56
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    AutoSize = False
    Caption = 'Select the FieldType for property "%s" type name "%s".'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lblExplain2: TLabel [2]
    Left = 10
    Top = 74
    Width = 365
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'The wizard will be declared the Field of this type.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object cbxFieldTypeNames: TComboBox [3]
    Left = 89
    Top = 104
    Width = 286
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Style = csDropDownList
    TabOrder = 0
    TextHint = 'Select the field type name'
  end
  object FlowPanel1: TFlowPanel [4]
    AlignWithMargins = True
    Left = 4
    Top = 142
    Width = 611
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
      Left = 480
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
      Left = 350
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
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
    end
  end
  inherited BalloonHint: TBalloonHint
    Left = 428
    Top = 92
  end
  inherited ImageList1: TImageList
    Left = 464
    Top = 93
  end
end
