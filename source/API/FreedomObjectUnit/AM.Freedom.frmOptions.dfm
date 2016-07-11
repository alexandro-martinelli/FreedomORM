inherited frmOptions: TfrmOptions
  BorderStyle = bsSingle
  Caption = 'Freedom Object unit options'
  ClientHeight = 222
  ClientWidth = 531
  PixelsPerInch = 120
  TextHeight = 17
  object lblMappingCharCase: TLabel [0]
    Left = 72
    Top = 14
    Width = 113
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Mapping CharCase'
  end
  object lblSchemaCharCase: TLabel [1]
    Left = 69
    Top = 44
    Width = 117
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Schemas CharCase'
  end
  object lblBaseClassName: TLabel [2]
    Left = 34
    Top = 75
    Width = 148
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Default Base Class Name'
  end
  object lblListBaseClassName: TLabel [3]
    Left = 9
    Top = 105
    Width = 172
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Default List Base Class Name'
  end
  object Label1: TLabel [4]
    Left = 67
    Top = 137
    Width = 115
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Default Class Prefix'
  end
  object cbxMappingCharCase: TComboBox [5]
    Left = 196
    Top = 8
    Width = 190
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 0
    Text = 'UPPER CASE'
    Items.Strings = (
      'Normal'
      'UPPER CASE'
      'lower case')
  end
  object cbxSchemaCharCase: TComboBox [6]
    Left = 196
    Top = 39
    Width = 190
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Style = csDropDownList
    ItemIndex = 2
    TabOrder = 1
    Text = 'lower case'
    Items.Strings = (
      'Normal'
      'UPPER CASE'
      'lower case')
  end
  object FlowPanel1: TFlowPanel [7]
    AlignWithMargins = True
    Left = 4
    Top = 175
    Width = 523
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
    TabOrder = 2
    object btnCancel: TButton
      Left = 392
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
      TabOrder = 1
    end
    object btnOK: TButton
      Left = 262
      Top = 0
      Width = 130
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object edtBaseClassName: TEdit [8]
    Left = 196
    Top = 71
    Width = 322
    Height = 25
    TabOrder = 3
    TextHint = 'typ the name o the base class for new classes'
  end
  object edtListBaseClassName: TEdit [9]
    Left = 196
    Top = 102
    Width = 322
    Height = 25
    TabOrder = 4
    TextHint = 'typ the name o the list base class for new classes'
  end
  object edtClassPrefix: TEdit [10]
    Left = 196
    Top = 135
    Width = 322
    Height = 25
    TabOrder = 5
    TextHint = 'typ the new class prefix'
  end
  inherited BalloonHint: TBalloonHint
    Left = 36
    Top = 135
  end
  inherited ImageList1: TImageList
    Left = 70
    Top = 134
  end
end
