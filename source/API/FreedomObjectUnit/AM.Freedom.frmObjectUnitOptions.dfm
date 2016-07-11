inherited frmObjectUnitOptions: TfrmObjectUnitOptions
  BorderStyle = bsSingle
  Caption = 'Freedom Object unit options'
  ClientHeight = 118
  ClientWidth = 279
  ExplicitWidth = 285
  ExplicitHeight = 147
  PixelsPerInch = 96
  TextHeight = 13
  object lblMappingCharCase: TLabel [0]
    Left = 14
    Top = 16
    Width = 90
    Height = 13
    Caption = 'Mapping CharCase'
  end
  object lblSchemaCharCase: TLabel [1]
    Left = 14
    Top = 48
    Width = 92
    Height = 13
    Caption = 'Schemas CharCase'
  end
  object cbxMappingCharCase: TComboBox [2]
    Left = 111
    Top = 13
    Width = 145
    Height = 22
    Style = csOwnerDrawFixed
    ItemIndex = 1
    TabOrder = 0
    Text = 'UPPER CASE'
    Items.Strings = (
      'Normal'
      'UPPER CASE'
      'lower case')
  end
  object cbxSchemaCharCase: TComboBox [3]
    Left = 111
    Top = 45
    Width = 145
    Height = 22
    Style = csOwnerDrawFixed
    ItemIndex = 2
    TabOrder = 1
    Text = 'lower case'
    Items.Strings = (
      'Normal'
      'UPPER CASE'
      'lower case')
  end
  object FlowPanel1: TFlowPanel [4]
    AlignWithMargins = True
    Left = 3
    Top = 82
    Width = 273
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'FlowPanel1'
    FlowStyle = fsRightLeftTopBottom
    ShowCaption = False
    TabOrder = 2
    object btnCancel: TButton
      Left = 173
      Top = 0
      Width = 100
      Height = 33
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      Left = 73
      Top = 0
      Width = 100
      Height = 33
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
end
