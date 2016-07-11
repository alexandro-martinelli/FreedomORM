inherited frmConfigurePrefix: TfrmConfigurePrefix
  ActiveControl = edtPrefixName
  BorderStyle = bsSingle
  Caption = 'Configure prefix'
  ClientHeight = 127
  ClientWidth = 438
  PixelsPerInch = 120
  TextHeight = 17
  object lblClassName: TLabel [0]
    Left = 35
    Top = 21
    Width = 68
    Height = 17
    Caption = 'Class name'
  end
  object Label2: TLabel [1]
    Left = 31
    Top = 52
    Width = 72
    Height = 17
    Caption = 'Prefix name'
  end
  object cbxClassName: TComboBox [2]
    Left = 112
    Top = 17
    Width = 305
    Height = 25
    Style = csDropDownList
    TabOrder = 0
  end
  object edtPrefixName: TEdit [3]
    Left = 112
    Top = 48
    Width = 161
    Height = 25
    TabOrder = 1
  end
  object btnOK: TButton [4]
    Left = 211
    Top = 84
    Width = 100
    Height = 33
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton [5]
    Left = 317
    Top = 84
    Width = 100
    Height = 33
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  inherited BalloonHint: TBalloonHint
    Left = 84
    Top = 87
  end
  inherited ImageList1: TImageList
    Left = 120
    Top = 88
  end
end
