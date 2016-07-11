inherited frmDisplayFieldsEditor: TfrmDisplayFieldsEditor
  ActiveControl = cbxDisplayLabel
  Caption = 'Display fields Editor'
  ClientHeight = 192
  ClientWidth = 320
  PixelsPerInch = 120
  TextHeight = 17
  object lblClassName: TLabel [0]
    Left = 43
    Top = 20
    Width = 68
    Height = 17
    Caption = 'Class name'
  end
  object lblName: TLabel [1]
    Left = 76
    Top = 51
    Width = 35
    Height = 17
    Caption = 'Name'
  end
  object lblDisplayLabel: TLabel [2]
    Left = 33
    Top = 82
    Width = 78
    Height = 17
    Caption = 'Display Label'
  end
  object lblDisplayFormat: TLabel [3]
    Left = 24
    Top = 113
    Width = 88
    Height = 17
    Caption = 'Display format'
  end
  object edtClassName: TEdit [4]
    Left = 120
    Top = 16
    Width = 180
    Height = 25
    TabStop = False
    Color = 12369084
    ReadOnly = True
    TabOrder = 0
  end
  object edtName: TEdit [5]
    Left = 120
    Top = 47
    Width = 180
    Height = 25
    TabStop = False
    Color = 12369084
    ReadOnly = True
    TabOrder = 1
  end
  object cbxDisplayLabel: TComboBox [6]
    Left = 120
    Top = 78
    Width = 180
    Height = 25
    TabOrder = 2
  end
  object cbxDisplayFormat: TComboBox [7]
    Left = 120
    Top = 109
    Width = 180
    Height = 25
    TabOrder = 3
  end
  object btnOK: TButton [8]
    Left = 106
    Top = 148
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = btnOKClick
  end
  object btnCancel: TButton [9]
    Left = 212
    Top = 148
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  inherited BalloonHint: TBalloonHint
    Left = 180
    Top = 39
  end
  inherited ImageList1: TImageList
    Left = 216
    Top = 40
  end
end
