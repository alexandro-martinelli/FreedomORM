inherited frmReconfigurePrefix: TfrmReconfigurePrefix
  BorderStyle = bsSingle
  Caption = 'Configure prefix'
  ClientHeight = 152
  ClientWidth = 334
  PixelsPerInch = 120
  TextHeight = 17
  object lblClassName: TLabel [0]
    Left = 22
    Top = 16
    Width = 68
    Height = 17
    Caption = 'Class name'
  end
  object lblPrefix: TLabel [1]
    Left = 56
    Top = 47
    Width = 34
    Height = 17
    Caption = 'Prefix'
  end
  object btnOK: TButton [2]
    Left = 107
    Top = 108
    Width = 100
    Height = 33
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton [3]
    Left = 213
    Top = 108
    Width = 100
    Height = 33
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object edtClassName: TEdit [4]
    Left = 96
    Top = 12
    Width = 217
    Height = 25
    TabOrder = 0
    TextHint = 'the class name'
  end
  object edtPrefix: TEdit [5]
    Left = 96
    Top = 43
    Width = 155
    Height = 25
    TabOrder = 1
    TextHint = 'the prefix for class name'
  end
  object chkIgnoreClassType: TCheckBox [6]
    Left = 48
    Top = 78
    Width = 161
    Height = 17
    Caption = 'Ignore this class type'
    TabOrder = 2
  end
  inherited BalloonHint: TBalloonHint
    Left = 244
    Top = 47
  end
  inherited ImageList1: TImageList
    Left = 280
    Top = 48
  end
end
