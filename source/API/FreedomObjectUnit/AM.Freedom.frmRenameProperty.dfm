inherited frmRenameProperty: TfrmRenameProperty
  BorderStyle = bsSingle
  Caption = 'Remane Property'
  ClientHeight = 128
  ClientWidth = 454
  ExplicitWidth = 460
  ExplicitHeight = 163
  PixelsPerInch = 120
  TextHeight = 17
  object lblOldName: TLabel [0]
    Left = 24
    Top = 19
    Width = 63
    Height = 17
    Caption = 'Old name:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblNewName: TLabel [1]
    Left = 18
    Top = 54
    Width = 69
    Height = 17
    Caption = 'New name:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object edtOldName: TEdit [2]
    Left = 95
    Top = 16
    Width = 350
    Height = 25
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object edtNewName: TEdit [3]
    Left = 95
    Top = 51
    Width = 350
    Height = 25
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnChange = edtNewNameChange
  end
  object btnOK: TButton [4]
    Left = 231
    Top = 88
    Width = 100
    Height = 33
    Caption = 'OK'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Tahoma'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 2
    OnClick = btnOKClick
  end
  object BtnCancel: TButton [5]
    Left = 337
    Top = 88
    Width = 100
    Height = 33
    Cancel = True
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Tahoma'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 3
  end
  inherited BalloonHint: TBalloonHint
    Left = 268
    Top = 7
  end
  inherited ImageList1: TImageList
    Left = 304
    Top = 8
  end
end
