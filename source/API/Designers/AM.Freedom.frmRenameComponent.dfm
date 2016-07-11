inherited frmRenameComponent: TfrmRenameComponent
  BorderStyle = bsSingle
  Caption = 'Rename Component'
  ClientHeight = 231
  ClientWidth = 334
  PixelsPerInch = 120
  TextHeight = 17
  object lblName: TLabel [0]
    Left = 56
    Top = 54
    Width = 35
    Height = 17
    Caption = 'Name'
  end
  object lblNewName: TLabel [1]
    Left = 27
    Top = 85
    Width = 64
    Height = 17
    Caption = 'New name'
  end
  object lblClassType: TLabel [2]
    Left = 29
    Top = 22
    Width = 62
    Height = 17
    Caption = 'Class type'
  end
  object lblCaption: TLabel [3]
    Left = 44
    Top = 116
    Width = 47
    Height = 17
    Caption = 'Caption'
  end
  object edtName: TEdit [4]
    Left = 104
    Top = 50
    Width = 209
    Height = 25
    TabStop = False
    Color = 12369084
    ReadOnly = True
    TabOrder = 1
  end
  object edtNewName: TEdit [5]
    Left = 104
    Top = 81
    Width = 209
    Height = 25
    TabOrder = 2
    OnChange = edtNewNameChange
    OnEnter = edtNewNameEnter
  end
  object chkIgnoreClassType: TCheckBox [6]
    Left = 29
    Top = 153
    Width = 204
    Height = 17
    Caption = 'Ignore this class type'
    TabOrder = 4
  end
  object edtClassType: TEdit [7]
    Left = 104
    Top = 19
    Width = 209
    Height = 25
    TabStop = False
    Color = 12369084
    ReadOnly = True
    TabOrder = 0
  end
  object btnOK: TButton [8]
    Left = 106
    Top = 189
    Width = 100
    Height = 33
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = btnOKClick
  end
  object btnCancel: TButton [9]
    Left = 212
    Top = 189
    Width = 100
    Height = 33
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object cbxCaption: TComboBox [10]
    Left = 104
    Top = 112
    Width = 209
    Height = 25
    TabOrder = 3
    OnEnter = edtNewNameEnter
  end
  inherited BalloonHint: TBalloonHint
    Left = 236
    Top = 144
  end
  inherited ImageList1: TImageList
    Left = 274
    Top = 145
  end
end
