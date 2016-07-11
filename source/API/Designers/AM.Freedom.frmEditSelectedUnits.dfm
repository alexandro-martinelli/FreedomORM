inherited frmEditSelectedUnits: TfrmEditSelectedUnits
  BorderStyle = bsSizeToolWin
  Caption = 'Selected units'
  ClientHeight = 373
  ClientWidth = 352
  Constraints.MinHeight = 420
  Constraints.MinWidth = 370
  PixelsPerInch = 120
  TextHeight = 17
  object lblHint: TLabel [0]
    Left = 8
    Top = 8
    Width = 305
    Height = 17
    Caption = '* Press Delete ( Del ) to remove the selected units'
  end
  object lbxUnits: TListBox [1]
    Left = 8
    Top = 33
    Width = 336
    Height = 290
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 17
    MultiSelect = True
    TabOrder = 0
    OnKeyDown = lbxUnitsKeyDown
  end
  object btnOK: TButton [2]
    Left = 138
    Top = 332
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton [3]
    Left = 244
    Top = 332
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  inherited BalloonHint: TBalloonHint
    Left = 76
    Top = 111
  end
  inherited ImageList1: TImageList
    Left = 112
    Top = 112
  end
end
