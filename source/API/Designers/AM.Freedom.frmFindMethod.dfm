inherited frmFindMethod: TfrmFindMethod
  BorderStyle = bsSizeToolWin
  Caption = 'Methods declaration'
  ClientHeight = 462
  ClientWidth = 554
  Constraints.MinHeight = 455
  Constraints.MinWidth = 565
  PixelsPerInch = 120
  TextHeight = 17
  object btnedtSearch: TButtonedEdit [0]
    Left = 10
    Top = 8
    Width = 534
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    TextHint = 'enter the search text in here'
    OnChange = btnedtSearchChange
    OnKeyDown = btnedtSearchKeyDown
  end
  object btnOK: TButton [1]
    Left = 340
    Top = 421
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton [2]
    Left = 446
    Top = 421
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object lvMethods: TListView [3]
    Left = 10
    Top = 39
    Width = 534
    Height = 372
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Method Name'
        Width = 100
      end
      item
        Caption = 'Class Name'
        Width = 261
      end>
    ReadOnly = True
    RowSelect = True
    SmallImages = ImageList1
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = lvMethodsDblClick
  end
  inherited BalloonHint: TBalloonHint
    Left = 20
    Top = 319
  end
  inherited ImageList1: TImageList
    Left = 56
    Top = 320
  end
end
