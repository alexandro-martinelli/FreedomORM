inherited frmViewRefactorings: TfrmViewRefactorings
  BorderStyle = bsSizeToolWin
  Caption = 'View Refactorings'
  ClientHeight = 443
  ClientWidth = 432
  Constraints.MinHeight = 490
  Constraints.MinWidth = 450
  PixelsPerInch = 120
  TextHeight = 17
  object btnedtSearch: TButtonedEdit [0]
    Left = 8
    Top = 8
    Width = 415
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    TextHint = 'insert the search text here'
    OnChange = btnedtSearchChange
    OnKeyDown = btnedtSearchKeyDown
  end
  object lvRefactorings: TListView [1]
    Left = 8
    Top = 39
    Width = 416
    Height = 354
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        AutoSize = True
        Caption = 'Refactoring'
      end>
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = lvRefactoringsDblClick
  end
  object btnOK: TButton [2]
    Left = 218
    Top = 402
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton [3]
    Left = 324
    Top = 402
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  inherited BalloonHint: TBalloonHint
    Left = 316
    Top = 295
  end
  inherited ImageList1: TImageList
    Left = 352
    Top = 296
  end
end
