inherited frmUnitNames: TfrmUnitNames
  BorderStyle = bsSizeToolWin
  Caption = 'Unit names'
  ClientHeight = 443
  ClientWidth = 622
  Constraints.MinHeight = 490
  Constraints.MinWidth = 570
  KeyPreview = True
  PixelsPerInch = 120
  TextHeight = 17
  object btnedtSearch: TButtonedEdit [0]
    Left = 8
    Top = 8
    Width = 605
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    TextHint = 'insert the search text here'
    OnChange = btnedtSearchChange
    OnKeyDown = btnedtSearchKeyDown
  end
  object lvUnits: TListView [1]
    Left = 8
    Top = 39
    Width = 606
    Height = 350
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'File name'
        Width = 400
      end
      item
        Caption = 'Project name'
        Width = 200
      end>
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = ppmUnits
    ShowColumnHeaders = False
    SmallImages = ImageList1
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = lvUnitsDblClick
  end
  object btnOK: TButton [2]
    Left = 408
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
    Left = 514
    Top = 402
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object chkDeclareOnInterface: TCheckBox [4]
    Left = 11
    Top = 397
    Width = 194
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Declare on interface'
    TabOrder = 4
  end
  object chkDeclareOnNewLine: TCheckBox [5]
    Left = 11
    Top = 419
    Width = 194
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Declare on new line'
    TabOrder = 5
  end
  object btnAddToList: TButton [6]
    Left = 281
    Top = 402
    Width = 121
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'Add to &list'
    Style = bsSplitButton
    TabOrder = 6
    OnClick = btnAddToListClick
    OnDropDownClick = btnAddToListDropDownClick
  end
  inherited BalloonHint: TBalloonHint
    Left = 188
    Top = 303
  end
  inherited ImageList1: TImageList
    Left = 224
    Top = 304
  end
  object ppmUnits: TPopupMenu
    Left = 264
    Top = 304
    object miHideSystemUnits: TMenuItem
      AutoCheck = True
      Caption = 'Hide system units'
      OnClick = miHideSystemUnitsClick
    end
    object miProjectSeparator: TMenuItem
      Caption = '-'
    end
  end
end
