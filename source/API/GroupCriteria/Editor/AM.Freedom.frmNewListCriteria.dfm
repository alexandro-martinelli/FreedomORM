inherited frmNewListCriteria: TfrmNewListCriteria
  BorderStyle = bsSizeToolWin
  Caption = 'New Group Criteria'
  ClientHeight = 547
  ClientWidth = 993
  Constraints.MinHeight = 452
  Constraints.MinWidth = 830
  KeyPreview = True
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 120
  TextHeight = 17
  object flpnlButtons: TFlowPanel [0]
    AlignWithMargins = True
    Left = 3
    Top = 502
    Width = 987
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'flpnlButtons'
    FlowStyle = fsBottomTopRightLeft
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    object btnCancel: TButton
      Left = 857
      Top = -1
      Width = 130
      Height = 43
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      Left = 727
      Top = -1
      Width = 130
      Height = 43
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object pnlCriteriasLIst: TPanel [1]
    Left = 0
    Top = 0
    Width = 993
    Height = 499
    Align = alClient
    ParentColor = True
    TabOrder = 1
    DesignSize = (
      993
      499)
    object lblCriteriasList: TLabel
      Left = 8
      Top = 6
      Width = 72
      Height = 17
      Caption = 'Criterias List'
    end
    object tvCriterias: TTreeView
      Left = 211
      Top = 6
      Width = 775
      Height = 489
      Anchors = [akLeft, akTop, akRight, akBottom]
      Images = ImageList1
      Indent = 19
      ReadOnly = True
      TabOrder = 0
    end
    object flpnlGroupCriteria: TFlowPanel
      Left = 3
      Top = 25
      Width = 205
      Height = 467
      BevelOuter = bvNone
      Caption = 'flpnlGroupCriteria'
      ParentColor = True
      ShowCaption = False
      TabOrder = 1
      object btnAddGroupCriteria: TButton
        Left = 0
        Top = 0
        Width = 200
        Height = 33
        Caption = 'Add Group Criteria (Ctrl+Ins)'
        TabOrder = 0
        OnClick = btnAddGroupCriteriaClick
      end
      object btnDelGroupCriteria: TButton
        Left = 0
        Top = 33
        Width = 200
        Height = 33
        Caption = 'Del Group Criteria (Ctrl+Del)'
        TabOrder = 1
        OnClick = btnDelGroupCriteriaClick
      end
      object BtnAddCriteria: TButton
        Left = 0
        Top = 66
        Width = 200
        Height = 33
        Caption = 'Add Criteria (Ins)'
        TabOrder = 2
        OnClick = BtnAddCriteriaClick
      end
      object btnDelCriteria: TButton
        Left = 0
        Top = 99
        Width = 200
        Height = 33
        Caption = 'Del Criteria (Del)'
        TabOrder = 3
        OnClick = btnDelCriteriaClick
      end
    end
  end
  inherited BalloonHint: TBalloonHint
    Left = 20
    Top = 391
  end
  inherited ImageList1: TImageList
    Left = 56
    Top = 392
  end
end
