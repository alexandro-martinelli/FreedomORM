inherited frmNewGroupCriteria: TfrmNewGroupCriteria
  BorderStyle = bsSingle
  Caption = 'New Group Criteria'
  ClientHeight = 215
  ClientWidth = 285
  ExplicitWidth = 291
  ExplicitHeight = 250
  PixelsPerInch = 120
  TextHeight = 17
  object lblPolicy: TLabel [0]
    Left = 53
    Top = 17
    Width = 35
    Height = 17
    Caption = 'Policy'
  end
  object lblLimitRows: TLabel [1]
    Left = 23
    Top = 48
    Width = 65
    Height = 17
    Caption = 'Limit Rows'
  end
  object lblVarName: TLabel [2]
    Left = 29
    Top = 81
    Width = 59
    Height = 17
    Caption = 'Var Name'
  end
  object lblExistingVarNames: TLabel [3]
    Left = 23
    Top = 137
    Width = 65
    Height = 17
    Caption = 'Var Names'
    Enabled = False
  end
  object cbxPolicy: TComboBox [4]
    Left = 98
    Top = 13
    Width = 160
    Height = 25
    AutoDropDown = True
    Style = csDropDownList
    TabOrder = 0
  end
  object spedtLimitRows: TSpinEdit [5]
    Left = 98
    Top = 44
    Width = 160
    Height = 27
    MaxValue = 999999999
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object FlowPanel1: TFlowPanel [6]
    AlignWithMargins = True
    Left = 4
    Top = 168
    Width = 277
    Height = 43
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'FlowPanel1'
    FlowStyle = fsRightLeftTopBottom
    ShowCaption = False
    TabOrder = 5
    object btnCancel: TButton
      Left = 146
      Top = 0
      Width = 131
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      Left = 16
      Top = 0
      Width = 130
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOKClick
    end
  end
  object cbxVarNames: TComboBox [7]
    Left = 98
    Top = 78
    Width = 160
    Height = 25
    AutoDropDown = True
    TabOrder = 2
    TextHint = 'New Group var Name'
  end
  object cbxGroupVarNames: TComboBox [8]
    Left = 98
    Top = 134
    Width = 160
    Height = 25
    AutoDropDown = True
    Style = csDropDownList
    Enabled = False
    TabOrder = 4
  end
  object chxIntoExisting: TCheckBox [9]
    Left = 23
    Top = 109
    Width = 235
    Height = 17
    Hint = 'VarName := ExistingVar.AddGroupCriteria(poOr, 10);'
    Caption = 'Into Existing Group Criteria'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = chxIntoExistingClick
  end
  inherited BalloonHint: TBalloonHint
    Left = 155
    Top = 41
  end
  inherited ImageList1: TImageList
    Left = 191
    Top = 42
  end
end
