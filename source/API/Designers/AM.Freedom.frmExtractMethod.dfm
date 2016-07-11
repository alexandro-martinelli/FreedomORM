inherited frmExtractMethod: TfrmExtractMethod
  ActiveControl = cbxMethodName
  BorderStyle = bsSizeToolWin
  Caption = 'Extract Method'
  ClientHeight = 533
  ClientWidth = 832
  Constraints.MinHeight = 580
  Constraints.MinWidth = 850
  PixelsPerInch = 120
  TextHeight = 17
  object lblClassName: TLabel [0]
    Left = 34
    Top = 15
    Width = 69
    Height = 17
    Caption = 'Class Name'
  end
  object lblMethodName: TLabel [1]
    Left = 19
    Top = 46
    Width = 84
    Height = 17
    Caption = 'Method name'
  end
  object lblMethodResult: TLabel [2]
    Left = 20
    Top = 105
    Width = 83
    Height = 17
    Caption = 'Method result'
  end
  object lblDeclareOn: TLabel [3]
    Left = 38
    Top = 137
    Width = 65
    Height = 17
    Caption = 'Declare on'
  end
  object edtClassName: TEdit [4]
    Left = 112
    Top = 12
    Width = 706
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Color = 12369084
    ReadOnly = True
    TabOrder = 0
  end
  object chkVirtual: TCheckBox [5]
    Left = 112
    Top = 78
    Width = 97
    Height = 17
    Caption = 'Virtual'
    TabOrder = 2
    OnClick = chkVirtualClick
  end
  object chkFinal: TCheckBox [6]
    Left = 355
    Top = 78
    Width = 97
    Height = 17
    Caption = 'Final'
    TabOrder = 4
  end
  object chkDynamic: TCheckBox [7]
    Left = 232
    Top = 78
    Width = 97
    Height = 17
    Caption = 'Dynamic'
    TabOrder = 3
    OnClick = chkVirtualClick
  end
  object chkOverload: TCheckBox [8]
    Left = 475
    Top = 78
    Width = 97
    Height = 17
    Caption = 'Overload'
    TabOrder = 5
  end
  object cbxMethodName: TComboBox [9]
    Left = 112
    Top = 43
    Width = 706
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    TextHint = 'the name of method'
  end
  object cbxType: TComboBox [10]
    Left = 112
    Top = 102
    Width = 265
    Height = 25
    TabOrder = 6
    TextHint = 'the result of method'
  end
  object btnOK: TButton [11]
    Left = 618
    Top = 492
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 12
    OnClick = btnOKClick
  end
  object btnCancel: TButton [12]
    Left = 724
    Top = 492
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 13
  end
  object cbxVisibilityScope: TComboBox [13]
    Left = 231
    Top = 133
    Width = 153
    Height = 25
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 9
    Text = 'strict private'
    Items.Strings = (
      'strict private'
      'private'
      'strict protected'
      'protected'
      'public'
      'published')
  end
  object rdbVisibilityScope: TRadioButton [14]
    Left = 112
    Top = 137
    Width = 113
    Height = 17
    BiDiMode = bdRightToLeft
    Caption = 'Visibility Scope'
    Checked = True
    ParentBiDiMode = False
    TabOrder = 8
    TabStop = True
    OnClick = rdbVisibilityScopeClick
  end
  object rdbAfterMethodDeclaration: TRadioButton [15]
    Left = 393
    Top = 137
    Width = 177
    Height = 17
    BiDiMode = bdRightToLeft
    Caption = 'After method declaration'
    ParentBiDiMode = False
    TabOrder = 10
    TabStop = True
    OnClick = rdbAfterMethodDeclarationClick
  end
  object cbxMethodList: TComboBox [16]
    Left = 575
    Top = 133
    Width = 243
    Height = 25
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 13
    Enabled = False
    TabOrder = 11
  end
  object chkIsClassMethod: TCheckBox [17]
    Left = 394
    Top = 106
    Width = 145
    Height = 17
    Caption = 'Is class method'
    TabOrder = 7
  end
  object pgcMethodComplementation: TPageControl [18]
    Left = 112
    Top = 168
    Width = 706
    Height = 316
    ActivePage = tsMethodParameters
    Anchors = [akLeft, akTop, akRight, akBottom]
    Style = tsFlatButtons
    TabOrder = 14
    object tsMethodImplementation: TTabSheet
      Caption = 'Method implementation'
      object mmoMethodImplemetation: TMemo
        Left = 0
        Top = 0
        Width = 698
        Height = 281
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object tsMethodParameters: TTabSheet
      Caption = 'Method parameters'
      ImageIndex = 1
      object lvParameters: TListView
        Left = 0
        Top = 43
        Width = 698
        Height = 238
        Align = alClient
        Columns = <
          item
            Caption = 'Parameter name'
            Width = 200
          end
          item
            Caption = 'Parameter type'
            Width = 200
          end
          item
            Caption = 'Parameter directive'
            Width = 200
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object pnlButtons: TPanel
        Left = 0
        Top = 0
        Width = 698
        Height = 43
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object btnAddParameter: TButton
          Left = 4
          Top = 4
          Width = 115
          Height = 33
          Caption = 'Add Parameter'
          TabOrder = 0
          OnClick = btnAddParameterClick
        end
        object btnAlterParameter: TButton
          Left = 127
          Top = 4
          Width = 115
          Height = 33
          Caption = 'Alter Parameter'
          TabOrder = 1
          OnClick = btnAlterParameterClick
        end
        object btnDelParameter: TButton
          Left = 250
          Top = 4
          Width = 115
          Height = 33
          Caption = 'Del Parameter'
          TabOrder = 2
          OnClick = btnDelParameterClick
        end
      end
    end
  end
  inherited BalloonHint: TBalloonHint
    Left = 12
    Top = 279
  end
  inherited ImageList1: TImageList
    Left = 48
    Top = 280
  end
end
