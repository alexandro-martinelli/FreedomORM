inherited frmNewCriteria: TfrmNewCriteria
  ActiveControl = cbxCreateAs
  BorderStyle = bsSizeToolWin
  Caption = 'Create new Criteria'
  ClientHeight = 490
  ClientWidth = 781
  Constraints.MinHeight = 530
  Constraints.MinWidth = 799
  OnShow = FormShow
  ExplicitWidth = 799
  ExplicitHeight = 537
  PixelsPerInch = 120
  TextHeight = 17
  object flpnlButtons: TFlowPanel [0]
    AlignWithMargins = True
    Left = 3
    Top = 445
    Width = 775
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'flpnlButtons'
    FlowStyle = fsBottomTopRightLeft
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    object btnCancel: TButton
      Left = 645
      Top = -1
      Width = 130
      Height = 43
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object btnFinish: TButton
      Left = 515
      Top = -1
      Width = 130
      Height = 43
      Caption = '&Finish'
      TabOrder = 2
      OnClick = btnFinishClick
    end
    object btnNext: TButton
      Left = 384
      Top = -1
      Width = 131
      Height = 43
      Caption = '&Next >>'
      Default = True
      TabOrder = 1
      OnClick = btnNextClick
    end
    object btnBack: TButton
      Left = 255
      Top = -1
      Width = 129
      Height = 43
      Caption = '<< &Back'
      TabOrder = 0
      OnClick = btnBackClick
    end
  end
  object pgcSteps: TPageControl [1]
    Left = 0
    Top = 0
    Width = 781
    Height = 442
    ActivePage = tsCreateType
    Align = alClient
    TabOrder = 1
    object tsCreateType: TTabSheet
      Caption = 'tsCreateType'
      DesignSize = (
        773
        410)
      object lblCreateAs: TLabel
        Left = 24
        Top = 14
        Width = 108
        Height = 17
        Alignment = taRightJustify
        Caption = 'TCriteria.CreateAs'
      end
      object lblCreateExplain: TLabel
        Left = 12
        Top = 44
        Width = 745
        Height = 44
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'This create requires two arguments(pLeftArgument(TCustomArgument' +
          ') and pRigthArgument(TCustomArgument))'
        WordWrap = True
      end
      object bvlSeparator: TBevel
        Left = 8
        Top = 98
        Width = 757
        Height = 5
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object cbxCreateAs: TComboBox
        Left = 137
        Top = 10
        Width = 199
        Height = 25
        Style = csDropDownList
        TabOrder = 0
        OnChange = cbxCreateAsChange
        Items.Strings = (
          'Equal'
          'Different'
          'Greater Than'
          'Less Than'
          'Greater Than Or Equal To'
          'Less Than Or Equal To'
          'Containing'
          'Null'
          'Not Null'
          'Starting With'
          'Like Left'
          'Like Midle'
          'Like Rigth'
          'Between')
      end
      object mmoCreateParameters: TMemo
        Left = 12
        Top = 107
        Width = 745
        Height = 296
        TabStop = False
        Anchors = [akLeft, akTop, akRight, akBottom]
        BorderStyle = bsNone
        Lines.Strings = (
          'The TCustomArgument is a descendent class for:'
          '1 - Simple Arguments'
          
            'a) TTableArgument: Generate aliased name of table database(PRODU' +
            'CTS PROD)'
          
            'b) TFieldArgument: Generate aliased field name of table database' +
            '(PROD.NAME AS NAME)'
          'c) TLiteralArgument: Generate aliased literal values(My value)'
          'd) TNullArgument: Generate null literal value(Null)'
          
            'e) TValueArgument: Generate value for comparison(1, "A", "Box", ' +
            '"16/11/1985")'
          '2 - Complex(Expressions) Arguments'
          
            'a) TSum: Generate a aliased sum expression(SUM(PROD.SELL_VALUE) ' +
            'AS SUM_SELL_VALUE)'
          
            'b) TMin: Generate a aliased min expression(MIN(PROD.SELL_VALUE) ' +
            'AS MIN_SELL_VALUE)'
          
            'c) TMax: Generate a aliased max expression(MAX(PROD.SELL_VALUE) ' +
            'AS MAX_SELL_VALUE)'
          
            'd) TAvg: Generate a aliased avg expression(AVG(PROD.SELL_VALUE) ' +
            'AS AVG_SELL_VALUE)'
          
            'e) TCount: Generate a aliased count expression(COUNT(PROD.ID) AS' +
            ' RECORD_COUNT)'
          
            'f) TUpper: Generate a aliased upper expression(UPPER(PROD.NAME) ' +
            'AS NAME)'
          
            'g) TLower: Generate a aliased lower expression(LOWER(PROD.NAME) ' +
            'AS NAME)'
          
            'h) TCoalesce: Generate a aliased coalesce expression(COALESCE(PR' +
            'OD.SELL_VALUE, Value) AS SELL_VALUE)'
          'And more expression for especific databases.')
        ReadOnly = True
        TabOrder = 1
      end
    end
  end
  inherited BalloonHint: TBalloonHint
    Left = 468
    Top = 351
  end
  inherited ImageList1: TImageList
    Left = 504
    Top = 352
  end
end
