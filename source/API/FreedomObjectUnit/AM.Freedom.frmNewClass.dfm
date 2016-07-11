inherited frmNewClass: TfrmNewClass
  BorderStyle = bsSizeToolWin
  Caption = 'New Object'
  ClientHeight = 573
  ClientWidth = 897
  Constraints.MinHeight = 620
  Constraints.MinWidth = 915
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 17
  object splProperties: TSplitter [0]
    Left = 475
    Top = 0
    Width = 4
    Height = 527
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Color = clActiveCaption
    ParentColor = False
    ResizeStyle = rsUpdate
  end
  object FlowPanel1: TFlowPanel [1]
    AlignWithMargins = True
    Left = 4
    Top = 527
    Width = 889
    Height = 46
    Margins.Left = 4
    Margins.Top = 0
    Margins.Right = 4
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    FlowStyle = fsRightLeftTopBottom
    TabOrder = 0
    object btnCancel: TButton
      Left = 758
      Top = 0
      Width = 131
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 4
    end
    object btnFinish: TButton
      Left = 627
      Top = 0
      Width = 131
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&Finish'
      Enabled = False
      TabOrder = 3
      OnClick = btnFinishClick
    end
    object btnNext: TButton
      Left = 495
      Top = 0
      Width = 132
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&Next >>'
      Default = True
      TabOrder = 2
      OnClick = btnNextClick
    end
    object btnBack: TButton
      Left = 366
      Top = 0
      Width = 129
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '<< &Back'
      Enabled = False
      TabOrder = 1
      OnClick = btnBackClick
    end
    object btnOptions: TButton
      Left = 235
      Top = 0
      Width = 131
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&Options'
      TabOrder = 0
      OnClick = btnOptionsClick
    end
  end
  object tvProperties: TTreeView [2]
    Left = 479
    Top = 0
    Width = 418
    Height = 527
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    DragMode = dmAutomatic
    Images = ImageList1
    Indent = 19
    MultiSelect = True
    MultiSelectStyle = [msControlSelect, msShiftSelect, msSiblingOnly]
    ReadOnly = True
    TabOrder = 1
    OnDragOver = tvPropertiesDragOver
  end
  object pnlClass: TPanel [3]
    Left = 0
    Top = 0
    Width = 475
    Height = 527
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    Caption = 'pnlClass'
    Constraints.MinWidth = 475
    ShowCaption = False
    TabOrder = 2
    object lblDescricao: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 465
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Please enter the specification of new TFreedomObject class'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object pgcSteps: TPageControl
      Left = 1
      Top = 31
      Width = 473
      Height = 495
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ActivePage = tsClassBaseClass
      Align = alClient
      Constraints.MinWidth = 471
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object tsClassBaseClass: TTabSheet
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 4
        Margins.Bottom = 0
        Caption = 'tsClassBaseClass'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        DesignSize = (
          465
          462)
        object lblClassName: TLabel
          Left = 35
          Top = 10
          Width = 82
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Class Name:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblBaseClass: TLabel
          Left = 43
          Top = 44
          Width = 73
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Base class:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object bvlClassListSeparator: TBevel
          Left = 7
          Top = 77
          Width = 449
          Height = 4
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Shape = bsTopLine
        end
        object lblClassList: TLabel
          Left = 10
          Top = 95
          Width = 108
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Class List Name:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblBaseClassList: TLabel
          Left = 22
          Top = 129
          Width = 94
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Base class list:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object edtClassName: TComboBox
          Left = 120
          Top = 7
          Width = 329
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          TextHint = 'typ the name of class'
        end
        object cbxBaseClasses: TComboBox
          Left = 120
          Top = 41
          Width = 329
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemIndex = 0
          ParentFont = False
          TabOrder = 1
          Text = 'TFreedomObject'
          TextHint = 'Typ the name of base class'
          Items.Strings = (
            'TFreedomObject')
        end
        object edtClassList: TComboBox
          Left = 120
          Top = 92
          Width = 329
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          TextHint = 'name of a class list'
        end
        object cbxBaseClassList: TComboBox
          Left = 120
          Top = 126
          Width = 329
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemIndex = 0
          ParentFont = False
          TabOrder = 3
          Text = 'TFreedomObjectList'
          TextHint = 'Typ the name of base generic class list'
          Items.Strings = (
            'TFreedomObjectList')
        end
      end
      object tsMapping: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'tsMapping'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ImageIndex = 3
        ParentFont = False
        object lblAlias: TLabel
          Left = 100
          Top = 122
          Width = 33
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Alias:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblMapping: TLabel
          Left = 89
          Top = 88
          Width = 44
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Name:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblInheritsCursorClassName: TLabel
          Left = 43
          Top = 234
          Width = 90
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Inherits from:'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblCursorClassName: TLabel
          Left = 53
          Top = 200
          Width = 80
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Class name:'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object chkMappedAt: TCheckBox
          Left = 18
          Top = 20
          Width = 127
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Mapped at'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          State = cbChecked
          TabOrder = 0
          OnClick = chkMappedAtClick
        end
        object edtEntityName: TComboBox
          Left = 141
          Top = 84
          Width = 237
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          TextHint = 'Entity Name'
        end
        object edtEntityAlias: TComboBox
          Left = 141
          Top = 118
          Width = 237
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          TextHint = 'Alias name'
        end
        object rdbEntity: TRadioButton
          Left = 36
          Top = 56
          Width = 113
          Height = 17
          Caption = 'Entity'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          TabStop = True
          OnClick = rdbEntityClick
        end
        object rdbCursor: TRadioButton
          Left = 36
          Top = 168
          Width = 113
          Height = 17
          Caption = 'Cursor'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 4
          TabStop = True
          OnClick = rdbEntityClick
        end
        object cbxCursorClassName: TComboBox
          Left = 141
          Top = 197
          Width = 207
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
          TextHint = 'Object cursor class name'
        end
        object cbxInheritsClassCursor: TComboBox
          Left = 141
          Top = 231
          Width = 207
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
          TextHint = 'inheritance class name'
          Items.Strings = (
            'TCustomObjectCursor'
            'TCustomDBObjectCursor')
        end
      end
      object tsPrimary: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'tsPrimary'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ImageIndex = 1
        ParentFont = False
        DesignSize = (
          465
          462)
        object lblPrimary: TLabel
          Left = 12
          Top = 7
          Width = 147
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Primary Constraint:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblPrimaryColumns: TLabel
          Left = 8
          Top = 38
          Width = 104
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Column Names:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label11: TLabel
          Left = 8
          Top = 73
          Width = 108
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Schema Names:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object edtPrimaryColumns: TComboBox
          Left = 122
          Top = 35
          Width = 310
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          TextHint = 'Typ the name of columns with coma values(;)'
        end
        object edtPrimarySchemas: TComboBox
          Left = 122
          Top = 69
          Width = 310
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          TextHint = 'Typ the name of Schemas(blank for all)'
        end
      end
      object tsUniques: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'tsUniques'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ImageIndex = 2
        ParentFont = False
        DesignSize = (
          465
          462)
        object lblUnique: TLabel
          Left = 4
          Top = 4
          Width = 147
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Unique Constraints:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblUnique1Columns: TLabel
          Left = 18
          Top = 37
          Width = 104
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Column Names:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label2: TLabel
          Left = 18
          Top = 122
          Width = 104
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Column Names:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label4: TLabel
          Left = 18
          Top = 207
          Width = 104
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Column Names:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label6: TLabel
          Left = 18
          Top = 290
          Width = 104
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Column Names:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label7: TLabel
          Left = 18
          Top = 72
          Width = 108
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Schema Names:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label8: TLabel
          Left = 18
          Top = 157
          Width = 108
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Schema Names:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label9: TLabel
          Left = 18
          Top = 242
          Width = 108
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Schema Names:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label10: TLabel
          Left = 18
          Top = 326
          Width = 108
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Schema Names:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Bevel1: TBevel
          Left = 12
          Top = 105
          Width = 446
          Height = 5
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Shape = bsTopLine
        end
        object Bevel2: TBevel
          Left = 12
          Top = 191
          Width = 446
          Height = 5
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Shape = bsTopLine
        end
        object Bevel3: TBevel
          Left = 12
          Top = 276
          Width = 446
          Height = 5
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Shape = bsTopLine
        end
        object lbl1: TLabel
          Left = 21
          Top = 374
          Width = 104
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Column Names:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lbl2: TLabel
          Left = 18
          Top = 409
          Width = 108
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Schema Names:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Bevel4: TBevel
          Left = 12
          Top = 360
          Width = 446
          Height = 5
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Shape = bsTopLine
        end
        object edtUnique1Column: TComboBox
          Left = 132
          Top = 33
          Width = 309
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          TextHint = 'Typ the name of columns with coma values(;)'
        end
        object edtUnique1Schemas: TComboBox
          Left = 132
          Top = 68
          Width = 309
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          TextHint = 'Typ the name of Schemas(blank for all)'
        end
        object edtUnique2Column: TComboBox
          Left = 132
          Top = 118
          Width = 309
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          TextHint = 'Typ the name of columns with coma values(;)'
        end
        object edtUnique2Schemas: TComboBox
          Left = 132
          Top = 153
          Width = 309
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          TextHint = 'Typ the name of Schemas(blank for all)'
        end
        object edtUnique3Column: TComboBox
          Left = 132
          Top = 203
          Width = 309
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          TextHint = 'Typ the name of columns with coma values(;)'
        end
        object edtUnique3Schemas: TComboBox
          Left = 132
          Top = 238
          Width = 309
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
          TextHint = 'Typ the name of Schemas(blank for all)'
        end
        object edtUnique4Column: TComboBox
          Left = 132
          Top = 286
          Width = 309
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
          TextHint = 'Typ the name of columns with coma values(;)'
        end
        object edtUnique4Schemas: TComboBox
          Left = 132
          Top = 322
          Width = 309
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 7
          TextHint = 'Typ the name of Schemas(blank for all)'
        end
        object edtUnique5Column: TComboBox
          Left = 132
          Top = 370
          Width = 309
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 8
          TextHint = 'Typ the name of columns with coma values(;)'
        end
        object edtUnique5Schemas: TComboBox
          Left = 132
          Top = 405
          Width = 309
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 9
          TextHint = 'Typ the name of Schemas(blank for all)'
        end
      end
      object tsForeigns: TTabSheet
        Caption = 'tsForeigns'
        ImageIndex = 6
        DesignSize = (
          465
          462)
        object Label1: TLabel
          Left = 5
          Top = 3
          Width = 152
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Foreign Constraints:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lvForeign: TListView
          Left = 3
          Top = 69
          Width = 459
          Height = 390
          Anchors = [akLeft, akTop, akRight, akBottom]
          Columns = <
            item
              Caption = 'Columns'
              Width = 100
            end
            item
              Caption = 'References To'
              Width = 120
            end
            item
              Caption = 'Reference Columns'
              Width = 150
            end
            item
              Caption = 'On Update'
              Width = 100
            end
            item
              Caption = 'On Delete'
              Width = 100
            end>
          MultiSelect = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
        end
        object btnAddForeign: TButton
          Left = 7
          Top = 28
          Width = 100
          Height = 35
          Caption = 'Add'
          TabOrder = 1
          OnClick = btnAddForeignClick
        end
        object btnRemoveForeign: TButton
          Left = 211
          Top = 28
          Width = 100
          Height = 35
          Caption = 'Remove'
          TabOrder = 2
          OnClick = btnRemoveForeignClick
        end
        object btnEditForeign: TButton
          Left = 109
          Top = 28
          Width = 100
          Height = 35
          Caption = 'Edit'
          TabOrder = 3
          OnClick = btnEditForeignClick
        end
      end
      object tsSchemas: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'tsSchemas'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ImageIndex = 4
        ParentFont = False
        DesignSize = (
          465
          462)
        object lblSchemas: TLabel
          Left = 17
          Top = 13
          Width = 118
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Schema Names:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edtClassSchema1: TComboBox
          Left = 27
          Top = 44
          Width = 278
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          TextHint = 'Typ the name of Schema'
        end
        object edtClassSchema2: TComboBox
          Left = 27
          Top = 80
          Width = 278
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          TextHint = 'Typ the name of Schema'
        end
        object edtClassSchema3: TComboBox
          Left = 27
          Top = 115
          Width = 278
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          TextHint = 'Typ the name of Schema'
        end
        object edtClassSchema4: TComboBox
          Left = 27
          Top = 150
          Width = 278
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          TextHint = 'Typ the name of Schema'
        end
        object rbDefSchema1: TRadioButton
          Left = 324
          Top = 46
          Width = 75
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Default'
          Checked = True
          TabOrder = 4
          TabStop = True
        end
        object rbDefSchema2: TRadioButton
          Left = 324
          Top = 82
          Width = 75
          Height = 23
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Default'
          TabOrder = 5
        end
        object rbDefSchema3: TRadioButton
          Left = 324
          Top = 118
          Width = 75
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Default'
          TabOrder = 6
        end
        object rbDefSchema4: TRadioButton
          Left = 324
          Top = 153
          Width = 75
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Default'
          TabOrder = 7
        end
        object edtClassSchema5: TComboBox
          Left = 27
          Top = 186
          Width = 278
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 8
          TextHint = 'Typ the name of Schema'
        end
        object rbDefSchema5: TRadioButton
          Left = 324
          Top = 188
          Width = 75
          Height = 23
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Default'
          TabOrder = 9
        end
      end
      object tsProperties: TTabSheet
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'tsProperties'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ImageIndex = 5
        ParentFont = False
        object lblPropertyFieldNames: TLabel
          Left = 4
          Top = 4
          Width = 177
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Property / Field names:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object btnAddProperty: TButton
          Left = 232
          Top = 52
          Width = 229
          Height = 33
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Add Property / Field'
          TabOrder = 0
          OnClick = btnAddPropertyClick
        end
        object btnExcProperty: TButton
          Left = 232
          Top = 88
          Width = 229
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Remove Property(ies) / Field(s)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = btnExcPropertyClick
        end
        object btnRemaneProperties: TButton
          Left = 232
          Top = 159
          Width = 229
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Rename Property(ies)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnClick = btnRemanePropertiesClick
        end
        object btnEditProperties: TButton
          Left = 232
          Top = 123
          Width = 229
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Edit Property(ies) / Field(s)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnClick = btnEditPropertiesClick
        end
      end
    end
  end
  object BindingsList1: TBindingsList [4]
    Methods = <>
    OutputConverters = <>
    Left = 542
    Top = 477
    object LinkControlToPropertyEnabled: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = chkMappedAt
      Track = True
      Component = edtEntityName
      ComponentProperty = 'Enabled'
    end
    object LinkControlToPropertyEnabled2: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = chkMappedAt
      Track = True
      Component = lblMapping
      ComponentProperty = 'Enabled'
      InitializeControlValue = False
    end
    object LinkControlToPropertyEnabled3: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = chkMappedAt
      Track = True
      Component = edtEntityAlias
      ComponentProperty = 'Enabled'
      InitializeControlValue = False
    end
    object LinkControlToPropertyEnabled4: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = chkMappedAt
      Track = True
      Component = lblAlias
      ComponentProperty = 'Enabled'
      InitializeControlValue = False
    end
  end
  inherited BalloonHint: TBalloonHint
    Left = 572
    Top = 479
  end
  inherited ImageList1: TImageList
    Left = 600
    Top = 480
  end
end
