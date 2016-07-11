inherited frmNewProperty: TfrmNewProperty
  BorderStyle = bsSingle
  Caption = 'New Property'
  ClientHeight = 469
  ClientWidth = 672
  Constraints.MinWidth = 630
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 17
  object lblDescription: TLabel [0]
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 664
    Height = 23
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'Specify Property Options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object pgcPropertySteps: TPageControl [1]
    Left = 0
    Top = 31
    Width = 672
    Height = 393
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ActivePage = tsEnumOptions
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object tsName: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'tsName'
      object lblPropertyName: TLabel
        Left = 25
        Top = 31
        Width = 48
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
        Font.Style = [fsBold]
        ParentFont = False
      end
      object edtPropertyName: TComboBox
        Left = 80
        Top = 27
        Width = 236
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
        TabOrder = 0
        TextHint = 'The property name'
        OnExit = edtPropertyNameExit
      end
    end
    object tsType: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'tsType'
      ImageIndex = 1
      object lblPropertyType: TLabel
        Left = 21
        Top = 25
        Width = 41
        Height = 18
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Type:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object cbxType: TComboBox
        Left = 70
        Top = 22
        Width = 250
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    object tsMappedField: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'tsMappedField'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 7
      ParentFont = False
      object chkMappedAt: TCheckBox
        Left = 18
        Top = 14
        Width = 127
        Height = 23
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Mapped at'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = chkMappedAtClick
      end
      object pnlMappedAt: TPanel
        Left = 38
        Top = 51
        Width = 489
        Height = 294
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        BevelOuter = bvNone
        Caption = 'pnlMappedAt'
        ParentColor = True
        ShowCaption = False
        TabOrder = 1
        object lblMapping: TLabel
          Left = 55
          Top = 8
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
        object lblSize: TLabel
          Left = 65
          Top = 43
          Width = 30
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Size:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblScale: TLabel
          Left = 276
          Top = 43
          Width = 38
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Scale:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblAlias: TLabel
          Left = 61
          Top = 80
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
        object lblDomain: TLabel
          Left = 44
          Top = 116
          Width = 54
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Domain:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblDefaultValue: TLabel
          Left = 7
          Top = 152
          Width = 91
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Default Value:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object edtMapping: TComboBox
          Left = 102
          Top = 4
          Width = 375
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
          TabOrder = 0
          TextHint = 'Column Name'
        end
        object spedtSize: TSpinEdit
          Left = 102
          Top = 39
          Width = 156
          Height = 28
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxValue = 0
          MinValue = 0
          ParentFont = False
          TabOrder = 1
          Value = 0
        end
        object spedtScale: TSpinEdit
          Left = 322
          Top = 39
          Width = 155
          Height = 28
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxValue = 0
          MinValue = 0
          ParentFont = False
          TabOrder = 2
          Value = 0
        end
        object edtAlias: TComboBox
          Left = 102
          Top = 76
          Width = 375
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          TabOrder = 3
          TextHint = 'Alias name'
        end
        object cbxDomain: TComboBox
          Left = 102
          Top = 111
          Width = 375
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          TabOrder = 4
          TextHint = 'Domain name'
        end
        object cbxDefaultValue: TComboBox
          Left = 102
          Top = 146
          Width = 375
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          TabOrder = 5
          TextHint = 'Ex: 1, '#39'Freedom'#39', Now, Date, Time'
        end
        object gbxOrder: TGroupBox
          Left = 17
          Top = 177
          Width = 460
          Height = 107
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = ' Order '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
          object lblOrderIndex: TLabel
            Left = 37
            Top = 30
            Width = 43
            Height = 18
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Index:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object lblOrderType: TLabel
            Left = 260
            Top = 30
            Width = 39
            Height = 18
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Type:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object lblOrderDirective: TLabel
            Left = 17
            Top = 65
            Width = 60
            Height = 18
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Directive:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object spedtOrderIndex: TSpinEdit
            Left = 85
            Top = 25
            Width = 158
            Height = 28
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            MaxValue = 0
            MinValue = 0
            TabOrder = 0
            Value = 0
          end
          object cbxOrderType: TComboBox
            Left = 305
            Top = 26
            Width = 134
            Height = 26
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Style = csDropDownList
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
          end
          object cbxOrderDirective: TComboBox
            Left = 85
            Top = 61
            Width = 354
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
            TextHint = 'FB Ex: NULLS LAST, NULLS FIRST'
          end
        end
      end
    end
    object tsEnumOptions: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'tsEnumOptions'
      ImageIndex = 3
      OnShow = tsEnumOptionsShow
      object lblEnumName: TLabel
        Left = 50
        Top = 22
        Width = 86
        Height = 18
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Enum Name:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblEnumType: TLabel
        Left = 54
        Top = 58
        Width = 81
        Height = 18
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Enum Type:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblEnumCharValues: TLabel
        Left = 10
        Top = 93
        Width = 124
        Height = 18
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Enum Char Values:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object cbxEnumName: TComboBox
        Left = 137
        Top = 18
        Width = 237
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 0
        TextHint = 'Enumeration Name'
      end
      object cbxEnumType: TComboBox
        Left = 137
        Top = 54
        Width = 85
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemIndex = 0
        ParentFont = False
        TabOrder = 1
        Text = 'Ordinal'
        OnChange = cbxEnumTypeChange
        Items.Strings = (
          'Ordinal'
          'Char')
      end
      object edtEnumCharValues: TComboBox
        Left = 137
        Top = 89
        Width = 237
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 2
        TextHint = 'Ex: A;B:C;D or A,B,C,D'
        Visible = False
      end
    end
    object tsBoolOptions: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'tsBoolOptions'
      ImageIndex = 6
      OnShow = tsBoolOptionsShow
      object lblBoolType: TLabel
        Left = 59
        Top = 21
        Width = 95
        Height = 18
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Boolean Type:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblBoolTrue: TLabel
        Left = 24
        Top = 56
        Width = 132
        Height = 18
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Boolean True Value:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object lblBoolFalse: TLabel
        Left = 20
        Top = 92
        Width = 134
        Height = 18
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Boolean False Value:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object cbxBoolType: TComboBox
        Left = 157
        Top = 17
        Width = 106
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemIndex = 0
        ParentFont = False
        TabOrder = 0
        Text = 'Boolean'
        OnChange = cbxBoolTypeChange
        Items.Strings = (
          'Boolean'
          'Char/Byte')
      end
      object edtBoolTrue: TComboBox
        Left = 157
        Top = 52
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
        TabOrder = 1
        TextHint = 'True value'
        Visible = False
      end
      object edtBoolFalse: TComboBox
        Left = 157
        Top = 88
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
        TextHint = 'False value'
        Visible = False
      end
    end
    object tsDetailOptions: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'tsDetailOptions'
      ImageIndex = 8
      DesignSize = (
        664
        360)
      object lblDetailColumnRefName: TLabel
        Left = 14
        Top = 16
        Width = 168
        Height = 18
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Reference Column Name:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblClassName: TLabel
        Left = 97
        Top = 51
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
      object lblClassList: TLabel
        Left = 72
        Top = 85
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
      object edtDetailColumRefName: TComboBox
        Left = 183
        Top = 12
        Width = 256
        Height = 26
        Hint = 'The name of column on this class make reference'
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
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        TextHint = 'The reference column name'
      end
      object cbxDetailClassName: TComboBox
        Left = 183
        Top = 47
        Width = 247
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        TextHint = 'name of class'
      end
      object chkDetailLazyLoad: TCheckBox
        Left = 184
        Top = 115
        Width = 139
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Enabled Lazy load'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 2
        Visible = False
      end
      object edtDetailListClassName: TComboBox
        Left = 183
        Top = 81
        Width = 247
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        TextHint = 'name of class list'
      end
    end
    object tsJoinOptions: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'tsJoinOptions'
      ImageIndex = 9
      DesignSize = (
        664
        360)
      object lblUpdateAction: TLabel
        Left = 88
        Top = 115
        Width = 96
        Height = 18
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Update Action:'
      end
      object lblDeleteAction: TLabel
        Left = 344
        Top = 115
        Width = 90
        Height = 18
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Delete Action:'
      end
      object lblJoinRefColumnName: TLabel
        Left = 13
        Top = 12
        Width = 168
        Height = 18
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Reference Column Name:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label1: TLabel
        Left = 105
        Top = 47
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
      object Label2: TLabel
        Left = 122
        Top = 80
        Width = 61
        Height = 18
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Join Kind:'
      end
      object edtJoinRefColumnName: TComboBox
        Left = 190
        Top = 9
        Width = 255
        Height = 26
        Hint = 'The name of column on this class make reference'
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
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        TextHint = 'The reference column name'
      end
      object cbxJoinClassName: TComboBox
        Left = 190
        Top = 43
        Width = 246
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        TextHint = 'The name of class'
      end
      object cbxJoinKind: TComboBox
        Left = 190
        Top = 77
        Width = 143
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = csDropDownList
        TabOrder = 2
      end
      object chkJoinLazyLoad: TCheckBox
        Left = 441
        Top = 81
        Width = 140
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Enabled Lazy load'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 3
        Visible = False
      end
      object cbxUpdateAction: TComboBox
        Left = 190
        Top = 111
        Width = 143
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = csDropDownList
        TabOrder = 4
      end
      object cbxDeleteAction: TComboBox
        Left = 441
        Top = 111
        Width = 145
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = csDropDownList
        TabOrder = 5
      end
    end
    object tsColumnOptions: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'tsColumnOptions'
      ImageIndex = 2
      object chkRequired: TCheckBox
        Left = 25
        Top = 20
        Width = 94
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Required'
        TabOrder = 0
      end
      object chkNoUpdate: TCheckBox
        Left = 150
        Top = 20
        Width = 95
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'No Update'
        TabOrder = 1
      end
      object chkNoInsert: TCheckBox
        Left = 25
        Top = 50
        Width = 94
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'No Insert'
        TabOrder = 2
      end
      object chkNoDelete: TCheckBox
        Left = 150
        Top = 50
        Width = 95
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'No Delete'
        TabOrder = 3
      end
      object chxIsNullable: TCheckBox
        Left = 25
        Top = 80
        Width = 94
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Is Nullable'
        TabOrder = 4
        OnClick = chxIsNullableClick
      end
      object chxHideNullable: TCheckBox
        Left = 43
        Top = 104
        Width = 110
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Hide Nullable'
        TabOrder = 5
      end
      object chxUseNullableTypes: TCheckBox
        Left = 43
        Top = 128
        Width = 150
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Use nullable types'
        TabOrder = 6
      end
    end
    object tsIdOptions: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'tsIdOptions'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 8
      ParentFont = False
      object chkIsId: TCheckBox
        Left = 13
        Top = 29
        Width = 100
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Is ID?'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = chkIsIdClick
      end
      object pnlIDOptions: TPanel
        Left = 14
        Top = 59
        Width = 703
        Height = 174
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        BevelOuter = bvNone
        Enabled = False
        ParentColor = True
        TabOrder = 1
        object lblIDType: TLabel
          Left = 61
          Top = 9
          Width = 58
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Id Type:'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblSequenceName: TLabel
          Left = 9
          Top = 44
          Width = 112
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Sequence Name:'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Visible = False
        end
        object lblOnSchemas: TLabel
          Left = 33
          Top = 80
          Width = 88
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'On Schemas:'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object cbxIDType: TComboBox
          Left = 123
          Top = 5
          Width = 107
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Style = csDropDownList
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemIndex = 0
          ParentFont = False
          TabOrder = 0
          Text = 'Identity'
          OnChange = cbxIDTypeChange
          Items.Strings = (
            'Identity'
            'Sequence'
            'None')
        end
        object edtSequenceName: TComboBox
          Left = 123
          Top = 41
          Width = 237
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
          TabOrder = 1
          TextHint = 'Sequence Name'
          Visible = False
        end
        object edtOnSchemas: TComboBox
          Left = 123
          Top = 76
          Width = 152
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
          TabOrder = 2
          TextHint = 'Ex: public'
        end
        object edtOnSchema2: TComboBox
          Left = 123
          Top = 111
          Width = 152
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
          TabOrder = 3
          TextHint = 'Ex: metrics'
        end
      end
    end
    object tsSchemas: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'tsSchemas'
      ImageIndex = 6
      object lblSchemas: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 656
        Height = 18
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        Caption = 'On Schemas:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object edtSchema1: TComboBox
        Left = 35
        Top = 38
        Width = 159
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 0
        TextHint = 'Ex: public'
      end
      object edtSchema2: TComboBox
        Left = 35
        Top = 73
        Width = 159
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 1
        TextHint = 'Ex: metrics'
      end
    end
    object tsExtension: TTabSheet
      Caption = 'tsExtension'
      ImageIndex = 10
      DesignSize = (
        664
        360)
      object lblExtensionClassName: TLabel
        Left = 11
        Top = 8
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
      object cbxExtensionClassName: TComboBox
        Left = 96
        Top = 4
        Width = 246
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        TextHint = 'The name of extension class'
        OnEnter = cbxExtensionClassNameEnter
        OnExit = cbxExtensionClassNameExit
      end
    end
  end
  object FlowPanel1: TFlowPanel [2]
    AlignWithMargins = True
    Left = 4
    Top = 424
    Width = 664
    Height = 45
    Margins.Left = 4
    Margins.Top = 0
    Margins.Right = 4
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    FlowStyle = fsRightLeftTopBottom
    TabOrder = 1
    object btnCancel: TButton
      Left = 534
      Top = 0
      Width = 130
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
      Left = 403
      Top = 0
      Width = 131
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&Finish'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ModalResult = 1
      ParentFont = False
      TabOrder = 3
    end
    object btnNext: TButton
      Left = 272
      Top = 0
      Width = 131
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Next >>'
      Default = True
      TabOrder = 2
      OnClick = btnNextClick
    end
    object btnBack: TButton
      Left = 141
      Top = 0
      Width = 131
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '<< &Back'
      TabOrder = 1
      OnClick = btnBackClick
    end
    object btnOptions: TButton
      Left = 11
      Top = 0
      Width = 130
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Options'
      TabOrder = 0
      OnClick = btnOptionsClick
    end
  end
  object BindingsList1: TBindingsList [3]
    Methods = <>
    OutputConverters = <>
    Left = 405
    Top = 201
    object LinkControlToPropertyEnabled: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = chkIsId
      Track = True
      Component = pnlIDOptions
      ComponentProperty = 'Enabled'
    end
    object LinkControlToPropertyEnabled10: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = chkIsId
      Track = True
      Component = edtSequenceName
      ComponentProperty = 'Enabled'
      InitializeControlValue = False
    end
    object LinkControlToPropertyEnabled11: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = chkIsId
      Track = True
      Component = edtOnSchemas
      ComponentProperty = 'Enabled'
      InitializeControlValue = False
    end
    object LinkControlToPropertyEnabled12: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = chkIsId
      Track = True
      Component = edtOnSchema2
      ComponentProperty = 'Enabled'
      InitializeControlValue = False
    end
    object LinkControlToPropertyEnabled13: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = chkIsId
      Track = True
      Component = lblIDType
      ComponentProperty = 'Enabled'
      InitializeControlValue = False
    end
    object LinkControlToPropertyEnabled14: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = chkIsId
      Track = True
      Component = lblSequenceName
      ComponentProperty = 'Enabled'
      InitializeControlValue = False
    end
    object LinkControlToPropertyEnabled15: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = chkIsId
      Track = True
      Component = lblOnSchemas
      ComponentProperty = 'Enabled'
      InitializeControlValue = False
    end
    object LinkControlToPropertyEnabled16: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = chkIsId
      Track = True
      Component = cbxIDType
      ComponentProperty = 'Enabled'
      InitializeControlValue = False
    end
  end
  inherited BalloonHint: TBalloonHint
    Left = 436
    Top = 201
  end
  inherited ImageList1: TImageList
    Left = 472
    Top = 201
  end
end
