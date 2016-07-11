inherited frmConfigFreedomORM: TfrmConfigFreedomORM
  BorderStyle = bsSizeToolWin
  Caption = 'Configure Freedom ORM '
  ClientHeight = 423
  ClientWidth = 632
  Constraints.MinHeight = 470
  Constraints.MinWidth = 650
  PixelsPerInch = 120
  TextHeight = 17
  object pgcConfig: TPageControl [0]
    Left = 4
    Top = 5
    Width = 624
    Height = 371
    ActivePage = tsShortcuts
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsRename: TTabSheet
      Caption = 'Rename'
      DesignSize = (
        616
        339)
      object lblCheckedHint: TLabel
        Left = 3
        Top = 319
        Width = 224
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = '* The checked classes will be ignored'
      end
      object chkActiveRename: TCheckBox
        Left = 7
        Top = 2
        Width = 169
        Height = 17
        Hint = 'To apply the changes made, the IDE should be restarted'
        Caption = 'Active Rename'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object lvRenameClassConfig: TListView
        Left = 3
        Top = 64
        Width = 608
        Height = 250
        Anchors = [akLeft, akTop, akRight, akBottom]
        Checkboxes = True
        Columns = <
          item
            Caption = 'Class name'
            Width = 300
          end
          item
            Caption = 'Prefix'
            Width = 200
          end>
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 4
        ViewStyle = vsReport
      end
      object btnRemove: TButton
        Left = 195
        Top = 25
        Width = 135
        Height = 33
        Caption = 'Remove selected(s)'
        TabOrder = 2
        OnClick = btnRemoveClick
      end
      object btnAddClass: TButton
        Left = 3
        Top = 25
        Width = 81
        Height = 33
        Caption = 'Add'
        TabOrder = 1
        OnClick = btnAddClassClick
      end
      object btnLoadFromIniFile: TButton
        Left = 335
        Top = 25
        Width = 130
        Height = 33
        Caption = 'Load from Ini file'
        TabOrder = 3
        OnClick = btnLoadFromIniFileClick
      end
      object btnSaveToIniFile: TButton
        Left = 470
        Top = 25
        Width = 130
        Height = 33
        Caption = 'Save to Ini file'
        TabOrder = 5
        OnClick = btnSaveToIniFileClick
      end
      object btnEditClass: TButton
        Left = 89
        Top = 25
        Width = 101
        Height = 33
        Caption = 'Edit class(es)'
        TabOrder = 6
        OnClick = btnEditClassClick
      end
    end
    object tsShortcuts: TTabSheet
      Caption = 'Shortcuts'
      ImageIndex = 1
      object lblNewFreedomObject: TLabel
        Left = 32
        Top = 11
        Width = 131
        Height = 17
        Caption = 'Nem Freedom Object'
      end
      object lblNewFreedomDBObject: TLabel
        Left = 16
        Top = 40
        Width = 147
        Height = 17
        Caption = 'New Freedom DBObject'
      end
      object lblNewGroupCriteria: TLabel
        Left = 49
        Top = 69
        Width = 114
        Height = 17
        Caption = 'New Group Criteria'
      end
      object lblNewCriteria: TLabel
        Left = 91
        Top = 97
        Width = 72
        Height = 17
        Caption = 'New Criteria'
      end
      object lblReverse: TLabel
        Left = 53
        Top = 127
        Width = 110
        Height = 17
        Caption = 'Reverse Sentence'
      end
      object lblMoveBlockUp: TLabel
        Left = 44
        Top = 156
        Width = 119
        Height = 17
        Caption = 'Move Block/Line Up'
      end
      object lblMoveBlockDown: TLabel
        Left = 25
        Top = 185
        Width = 138
        Height = 17
        Caption = 'Move Block/Line Down'
      end
      object lblDeclareVariable: TLabel
        Left = 68
        Top = 214
        Width = 95
        Height = 17
        Caption = 'Declare Variable'
      end
      object lblExtractVariable: TLabel
        Left = 68
        Top = 243
        Width = 95
        Height = 17
        Caption = 'Extract variable'
      end
      object lblExtractConstant: TLabel
        Left = 60
        Top = 272
        Width = 103
        Height = 17
        Caption = 'Extract constant'
      end
      object lblFindMethod: TLabel
        Left = 366
        Top = 69
        Width = 75
        Height = 17
        Caption = 'Find Method'
      end
      object lblExtractMethod: TLabel
        Left = 344
        Top = 11
        Width = 97
        Height = 17
        Caption = 'Extract method'
      end
      object lblDeclareMethod: TLabel
        Left = 344
        Top = 40
        Width = 97
        Height = 17
        Caption = 'Declare method'
      end
      object lblViewRefactorings: TLabel
        Left = 338
        Top = 97
        Width = 103
        Height = 17
        Caption = 'View refactorings'
      end
      object lblDeclareUnit: TLabel
        Left = 369
        Top = 126
        Width = 72
        Height = 17
        Caption = 'Declare unit'
      end
      object lblNewUnit: TLabel
        Left = 388
        Top = 155
        Width = 53
        Height = 17
        Caption = 'New unit'
      end
      object lblUseUnit: TLabel
        Left = 392
        Top = 184
        Width = 49
        Height = 17
        Caption = 'Use unit'
      end
      object lblFreedomORMOptions: TLabel
        Left = 27
        Top = 301
        Width = 136
        Height = 17
        Caption = 'Freedom ORM options'
      end
      object edthkNewFreedomObject: THotKey
        Left = 173
        Top = 8
        Width = 150
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = [hcNone]
        Modifiers = []
        TabOrder = 0
      end
      object edthkNewFreedomDBObject: THotKey
        Left = 173
        Top = 37
        Width = 150
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = [hcNone]
        Modifiers = []
        TabOrder = 1
      end
      object edthkNewGroupCriteria: THotKey
        Left = 173
        Top = 66
        Width = 150
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = [hcNone]
        Modifiers = []
        TabOrder = 2
      end
      object edthkNewCriteria: THotKey
        Left = 173
        Top = 95
        Width = 150
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = [hcNone]
        Modifiers = []
        TabOrder = 3
      end
      object edthkInverteSentence: THotKey
        Left = 173
        Top = 124
        Width = 150
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = [hcNone]
        Modifiers = []
        TabOrder = 4
      end
      object edthkMoveBlockUp: THotKey
        Left = 173
        Top = 153
        Width = 151
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = []
        Modifiers = []
        TabOrder = 5
      end
      object edthkMoveBlockDown: THotKey
        Left = 173
        Top = 182
        Width = 151
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = []
        Modifiers = []
        TabOrder = 6
      end
      object edthkDeclareVariable: THotKey
        Left = 173
        Top = 211
        Width = 151
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = []
        Modifiers = []
        TabOrder = 7
      end
      object edthkExtractVariable: THotKey
        Left = 173
        Top = 240
        Width = 151
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = []
        Modifiers = []
        TabOrder = 8
      end
      object edthkExtractConstant: THotKey
        Left = 173
        Top = 269
        Width = 151
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = []
        Modifiers = []
        TabOrder = 9
      end
      object edthkFindMethod: THotKey
        Left = 450
        Top = 66
        Width = 151
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = []
        Modifiers = []
        TabOrder = 13
      end
      object edthkExtractMethod: THotKey
        Left = 450
        Top = 8
        Width = 151
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = []
        Modifiers = []
        TabOrder = 11
      end
      object edthkDeclareMethod: THotKey
        Left = 450
        Top = 37
        Width = 151
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = []
        Modifiers = []
        TabOrder = 12
      end
      object edthkViewRefactorings: THotKey
        Left = 450
        Top = 95
        Width = 151
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = []
        Modifiers = []
        TabOrder = 14
      end
      object edthkDeclareUnit: THotKey
        Left = 450
        Top = 124
        Width = 151
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = []
        Modifiers = []
        TabOrder = 15
      end
      object edthkNewUnit: THotKey
        Left = 450
        Top = 153
        Width = 151
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = []
        Modifiers = []
        TabOrder = 16
      end
      object edthkUseUnit: THotKey
        Left = 450
        Top = 182
        Width = 151
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = []
        Modifiers = []
        TabOrder = 17
      end
      object edthkFreedomORMOptions: THotKey
        Left = 173
        Top = 299
        Width = 151
        Height = 22
        Hint = 'Without the shortcut, this functionally is disabled'
        HotKey = 0
        InvalidKeys = []
        Modifiers = []
        TabOrder = 10
      end
    end
  end
  object btnOK: TButton [1]
    Left = 418
    Top = 382
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton [2]
    Left = 524
    Top = 382
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  inherited BalloonHint: TBalloonHint
    Left = 12
    Top = 384
  end
  inherited ImageList1: TImageList
    Left = 48
    Top = 385
  end
end
