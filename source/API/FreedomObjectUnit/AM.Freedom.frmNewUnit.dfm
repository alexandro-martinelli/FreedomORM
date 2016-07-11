inherited frmNewUnit: TfrmNewUnit
  Caption = 'New Freedom unit'
  ClientHeight = 590
  ClientWidth = 823
  Constraints.MinHeight = 620
  Constraints.MinWidth = 815
  ExplicitWidth = 841
  ExplicitHeight = 637
  PixelsPerInch = 120
  TextHeight = 17
  object splProperties: TSplitter [0]
    Left = 384
    Top = 0
    Width = 4
    Height = 544
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Color = clActiveCaption
    ParentColor = False
    ResizeStyle = rsUpdate
  end
  object tvClasses: TTreeView [1]
    Left = 388
    Top = 0
    Width = 435
    Height = 544
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    DragMode = dmAutomatic
    Images = ImageList1
    Indent = 19
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
    OnDragOver = tvClassesDragOver
  end
  object pnlClass: TPanel [2]
    Left = 0
    Top = 0
    Width = 384
    Height = 544
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    Caption = 'pnlClass'
    Constraints.MinWidth = 384
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      384
      544)
    object lblClassName: TLabel
      Left = 35
      Top = 34
      Width = 74
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Unit Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblPropertyFieldNames: TLabel
      Left = 10
      Top = 7
      Width = 80
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Unit name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edtUnitName: TComboBox
      Left = 116
      Top = 31
      Width = 255
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      TextHint = 'typ the name of class'
    end
    object btnAddClass: TButton
      Left = 212
      Top = 75
      Width = 159
      Height = 34
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akTop, akRight]
      Caption = 'Add Class >>'
      Default = True
      TabOrder = 1
      OnClick = btnAddClassClick
    end
    object btnExcClass: TButton
      Left = 212
      Top = 116
      Width = 159
      Height = 32
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akTop, akRight]
      Caption = 'Remove Class >>'
      TabOrder = 2
      OnClick = btnExcClassClick
    end
    object btnEditClass: TButton
      Left = 212
      Top = 156
      Width = 159
      Height = 32
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akTop, akRight]
      Caption = 'Edit Class >>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      Visible = False
      OnClick = btnEditClassClick
    end
  end
  object FlowPanel1: TFlowPanel [3]
    AlignWithMargins = True
    Left = 4
    Top = 544
    Width = 815
    Height = 46
    Margins.Left = 4
    Margins.Top = 0
    Margins.Right = 4
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    FlowStyle = fsRightLeftTopBottom
    ParentColor = True
    TabOrder = 2
    object btnCancel: TButton
      Left = 684
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
      TabOrder = 2
    end
    object btnFinish: TButton
      Left = 553
      Top = 0
      Width = 131
      Height = 43
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Finish'
      ModalResult = 1
      TabOrder = 1
    end
    object btnOptions: TButton
      Left = 422
      Top = 0
      Width = 131
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
  inherited ImageList1: TImageList
    Left = 80
    Top = 288
  end
end
