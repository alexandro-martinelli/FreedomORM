inherited frmExtractConstant: TfrmExtractConstant
  ActiveControl = cbxConstantName
  BorderStyle = bsSingle
  Caption = 'Extract constant'
  ClientHeight = 182
  ClientWidth = 366
  PixelsPerInch = 120
  TextHeight = 17
  object lblConstantName: TLabel [0]
    Left = 24
    Top = 49
    Width = 95
    Height = 17
    Caption = 'Constant Name'
  end
  object lblConstantValue: TLabel [1]
    Left = 27
    Top = 18
    Width = 92
    Height = 17
    Caption = 'Constant Value'
  end
  object lblVisibilityScope: TLabel [2]
    Left = 32
    Top = 105
    Width = 85
    Height = 17
    Caption = 'Visibility scope'
    Enabled = False
  end
  object edtConstantValue: TEdit [3]
    Left = 125
    Top = 15
    Width = 220
    Height = 25
    Color = 12369084
    ReadOnly = True
    TabOrder = 0
  end
  object cbxConstantName: TComboBox [4]
    Left = 128
    Top = 46
    Width = 217
    Height = 25
    TabOrder = 1
    TextHint = 'the constant name'
  end
  object btnOK: TButton [5]
    Left = 141
    Top = 138
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = btnOKClick
  end
  object btnCancel: TButton [6]
    Left = 247
    Top = 138
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object chkDeclareOnClass: TCheckBox [7]
    Left = 27
    Top = 78
    Width = 206
    Height = 17
    Caption = 'Declare on class'
    TabOrder = 2
  end
  object cbxVisibilityScope: TComboBox [8]
    Left = 125
    Top = 102
    Width = 220
    Height = 25
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 3
    Text = 'strict private'
    Items.Strings = (
      'strict private'
      'private'
      'strict protected'
      'protected'
      'public'
      'published')
  end
  inherited BalloonHint: TBalloonHint
    Left = 276
    Top = 40
  end
  inherited ImageList1: TImageList
    Left = 312
    Top = 41
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 20
    Top = 141
    object LinkControlToPropertyEnabled: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = chkDeclareOnClass
      Track = True
      Component = lblVisibilityScope
      ComponentProperty = 'Enabled'
    end
    object LinkControlToPropertyEnabled2: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = chkDeclareOnClass
      Track = True
      Component = cbxVisibilityScope
      ComponentProperty = 'Enabled'
      InitializeControlValue = False
    end
  end
end
