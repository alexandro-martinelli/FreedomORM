inherited frmMethodParameter: TfrmMethodParameter
  BorderStyle = bsSingle
  Caption = 'Method parameter'
  ClientHeight = 160
  ClientWidth = 383
  PixelsPerInch = 120
  TextHeight = 17
  object lblParameterName: TLabel [0]
    Left = 40
    Top = 24
    Width = 101
    Height = 17
    Caption = 'Parameter name'
  end
  object lblParameterType: TLabel [1]
    Left = 46
    Top = 55
    Width = 95
    Height = 17
    Caption = 'Parameter type'
  end
  object lblParameterDirective: TLabel [2]
    Left = 88
    Top = 86
    Width = 53
    Height = 17
    Caption = 'Directive'
  end
  object cbxParameterName: TComboBox [3]
    Left = 151
    Top = 21
    Width = 200
    Height = 25
    TabOrder = 0
    TextHint = 'the parameter name'
    OnChange = cbxParameterNameChange
  end
  object cbxParameterType: TComboBox [4]
    Left = 151
    Top = 52
    Width = 200
    Height = 25
    TabOrder = 1
    TextHint = 'the parameter type'
  end
  object cbxParameterDirective: TComboBox [5]
    Left = 151
    Top = 83
    Width = 200
    Height = 25
    Style = csDropDownList
    TabOrder = 2
  end
  object btnOK: TButton [6]
    Left = 169
    Top = 119
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton [7]
    Left = 275
    Top = 119
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  inherited BalloonHint: TBalloonHint
    Left = 244
    Top = 47
  end
  inherited ImageList1: TImageList
    Left = 280
    Top = 48
  end
end
