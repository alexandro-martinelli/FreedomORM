inherited frmNewArgument: TfrmNewArgument
  ActiveControl = cbxArgument
  Caption = ''
  ClientHeight = 281
  ClientWidth = 609
  OnShow = FormShow
  ExplicitWidth = 615
  ExplicitHeight = 316
  PixelsPerInch = 120
  TextHeight = 17
  object pnlArgument: TPanel [0]
    Left = 0
    Top = 33
    Width = 609
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object lblArgument: TLabel
      Left = 18
      Top = 7
      Width = 125
      Height = 17
      Alignment = taRightJustify
      Caption = 'Argument class type'
    end
    object lblCreate: TLabel
      Left = 330
      Top = 7
      Width = 58
      Height = 17
      Alignment = taRightJustify
      Caption = '.CreateAs'
      Visible = False
    end
    object cbxArgument: TComboBox
      Left = 145
      Top = 3
      Width = 171
      Height = 25
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbxArgumentChange
    end
    object cbxCreateAs: TComboBox
      Left = 396
      Top = 3
      Width = 152
      Height = 25
      Style = csDropDownList
      TabOrder = 1
      Visible = False
      OnChange = cbxArgumentChange
      Items.Strings = (
        'String'
        'Byte'
        'Smallint'
        'Integer'
        'Int64'
        'Single'
        'Double'
        'Extended'
        'Currency'
        'Date'
        'Time'
        'DateTime'
        'Boolean')
    end
  end
  inherited pnlName: TPanel
    Width = 609
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Argument Name: "%s"'
    TabOrder = 1
    ExplicitWidth = 609
    ExplicitHeight = 33
  end
  inherited BalloonHint: TBalloonHint
    Left = 12
    Top = 79
  end
  inherited ImageList1: TImageList
    Left = 48
    Top = 80
  end
end
