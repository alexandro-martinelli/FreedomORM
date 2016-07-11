inherited frmBaseArgumentParameter: TfrmBaseArgumentParameter
  BorderStyle = bsNone
  Caption = ''
  ClientHeight = 25
  ClientWidth = 314
  PixelsPerInch = 96
  TextHeight = 13
  object lblParameterName: TLabel [0]
    Left = 22
    Top = 5
    Width = 84
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    Caption = 'Parameter Name:'
  end
  inherited pnlName: TPanel
    Left = 37
    Top = 73
    Width = 243
    Align = alNone
    Caption = 'Argument Parameter Name: '
    TabOrder = 1
    TabStop = True
    Visible = False
  end
  object cbxParameterValue: TComboBox [2]
    Left = 111
    Top = 2
    Width = 190
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    TabOrder = 0
    TextHint = 'type the parameter value'
  end
  inherited BalloonHint: TBalloonHint
    Left = 308
    Top = 1
  end
  inherited ImageList1: TImageList
    Left = 344
    Top = 2
  end
end
