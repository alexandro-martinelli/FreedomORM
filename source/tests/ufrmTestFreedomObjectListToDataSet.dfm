object frmTestFreedomObjectListToDataSet: TfrmTestFreedomObjectListToDataSet
  Left = 0
  Top = 0
  Caption = 'Show Data'
  ClientHeight = 519
  ClientWidth = 917
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 917
    Height = 519
    Align = alClient
    DataSource = dtsData
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -13
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object dtsData: TDataSource
    Left = 360
    Top = 168
  end
end
