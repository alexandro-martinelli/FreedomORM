object dmConnection: TdmConnection
  OldCreateOrder = False
  Height = 407
  Width = 579
  object FDGUIxWaitCursor: TFDGUIxWaitCursor
    Provider = 'Forms'
    ScreenCursor = gcrNone
    Left = 72
    Top = 16
  end
  object FDConnection: TFDConnection
    Params.Strings = (
      'OSAuthent=Yes'
      'Server=.'
      'DriverID=MSSQL')
    LoginPrompt = False
    Left = 68
    Top = 80
  end
  object QryTableSchemas: TFDQuery
    Connection = FDConnection
    Left = 192
    Top = 24
  end
  object FDPhysFBDriverLink: TFDPhysFBDriverLink
    Left = 64
    Top = 192
  end
  object FDPhysMSSQLDriverLink: TFDPhysMSSQLDriverLink
    Left = 64
    Top = 136
  end
  object FDPhysPgDriverLink: TFDPhysPgDriverLink
    Left = 64
    Top = 248
  end
end
