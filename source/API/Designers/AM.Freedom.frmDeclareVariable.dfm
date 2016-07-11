inherited frmDeclareVariable: TfrmDeclareVariable
  BorderStyle = bsSingle
  Caption = 'Declare variable'
  ClientHeight = 185
  ClientWidth = 357
  PixelsPerInch = 120
  TextHeight = 17
  object lblVarName: TLabel [0]
    Left = 58
    Top = 19
    Width = 35
    Height = 17
    Caption = 'Name'
  end
  object lblVarType: TLabel [1]
    Left = 62
    Top = 50
    Width = 31
    Height = 17
    Caption = 'Type'
  end
  object lblInitialValue: TLabel [2]
    Left = 27
    Top = 81
    Width = 66
    Height = 17
    Caption = 'Initial Value'
  end
  object cbxVarName: TComboBox [3]
    Left = 102
    Top = 16
    Width = 200
    Height = 25
    TabOrder = 0
    TextHint = 'the variable name'
    OnChange = cbxVarNameChange
  end
  object cbxType: TComboBox [4]
    Left = 102
    Top = 47
    Width = 200
    Height = 25
    TabOrder = 1
    TextHint = 'the variable type'
    Items.Strings = (
      'Boolean'
      'ShortInt'
      'ShortInt'
      'SmallInt'
      'Integer'
      'Byte'
      'Word'
      'Cardinal'
      'Int64'
      'UInt64'
      'NativeInt'
      'NativeUInt'
      'Single'
      'Double'
      'Extended'
      'Currency'
      'Real'
      'ShortString'
      'OpenString'
      'File'
      'Text'
      'ByteBool'
      'WordBool'
      'LongBool'
      'Real48'
      'Extended80'
      'Pointer'
      'PWideChar'
      'PAnsiChar'
      'Variant'
      'OleVariant'
      'LongInt'
      'LongWord'
      'TextFile'
      'AnsiChar'
      'Char'
      'String'
      'AnsiString'
      'WideString'
      'PChar'
      'WideChar'
      'UnicodeString'
      'TObject'
      'TClass')
  end
  object btnOK: TButton [5]
    Left = 141
    Top = 144
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
    Top = 144
    Width = 100
    Height = 33
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object cbxInitialValue: TComboBox [7]
    Left = 102
    Top = 78
    Width = 200
    Height = 25
    TabOrder = 2
    TextHint = 'the variable initial value'
    Items.Strings = (
      'Boolean'
      'ShortInt'
      'ShortInt'
      'SmallInt'
      'Integer'
      'Byte'
      'Word'
      'Cardinal'
      'Int64'
      'UInt64'
      'NativeInt'
      'NativeUInt'
      'Single'
      'Double'
      'Extended'
      'Currency'
      'Real'
      'ShortString'
      'OpenString'
      'File'
      'Text'
      'ByteBool'
      'WordBool'
      'LongBool'
      'Real48'
      'Extended80'
      'Pointer'
      'PWideChar'
      'PAnsiChar'
      'Variant'
      'OleVariant'
      'LongInt'
      'LongWord'
      'TextFile'
      'AnsiChar'
      'Char'
      'String'
      'AnsiString'
      'WideString'
      'PChar'
      'WideChar'
      'UnicodeString'
      'TObject'
      'TClass')
  end
  object chkInitializeBeforeCurrentRow: TCheckBox [8]
    Left = 27
    Top = 114
    Width = 193
    Height = 17
    Caption = 'Initialize before current row'
    TabOrder = 3
  end
  inherited BalloonHint: TBalloonHint
    Left = 172
    Top = 39
  end
  inherited ImageList1: TImageList
    Left = 200
    Top = 40
  end
end
