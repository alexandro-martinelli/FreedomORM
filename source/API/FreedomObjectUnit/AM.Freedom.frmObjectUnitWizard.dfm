object frmObjectUnitWizard: TfrmObjectUnitWizard
  Left = 0
  Top = 0
  Caption = 'frmObjectUnitWizard'
  ClientHeight = 592
  ClientWidth = 969
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    969
    592)
  PixelsPerInch = 120
  TextHeight = 16
  object lblText: TLabel
    Left = 175
    Top = 6
    Width = 794
    Height = 29
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Welcome to Freedom Object Wizard'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object imgLeft: TImage
    Left = 0
    Top = 0
    Width = 201
    Height = 592
    Align = alLeft
    Stretch = True
    Transparent = True
    ExplicitTop = -71
    ExplicitHeight = 512
  end
  object PageControl1: TPageControl
    Left = 201
    Top = 41
    Width = 767
    Height = 549
    ActivePage = tsEditor
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsEditor: TTabSheet
      Caption = 'tsEditor'
      DesignSize = (
        759
        518)
      object btnOptions: TButton
        Left = 662
        Top = 482
        Width = 91
        Height = 33
        Anchors = [akRight, akBottom]
        Caption = 'Options'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object btnNewObjectUnit: TButton
        Left = 3
        Top = 2
        Width = 750
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        Caption = 'New Object unit'
        CommandLinkHint = 
          'Creates a new Object unit  with a manual especifications, you ca' +
          'n create a list<Object> too. Create a one or more classes in sam' +
          'e unit.'
        Default = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Style = bsCommandLink
        TabOrder = 1
      end
      object btnNewDBObjectUnit: TButton
        Left = 3
        Top = 89
        Width = 750
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        Caption = 'New DB Object unit'
        CommandLinkHint = 
          'Creates a new Object unit  with a database especifications, you ' +
          'can create a list<Object> too. Create a one or more classes in s' +
          'ame unit.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Style = bsCommandLink
        TabOrder = 2
      end
    end
  end
end
