object frmCreateAcc: TfrmCreateAcc
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Account creation'
  ClientHeight = 168
  ClientWidth = 257
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblBirthday: TLabel
    Left = 38
    Top = 100
    Width = 44
    Height = 13
    Alignment = taRightJustify
    Caption = 'Birthday:'
  end
  object edtName: TLabeledEdit
    Left = 88
    Top = 16
    Width = 121
    Height = 21
    EditLabel.Width = 34
    EditLabel.Height = 13
    EditLabel.Caption = 'Name: '
    LabelPosition = lpLeft
    MaxLength = 12
    TabOrder = 0
  end
  object edtPassword: TLabeledEdit
    Left = 88
    Top = 43
    Width = 121
    Height = 21
    EditLabel.Width = 53
    EditLabel.Height = 13
    EditLabel.Caption = 'Password: '
    LabelPosition = lpLeft
    MaxLength = 12
    TabOrder = 1
  end
  object edtPin: TLabeledEdit
    Left = 88
    Top = 70
    Width = 41
    Height = 21
    EditLabel.Width = 21
    EditLabel.Height = 13
    EditLabel.Caption = 'Pin: '
    LabelPosition = lpLeft
    MaxLength = 4
    NumbersOnly = True
    TabOrder = 2
  end
  object cbGM: TCheckBox
    Left = 152
    Top = 72
    Width = 57
    Height = 17
    Caption = 'GM'
    TabOrder = 3
  end
  object dtpBirthday: TDateTimePicker
    Left = 88
    Top = 97
    Width = 121
    Height = 21
    Date = 32874.000000000000000000
    Time = 32874.000000000000000000
    TabOrder = 4
  end
  object btnCreate: TButton
    Left = 96
    Top = 130
    Width = 75
    Height = 25
    Caption = 'Create'
    TabOrder = 5
    OnClick = btnCreateClick
  end
end
