object frmMapDebug: TfrmMapDebug
  Left = 0
  Top = 0
  Caption = 'Map Debug'
  ClientHeight = 500
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LVInfo: TListView
    Left = 0
    Top = 0
    Width = 472
    Height = 500
    Align = alClient
    Columns = <
      item
        Caption = 'ObjectID'
        Width = 60
      end
      item
        Caption = 'LifeType'
        Width = 100
      end
      item
        Caption = 'LifeID'
        Width = 80
      end
      item
        Caption = 'X'
      end
      item
        Caption = 'Y'
      end
      item
        Caption = 'Controller'
        Width = 80
      end>
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    ExplicitLeft = 187
    ExplicitTop = 98
    ExplicitWidth = 250
    ExplicitHeight = 150
  end
  object TmrUpdate: TTimer
    Enabled = False
    Interval = 3000
    OnTimer = TmrUpdateTimer
    Left = 28
    Top = 142
  end
end
