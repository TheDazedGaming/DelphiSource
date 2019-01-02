object frmSettings: TfrmSettings
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Settings'
  ClientHeight = 286
  ClientWidth = 407
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnSave: TButton
    Left = 176
    Top = 244
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 0
    OnClick = btnSaveClick
  end
  object pcSettings: TPageControl
    Left = 0
    Top = 0
    Width = 407
    Height = 231
    ActivePage = tsConnection
    Align = alTop
    TabOrder = 1
    object tsConnection: TTabSheet
      Caption = 'Connection'
      object gbDatabase: TGroupBox
        Left = 0
        Top = 0
        Width = 399
        Height = 129
        Align = alTop
        Caption = 'MySQL Database'
        TabOrder = 0
        object lblSQLHost: TLabel
          Left = 24
          Top = 24
          Width = 26
          Height = 13
          Caption = 'Host:'
        end
        object lblSQLPort: TLabel
          Left = 24
          Top = 75
          Width = 24
          Height = 13
          Caption = 'Port:'
        end
        object lblSQLName: TLabel
          Left = 152
          Top = 24
          Width = 79
          Height = 13
          Caption = 'Database name:'
        end
        object lblSQLUser: TLabel
          Left = 152
          Top = 75
          Width = 52
          Height = 13
          Caption = 'Username:'
        end
        object lblPassword: TLabel
          Left = 264
          Top = 75
          Width = 50
          Height = 13
          Caption = 'Password:'
        end
        object lblMCDBName: TLabel
          Left = 264
          Top = 24
          Width = 61
          Height = 13
          Caption = 'MCDB name:'
        end
        object edtSQLHost: TEdit
          Left = 24
          Top = 40
          Width = 113
          Height = 21
          TabOrder = 0
          Text = 'localhost'
        end
        object seSQLPort: TSpinEdit
          Left = 24
          Top = 92
          Width = 65
          Height = 22
          MaxValue = 65535
          MinValue = 0
          TabOrder = 1
          Value = 3306
        end
        object edtSQLName: TEdit
          Left = 152
          Top = 40
          Width = 89
          Height = 21
          TabOrder = 2
          Text = 'delphims'
        end
        object edtSQLUser: TEdit
          Left = 152
          Top = 92
          Width = 89
          Height = 21
          TabOrder = 3
          Text = 'root'
        end
        object edtSQLPW: TEdit
          Left = 264
          Top = 92
          Width = 97
          Height = 21
          TabOrder = 4
        end
        object edtMCDBName: TEdit
          Left = 264
          Top = 40
          Width = 97
          Height = 21
          TabOrder = 5
          Text = 'mcdb95'
        end
      end
      object gbGeneral: TGroupBox
        Left = 0
        Top = 129
        Width = 399
        Height = 72
        Align = alTop
        Caption = 'General'
        TabOrder = 1
        object lblCSIP: TLabel
          Left = 30
          Top = 46
          Width = 92
          Height = 13
          Caption = 'Channel-Server IP:'
        end
        object cbListenonStartup: TCheckBox
          Left = 24
          Top = 20
          Width = 225
          Height = 17
          Caption = 'Start listening on startup'
          TabOrder = 0
        end
        object edtCSIP: TEdit
          Left = 134
          Top = 43
          Width = 147
          Height = 21
          TabOrder = 1
          Text = '127.0.0.1'
        end
      end
    end
    object tsWorlds: TTabSheet
      Caption = 'Worlds'
      ImageIndex = 1
      object lblRightClick: TLabel
        Left = 0
        Top = 153
        Width = 399
        Height = 13
        Align = alTop
        Alignment = taCenter
        Caption = '-> Rightclick on the list for more options.'
        Layout = tlCenter
        ExplicitWidth = 195
      end
      object lblEventMsg: TLabel
        Left = 16
        Top = 178
        Width = 182
        Height = 13
        Caption = 'Event-Message at Channel-selection: '
      end
      object LVWorlds: TListView
        Left = 0
        Top = 0
        Width = 399
        Height = 153
        Align = alTop
        Columns = <
          item
            Caption = 'Worldname'
            Width = 250
          end
          item
            Caption = 'Channel Count'
            Width = 120
          end>
        RowSelect = True
        PopupMenu = pmRight
        TabOrder = 0
        ViewStyle = vsReport
        OnContextPopup = LVWorldsContextPopup
        OnDblClick = LVWorldsDblClick
      end
      object edtEventMsg: TEdit
        Left = 204
        Top = 175
        Width = 165
        Height = 21
        TabOrder = 1
        Text = 'Welcome to HendiMS'
      end
    end
    object tsFeatures: TTabSheet
      Caption = 'Features'
      ImageIndex = 2
      object cbEnablePins: TCheckBox
        Left = 16
        Top = 16
        Width = 137
        Height = 17
        Caption = 'Enable pin-system'
        TabOrder = 0
      end
      object gbRates: TGroupBox
        Left = 16
        Top = 48
        Width = 257
        Height = 97
        Caption = 'Rates'
        TabOrder = 1
        object Label1: TLabel
          Left = 16
          Top = 28
          Width = 57
          Height = 13
          Caption = 'Experience:'
        end
        object Label2: TLabel
          Left = 46
          Top = 56
          Width = 27
          Height = 13
          Caption = 'Drop:'
        end
        object Label3: TLabel
          Left = 150
          Top = 56
          Width = 29
          Height = 13
          Caption = 'Meso:'
        end
        object seEXPRate: TSpinEdit
          Left = 79
          Top = 24
          Width = 65
          Height = 22
          MaxValue = 1000
          MinValue = 1
          TabOrder = 0
          Value = 1
        end
        object seDropRate: TSpinEdit
          Left = 79
          Top = 52
          Width = 65
          Height = 22
          MaxValue = 100
          MinValue = 1
          TabOrder = 1
          Value = 1
        end
        object seMesoRate: TSpinEdit
          Left = 187
          Top = 52
          Width = 57
          Height = 22
          MaxValue = 1000
          MinValue = 1
          TabOrder = 2
          Value = 1
        end
      end
      object GroupBox1: TGroupBox
        Left = 16
        Top = 151
        Width = 257
        Height = 49
        Caption = 'Inventory'
        TabOrder = 2
        object Label4: TLabel
          Left = 24
          Top = 20
          Width = 97
          Height = 13
          Caption = 'Standard Slot-Limit: '
        end
        object seSlotLimit: TSpinEdit
          Left = 127
          Top = 17
          Width = 52
          Height = 22
          MaxValue = 100
          MinValue = 24
          TabOrder = 0
          Value = 24
        end
      end
      object cbEnablePIC: TCheckBox
        Left = 167
        Top = 16
        Width = 146
        Height = 17
        Caption = 'Enable PIC-System'
        TabOrder = 3
      end
    end
  end
  object pmRight: TPopupMenu
    Left = 328
    Top = 104
    object Addworld1: TMenuItem
      Caption = 'Add world'
      OnClick = Addworld1Click
    end
    object Deleteworld1: TMenuItem
      Caption = 'Delete world'
      OnClick = Deleteworld1Click
    end
    object ChangeChannelcount1: TMenuItem
      Caption = 'Change Channel Count'
      ShortCut = 16451
      OnClick = ChangeChannelcount1Click
    end
  end
end
