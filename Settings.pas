(*
    This file is part of the Delphi MapleStory Server

    	Copyright (C) 2009-2010  Hendi

    The code contains portions of:

	    OdinMS
	    KryptoDEV Source
	    Vana

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License version 3
    as published by the Free Software Foundation. You may not use, modify
    or distribute this program under any other version of the
    GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License <http://www.gnu.org/licenses/>
    for more details.
*)

unit Settings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, IniFiles, ComCtrls, Menus, MaplePacketCreator, Utils,
  DatabaseConnection, ZDataset;

type
  TfrmSettings = class(TForm)
    btnSave: TButton;
    pmRight: TPopupMenu;
    Addworld1: TMenuItem;
    Deleteworld1: TMenuItem;
    pcSettings: TPageControl;
    tsConnection: TTabSheet;
    gbDatabase: TGroupBox;
    lblSQLHost: TLabel;
    lblSQLPort: TLabel;
    lblSQLName: TLabel;
    lblSQLUser: TLabel;
    lblPassword: TLabel;
    lblMCDBName: TLabel;
    edtSQLHost: TEdit;
    seSQLPort: TSpinEdit;
    edtSQLName: TEdit;
    edtSQLUser: TEdit;
    edtSQLPW: TEdit;
    edtMCDBName: TEdit;
    gbGeneral: TGroupBox;
    lblCSIP: TLabel;
    cbListenonStartup: TCheckBox;
    edtCSIP: TEdit;
    tsWorlds: TTabSheet;
    lblRightClick: TLabel;
    LVWorlds: TListView;
    tsFeatures: TTabSheet;
    cbEnablePins: TCheckBox;
    gbRates: TGroupBox;
    seEXPRate: TSpinEdit;
    seDropRate: TSpinEdit;
    seMesoRate: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    seSlotLimit: TSpinEdit;
    ChangeChannelcount1: TMenuItem;
    edtEventMsg: TEdit;
    lblEventMsg: TLabel;
    cbEnablePIC: TCheckBox;
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Addworld1Click(Sender: TObject);
    procedure Deleteworld1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ChangeChannelcount1Click(Sender: TObject);
    procedure LVWorldsDblClick(Sender: TObject);
    procedure LVWorldsContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    FLimit: Byte;

    procedure ChangeChannelCount(Item: TListItem);
  public
    function GetChannelCount(const World: Byte): Integer;
  end;

var
  frmSettings: TfrmSettings;

implementation

uses MapleServerHandler;

{$R *.dfm}

procedure TfrmSettings.Addworld1Click(Sender: TObject);
begin
  if LVWorlds.Items.Count < 5 then
  begin
    with LVWorlds.Items.Add do
    begin
      Caption := WorldNames[Index];
      SubItems.Add('');
    end;
  end
  else
    raise Exception.Create('If you want to add more worlds, please add the names in MaplePacketCreator first!');
end;

procedure TfrmSettings.btnSaveClick(Sender: TObject);
var
  Ini: TIniFile;
  i, CharID: Integer;
  CurSlots, NewSlots: array[0..3] of Byte;
  UQ: TZQuery;
begin
  edtCSIP.Text := StringReplace(LowerCase(edtCSIP.Text), 'localhost', '127.0.0.1', [rfReplaceAll]);

  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Settings.ini');
  try
    // Database
    Ini.WriteString('Database', 'Host', edtSQLHost.Text);
    Ini.WriteInteger('Database', 'Port', seSQLPort.Value);
    Ini.WriteString('Database', 'Name', edtSQLName.Text);
    Ini.WriteString('Database', 'User', edtSQLUser.Text);
    Ini.WriteString('Database', 'Password', edtSQLPW.Text);
    Ini.WriteString('Database', 'MCDBName', edtMCDBName.Text);

    // General
    Ini.WriteBool('General', 'ListenonStartup', cbListenonStartup.Checked);
    Ini.WriteString('General', 'ChannelIP', edtCSIP.Text);

    // Features
    Ini.WriteBool('Features', 'EnablePinSystem', cbEnablePins.Checked);
    {$IFDEF VERSION82_UP}
    Ini.WriteBool('Features', 'EnablePICSystem', cbEnablePIC.Checked);
    {$ENDIF}
    // - Rates
    Ini.WriteInteger('Features', 'EXPRate', seEXPRate.Value);
    Ini.WriteInteger('Features', 'DropRate', seDropRate.Value);
    Ini.WriteInteger('Features', 'MesoRate', seMesoRate.Value);
    // - Inventory
    Ini.WriteInteger('Features', 'SlotLimit', seSlotLimit.Value);

    // Worlds
    Ini.WriteInteger('Worlds', 'Count', LVWorlds.Items.Count);

    for i := 0 to LVWorlds.Items.Count - 1 do
      Ini.WriteInteger('Worlds', Format('World%dChannels', [i]), StrToInt(LVWorlds.Items[i].SubItems[0]));

    Ini.WriteString('Worlds', 'EventMsg', edtEventMsg.Text);
  finally
    Ini.Free;
  end;

  // Update the characters' slot limit if changed
  if FLimit <> seSlotLimit.Value then
    with TDatabaseConnection.Create(edtSQLName.Text) do
    begin
      UQ := GetQuery;
      UQ.SQL.Text := 'UPDATE characters SET equipSlots = :eqs, useSlots = :us, setupSlots = :sets, etcSlots = :etcs WHERE id = :id';

      with GetQuery do
      begin
        SQL.Text := 'SELECT id, equipSlots, useSlots, setupSlots, etcSlots FROM characters';
        Open;

        while not EOF do
        begin
          CurSlots[0] := FieldByName('equipSlots').AsInteger;
          CurSlots[1] := FieldByName('useSlots').AsInteger;
          CurSlots[2] := FieldByName('setupSlots').AsInteger;
          CurSlots[3] := FieldByName('etcSlots').AsInteger;
          CharID := FieldByName('id').AsInteger;

          for i := 0 to 3 do
            if CurSlots[i] - FLimit > 0 then
              NewSlots[i] := seSlotLimit.Value + (CurSlots[i] - FLimit)
            else
              NewSlots[i] := seSlotLimit.Value;

          UQ.ParamByName('eqs').Value := Integer(NewSlots[0]);
          UQ.ParamByName('us').Value := Integer(NewSlots[1]);
          UQ.ParamByName('sets').Value := Integer(NewSlots[2]);
          UQ.ParamByName('etcs').Value := Integer(NewSlots[3]);
          UQ.ParamByName('id').Value := CharID;
          UQ.ExecSQL;

          Next;
        end;

        Free;
      end;

      UQ.Free;
      Free;
    end;

  Close;
end;

procedure TfrmSettings.ChangeChannelCount(Item: TListItem);
var
  NewCC: string;
  C: Char;
begin
  NewCC := InputBox('Change Channel Count', 'New Channel Count:', Item.SubItems[0]);
  for C in NewCC do
    if not CharInSet(C, ['0'..'9']) then
      Exit;

  if NewCC = '0' then
    Exit;

  Item.SubItems[0] := NewCC;
end;

procedure TfrmSettings.ChangeChannelcount1Click(Sender: TObject);
begin
  ChangeChannelCount(LVWorlds.Selected);
end;

procedure TfrmSettings.Deleteworld1Click(Sender: TObject);
begin
  if LVWorlds.Items.Count = 0 then
    Exit;

  LVWorlds.Items[LVWorlds.Items.Count - 1].Delete;
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
  Count, i: Integer;
begin
  cbEnablePIC.Visible := {$IFDEF VERSION82_UP} True {$ELSE} False {$ENDIF};

  pcSettings.ActivePageIndex := 0;

  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Settings.ini');
  try
    // Database
    edtSQLHost.Text := Ini.ReadString('Database', 'Host', 'localhost');
    seSQLPort.Value := Ini.ReadInteger('Database', 'Port', 3306);
    edtSQLName.Text := Ini.ReadString('Database', 'Name', 'delphims');
    edtSQLUser.Text := Ini.ReadString('Database', 'User', 'root');
    edtSQLPW.Text := Ini.ReadString('Database', 'Password', '');
    edtMCDBName.Text := Ini.ReadString('Database', 'MCDBName', 'mcdb' + IntToStr(MAPLE_VERSION));

    // General
    cbListenonStartup.Checked := Ini.ReadBool('General', 'ListenonStartup', False);
    edtCSIP.Text := Ini.ReadString('General', 'ChannelIP', '127.0.0.1');

    // Features
    cbEnablePins.Checked := Ini.ReadBool('Features', 'EnablePinSystem', False);
    cbEnablePIC.Checked := cbEnablePIC.Visible and Ini.ReadBool('Features', 'EnablePICSystem', False);
    // - Rates
    seEXPRate.Value := Ini.ReadInteger('Features', 'EXPRate', 1);
    seDropRate.Value := Ini.ReadInteger('Features', 'DropRate', 1);
    seMesoRate.Value := Ini.ReadInteger('Features', 'MesoRate', 1);
    // - Inventory
    seSlotLimit.Value := Ini.ReadInteger('Features', 'SlotLimit', 24);

    // Worlds
    Count := Ini.ReadInteger('Worlds', 'Count', 0);
    if Count = 0 then
      Exit;

    for i := 0 to Count - 1 do
      with LVWorlds.Items.Add do
      begin
        Caption := WorldNames[i];
        SubItems.Add(IntToStr(Ini.ReadInteger('Worlds', Format('World%dChannels', [i]), 1)))
      end;

    edtEventMsg.Text := Ini.ReadString('Worlds', 'EventMsg', 'Welcome to HendiMS');
  finally
    Ini.Free;
  end;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  FLimit := seSlotLimit.Value;
end;

function TfrmSettings.GetChannelCount(const World: Byte): Integer;
begin
  if World > LVWorlds.Items.Count - 1 then
    Exit(0);

  Result := StrToInt(LVWorlds.Items[World].SubItems[0]);
end;

procedure TfrmSettings.LVWorldsContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  ChangeChannelCount1.Visible := LVWorlds.Selected <> nil;
end;

procedure TfrmSettings.LVWorldsDblClick(Sender: TObject);
begin
  if LVWorlds.Selected <> nil then
    ChangeChannelCount(LVWorlds.Selected);
end;

end.
