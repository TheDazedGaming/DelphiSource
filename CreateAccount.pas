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

unit CreateAccount;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, DatabaseConnection, ZDataset;

type
  TfrmCreateAcc = class(TForm)
    edtName: TLabeledEdit;
    edtPassword: TLabeledEdit;
    edtPin: TLabeledEdit;
    cbGM: TCheckBox;
    dtpBirthday: TDateTimePicker;
    lblBirthday: TLabel;
    btnCreate: TButton;
    procedure btnCreateClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCreateAcc: TfrmCreateAcc;

implementation

uses Settings;

{$R *.dfm}

procedure TfrmCreateAcc.btnCreateClick(Sender: TObject);
var
  DB: TDatabaseConnection;
begin
  if (Length(edtName.Text) < 4) or (Length(edtPassword.Text) < 4) or (Length(edtPin.Text) < 4) then
    raise Exception.Create('The name, password & pin must be at least 4 chars long!');

  DB := TDatabaseConnection.Create(frmSettings.edtSQLName.Text);
  try
    with DB.GetQuery do
    begin
      try
        SQL.Text := Format('SELECT loggedin FROM accounts WHERE name = ''%s''', [edtName.Text]);
        Open;

        if not EOF then
          raise Exception.CreateFmt('The account %s already exists!', [edtName.Text]);

        Close;

        SQL.Text := 'INSERT INTO accounts (name, password, loggedin, createdat, birthday, banned, tempban, gm, pin) VALUES (' +
                    '?, ?, ?, ?, ?, ?, ?, ?, ?)';
        Prepare;
        with DBCStatement do
        begin
          SetString(1, AnsiString(edtName.Text));
          SetString(2, AnsiString(edtPassword.Text));    // the hashed password & salt will be set by MapleClient later
          SetInt(3, 0);
          SetDate(4, Now);
          SetDate(5, dtpBirthday.Date);
          SetInt(6, 0);
          SetInt(7, 0);
          SetInt(8, Ord(cbGM.Checked));
          SetInt(9, StrToInt(edtPin.Text));

          ExecuteUpdatePrepared;
        end;
        Unprepare;
      finally
        Free;
      end;
    end;
  finally
    DB.Free;
  end;

  Close;
end;

end.
