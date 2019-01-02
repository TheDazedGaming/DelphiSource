(*
    This file is part of the Delphi MapleStory Server

    	Copyright (C) 2009-2011  Hendi

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

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MapleServerHandler, MTRand, Spin;

type
  TfrmGUI = class(TForm)
    mmLog: TMemo;
    btnListen: TButton;
    btnSettings: TButton;
    btnCreateAccount: TButton;
    procedure btnListenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnCreateAccountClick(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
  end;

procedure Log(Msg: string); overload;
procedure Log(Msg: string; Args: array of const); overload;
procedure SaveLog;

var
  frmGUI: TfrmGUI;

  MSH: TMapleServerHandler;

implementation

uses Settings, PacketProcessor, MaplePacketCreator, MapleStream, CreateAccount, MapDebug;

{$R *.dfm}

procedure Log(Msg: string);
begin
  frmGUI.mmLog.Lines.Add(TimeToStr(Now) + '  ' + Msg);
end;

procedure Log(Msg: string; Args: array of const);
begin
  frmGUI.mmLog.Lines.Add(TimeToStr(Now) + '  ' + Format(Msg, Args));
end;

procedure SaveLog;
begin
  frmGUI.mmLog.Lines.SaveToFile(ExtractFilePath(ParamStr(0)) + 'Log.txt');
end;

procedure TfrmGUI.btnListenClick(Sender: TObject);
begin
  if frmSettings.LVWorlds.Items.Count = 0 then
    raise Exception.Create('Please add worlds in the settings first!');

  MSH := TMapleServerHandler.Create;

  // Start accepting clients
  MSH.RunLoginServer;
  MSH.RunWorldServers;

  TButton(Sender).Enabled := False;
end;

procedure TfrmGUI.btnSettingsClick(Sender: TObject);
begin
  frmSettings.Show;
end;

procedure TfrmGUI.btnCreateAccountClick(Sender: TObject);
begin
  frmCreateAcc.Show;
end;

procedure TfrmGUI.FormCreate(Sender: TObject);
begin
  // Delphi Randomizer
  Randomize;

  // Mersenne Twister Randomizer
  Rand := TMTRand.Create;
  Rand.Randomize;
end;

procedure TfrmGUI.FormDblClick(Sender: TObject);
begin
  frmMapDebug.Show;
end;

procedure TfrmGUI.FormDestroy(Sender: TObject);
begin
  if Assigned(MSH) then
  begin
    MSH.Shutdown;
    FreeAndNil(MSH);
  end;

  FreeAndNil(Rand);
end;

procedure TfrmGUI.FormShow(Sender: TObject);
//var
  //Cl: THandlerClass;
begin
  {$IFDEF EMS}
  Log('Welcome, I''m configured for MapleStory Europe v%d', [MAPLE_VERSION]);
  {$ELSE}
  Log('Welcome, I''m configured for MapleStory Global v%d', [MAPLE_VERSION]);
  {$ENDIF}

  Log('Registered packet handlers: %d', [HandlerClasses.Count]);
 (*for Cl in HandlerClasses.Keys do
    Log(Cl.ClassName);  *)

  if frmSettings.cbListenonStartup.Checked then
    btnListen.Click;
end;

end.
