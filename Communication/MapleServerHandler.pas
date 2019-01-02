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

unit MapleServerHandler;

interface

uses IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdContext,
     IdTCPConnection, Classes, SysUtils, OpcodeHandler, MapleCrypt, MapleClient,
     Generics.Collections, WorldServer, DatabaseConnection, Windows,
     // Data providers
     BuffDataProvider, DropDataProvider, ItemDataProvider, QuestDataProvider,
     ReactorDataProvider, ShopDataProvider, SkillDataProvider, LifeDataProvider;

type
  TIVArray = array[0..3] of Byte;

const
  {$IFDEF VERSION83}
  MAPLE_VERSION = 83;
  PATCH_REVISION = '1';
  {$ENDIF}{$IFDEF VERSION90}
  MAPLE_VERSION = 90;
  PATCH_REVISION = '3';
  LOGIN_WORKAROUND = False;
  {$ENDIF}{$IFDEF VERSION97}
  MAPLE_VERSION = 97;
  PATCH_REVISION = '3';
  LOGIN_WORKAROUND = True;
  {$ENDIF}{$IFDEF VERSION99}
  MAPLE_VERSION = 99;
  PATCH_REVISION = '1';
  LOGIN_WORKAROUND = True;
  {$ENDIF}{$IFDEF VERSION102}
  MAPLE_VERSION = 102;
  PATCH_REVISION = '1';
  LOGIN_WORKAROUND = True;
  {$ENDIF}{$IFDEF EMS}
  MAPLE_VERSION = 63;
  PATCH_REVISION = '3';
  {$ENDIF}

  {$IFNDEF EMS}
  MAPLE_LOCALE = 8;  // Global
  {$ELSE}
  MAPLE_LOCALE = 9;  // Europe
  {$ENDIF}

  IV_RECV: TIVArray = ( 72, 101, 110, 100);
  IV_SEND: TIVArray = (105,  82,  48, 120);

  LOGIN_SERVER_PORT = 8484;
  BASE_CHANNEL_PORT = 8585;

  TICKER_MSG = 'Welcome to your Delphi based MapleStory Server!';

  MCDB_MAJOR_VERSION = 4;
  MCDB_MINOR_VERSION = 3;

type
  TMapleServerHandler = class
  private
    FLoginServer: TIdTCPServer;
    FWorldServers: TList<TWorldServer>;

    procedure LoginServerAfterBind(Sender: TObject);
    procedure LoginServerClientConnect(AContext: TIdContext);
    procedure LoginServerClientDisconnect(AContext: TIdContext);
    procedure LoginServerExecute(AContext: TIdContext);

    procedure CreateDataProviders;
    function VerifyMCDBVersion: Boolean;

    procedure SetAccountsLoggedOff;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadOpcodes;
    procedure RunLoginServer;
    procedure RunWorldServers;
    procedure Shutdown;

    property Worlds: TList<TWorldServer> read FWorldServers;
  end;

var
  OpHandler: TOpcodeHandler;
  MCDB: TDatabaseConnection;

  // Counter var, starting from BaseChannelPort
  RunningChannelPort: Word;

implementation

uses Main, MaplePacketCreator, Settings, PacketProcessor;

{ TMapleServerHandler }

constructor TMapleServerHandler.Create;
var
  i: Integer;
  MCDBCon: Boolean;
begin
  try
    // This class will connect to the MySQL database using ZeosLib
    MCDB := TDatabaseConnection.Create(frmSettings.edtMCDBName.Text);
    MCDBCon := True;
  except
    MCDBCon := False;
    Log('[WARNING] Could NOT connect to database "%s"! (%s)', [frmSettings.edtMCDBName.Text, Exception(ExceptObject).Message]);
  end;

  if MCDBCon and (not VerifyMCDBVersion) then
    Log('[WARNING] Your MCDB version does not match the required one (%d.%d)',
          [MCDB_MAJOR_VERSION, MCDB_MINOR_VERSION]);

  // Server is starting - set everyone logged off, it might have crashed before
  try
    SetAccountsLoggedOff;
  except
    Log('[FATAL] Connecting to main-database failed! (%s@%s)', [frmSettings.edtSQLName.Text, frmSettings.edtSQLHost.Text]);
  end;

  FLoginServer := TIdTCPServer.Create;
  FLoginServer.DefaultPort := LOGIN_SERVER_PORT;

  FLoginServer.OnAfterBind := LoginServerAfterBind;
  FLoginServer.OnConnect := LoginServerClientConnect;
  FLoginServer.OnDisconnect := LoginServerClientDisconnect;
  FLoginServer.OnExecute := LoginServerExecute;

  FLoginServer.IOHandler := TMapleServerIOHandler.Create;

  LoadOpcodes;

  FWorldServers := TList<TWorldServer>.Create;
  RunningChannelPort := BASE_CHANNEL_PORT;
  for i := 0 to frmSettings.LVWorlds.Items.Count - 1 do
    FWorldServers.Add(TWorldServer.Create(i));

  if MCDBCon then
    CreateDataProviders
  else
    Log('[WARNING] As the connection to MCDB couldn''t be established, you will experience massive problems!');
end;

procedure TMapleServerHandler.CreateDataProviders;
begin
  BuffDataProv := TBuffDataProvider.Create;
  DropDataProv := TDropDataProvider.Create;
  ItemDataProv := TItemDataProvider.Create;
  LifeDataProv := TLifeDataProvider.Create;
  ReactorDataProv := TReactorDataProvider.Create;
  ShopDataProv := TShopDataProvider.Create;
  SkillDataProv := TSkillDataProvider.Create;
  QuestDataProv := TQuestDataProvider.Create;
end;

destructor TMapleServerHandler.Destroy;
var
  World: TWorldServer;
begin
  FreeAndNil(FLoginServer);

  FreeAndNil(MCDB);
  FreeAndNil(OpHandler);

  for World in FWorldServers do
    World.Free;
  FreeAndNil(FWorldServers);

  FreeAndNil(BuffDataProv);
  FreeAndNil(DropDataProv);
  FreeAndNil(ItemDataProv);
  FreeAndNil(LifeDataProv);
  FreeAndNil(ReactorDataProv);
  FreeAndNil(ShopDataProv);
  FreeAndNil(SkillDataProv);
  FreeAndNil(QuestDataProv);

  inherited;
end;

procedure TMapleServerHandler.SetAccountsLoggedOff;
var
  DB: TDatabaseConnection;
begin
  DB := TDatabaseConnection.Create(frmSettings.edtSQLName.Text);
  try
    with DB.GetQuery do
    begin
      SQL.Text := 'UPDATE accounts SET loggedin = 0 WHERE loggedin > 0';
      ExecSQL;

      Free;
    end;
  finally
    DB.Free;
  end;
end;

procedure TMapleServerHandler.Shutdown;
var
  World: TWorldServer;
begin
  FLoginServer.Active := False;

  for World in FWorldServers do
    World.Shutdown;
end;

function TMapleServerHandler.VerifyMCDBVersion: Boolean;
begin
  with MCDB.GetQuery do
  begin
    SQL.Text := 'SELECT * FROM mcdb_info';
    Open;

    try
      if EOF then
        Exit(False);

      Result := (FieldByName('version').AsInteger = MCDB_MAJOR_VERSION) and
                (FieldByName('subversion').AsInteger = MCDB_MINOR_VERSION);

      if FieldByName('maple_version').AsInteger <> MAPLE_VERSION then
        Log('[WARNING] Your MCDB is for another MapleStory version!');
    finally
      Close;
      Free;
    end;
  end;
end;

procedure TMapleServerHandler.RunLoginServer;
begin
  FLoginServer.Active := True;
end;

procedure TMapleServerHandler.RunWorldServers;
var
  World: TWorldServer;
begin
  for World in FWorldServers do
    World.Run;
end;

procedure TMapleServerHandler.LoadOpcodes;
begin
  if OpHandler <> nil then
    OpHandler.Free;

  {$IFDEF VERSION83}
  OpHandler := TOpcodeHandler.Create(ExtractFilePath(ParamStr(0)) + 'OpcodesV83.ini');
  {$ENDIF}{$IFDEF VERSION90}
  OpHandler := TOpcodeHandler.Create(ExtractFilePath(ParamStr(0)) + 'OpcodesV90.ini');
  {$ENDIF}{$IFDEF VERSION97}
  OpHandler := TOpcodeHandler.Create(ExtractFilePath(ParamStr(0)) + 'OpcodesV97.ini');
  {$ENDIF}{$IFDEF VERSION99}
  OpHandler := TOpcodeHandler.Create(ExtractFilePath(ParamStr(0)) + 'OpcodesV99.ini');
  {$ENDIF}{$IFDEF VERSION102}
  OpHandler := TOpcodeHandler.Create(ExtractFilePath(ParamStr(0)) + 'OpcodesV102.ini');
  {$ENDIF}{$IFDEF EMS}
  OpHandler := TOpcodeHandler.Create(ExtractFilePath(ParamStr(0)) + 'OpcodesEMS.ini');
  {$ENDIF}
end;

procedure TMapleServerHandler.LoginServerAfterBind(Sender: TObject);
begin
  Log('Login-Server listening on port %d', [FLoginServer.DefaultPort]);
end;

procedure TMapleServerHandler.LoginServerClientConnect(AContext: TIdContext);
var
  Client: TMapleClient;
begin
  Log('Client connected to Login-Server: %s:%d',
      [AContext.Binding.PeerIP, AContext.Binding.PeerPort]);

  Client := TMapleClient.Create(AContext);
  AContext.Data := Client;
end;

procedure TMapleServerHandler.LoginServerClientDisconnect(AContext: TIdContext);
begin
  if Assigned(AContext.Data) and (AContext.Data is TMapleClient) then
  begin
    TMapleClient(AContext.Data).Free;
    AContext.Data := nil;
  end
  else   // very very weird error... I don't think this will ever happen
    Log('WARNING - couldn''t free client object, not assigned!');

  Log('Client disconnected from Login-Server: %s:%d',
      [AContext.Binding.PeerIP, AContext.Binding.PeerPort]);
end;

procedure TMapleServerHandler.LoginServerExecute(AContext: TIdContext);
var
  Data: TMemoryStream;
  Con: TIdTCPConnection;
begin
  Con := AContext.Connection;
  repeat
    if not Con.IOHandler.InputBufferIsEmpty then
    begin
      Data := TMemoryStream.Create;

      Con.IOHandler.InputBufferToStream(Data);
      //Log('Login-Server received %d bytes, handling...', [Data.Size]);
      if (not Assigned(AContext.Data)) or (not (AContext.Data is TMapleClient)) then
      begin
        // didn't happen yet... I think it works fine
        Log('[ !! FATAL !! ] AContext.Data not assigned!');
        Con.Disconnect;
      end
      else
        try
          TMapleClient(AContext.Data).ReceivePacket(Data);
        except
          Log('[EXCEPTION @ Login] ' + Exception(ExceptObject).Message);
          SaveLog;
        end;

      // Data will be freed by client (-> processor)
    end;
    SleepEx(1, True);    // avoid high cpu usage
  until (not Con.Connected) or (not FLoginServer.Active);
end;

end.
