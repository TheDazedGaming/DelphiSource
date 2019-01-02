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

unit ChannelServer;

interface

uses IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdContext,
     IdTCPConnection, Classes, SysUtils, OpcodeHandler, MapleCrypt, Utils,
     Generics.Collections, MapDataProvider, MapleCharacter, Windows, BuddyList,
     MapleParty, EventScript;

type
  TCharChannelInfo = record
    ID, Channel: Integer;

    constructor Create(AID, AChannel: Integer);
  end;

  TChannelServer = class
  private
    FIndex: Byte;
    FServer: TIdTCPServer;
    FWorld: TObject;
    FMapProvider: TMapDataProvider;
    FPlayers: TList<TMapleCharacter>;
    FEvents: TEventScriptManager;

    procedure ChannelAfterBind(Sender: TObject);
    procedure ChannelConnect(AContext: TIdContext);
    procedure ChannelExecute(AContext: TIdContext);
    procedure ChannelDisconnect(AContext: TIdContext);

    function GetPlayer(ID: Integer): TMapleCharacter; overload;
    function GetPlayer(Name: string): TMapleCharacter; overload;
    function GetPort: Word;

    procedure UpdateBuddies(CharID, Channel: Integer; Buddies: TArrayofInteger; Offline: Boolean);
  public
    constructor Create(Index, Port: Word; World: TObject);
    destructor Destroy; override;

    procedure AddPlayer(Char: TMapleCharacter);
    procedure RemovePlayer(Char: TMapleCharacter);

    procedure Run;
    procedure Shutdown;

    procedure Whisper(Sender, Target, Msg: string; Channel: Byte);
    procedure BuddyChat(Recipients: TList<Integer>; IDFrom: Integer; NameFrom, Msg: string);
    procedure PartyChat(const Party: TMapleParty; Msg, NameFrom: string);

    procedure BuddyChanged(CID, CIDFrom: Integer; Name: string; Channel: Byte; Operation: TBuddyOperation);
    function RequestBuddyAdd(AddName: string; ChannelFrom: Integer; Request: PPendingRequest): TBuddyAddResult;
    procedure LoggedOn(Name: string; CharID, Channel: Integer; Buddies: TArrayofInteger);
    procedure LoggedOff(Name: string; CharID, Channel: Integer; Buddies: TArrayofInteger);
    function MultiBuddyFind(CharIDFrom: Integer; CharIDs: TArrayofInteger): TList<Integer>;

    procedure UpdateParty(const Party: TMapleParty; Operation: TPartyOperation; Target: TMaplePartyCharacter);

    property Events: TEventScriptManager read FEvents;
    property Index: Byte read FIndex;
    property MapProvider: TMapDataProvider read FMapProvider;
    property Player[ID: Integer]: TMapleCharacter read GetPlayer; default;    // using this as ChannelX['char'] might be strange, I just defaulted it because overloading it doesn't work
    property Player[Name: string]: TMapleCharacter read GetPlayer; default;
    property Players: TList<TMapleCharacter> read FPlayers;
    property Port: Word read GetPort;
  end;

implementation

uses Main, MaplePacketCreator, MapleClient, MapleServerHandler, WorldServer,
     PacketProcessor;

{ TChannelServer }

constructor TChannelServer.Create(Index, Port: Word; World: TObject);
begin
  FIndex := Index;
  FWorld := World;

  FServer := TIdTCPServer.Create;
  FServer.DefaultPort := Port;

  FServer.OnAfterBind := ChannelAfterBind;
  FServer.OnConnect := ChannelConnect;
  FServer.OnExecute := ChannelExecute;
  FServer.OnDisconnect := ChannelDisconnect;

  FServer.IOHandler := TMapleServerIOHandler.Create;

  FMapProvider := TMapDataProvider.Create;
  FPlayers := TList<TMapleCharacter>.Create;
  FEvents := TEventScriptManager.Create;
end;

destructor TChannelServer.Destroy;
begin
  FreeAndNil(FServer);
  FreeAndNil(FMapProvider);
  FreeAndNil(FPlayers);
  FEvents.Free;

  inherited;
end;

procedure TChannelServer.AddPlayer(Char: TMapleCharacter);
begin
  FPlayers.Add(Char);
  {$IFNDEF EMS}
  TMapleClient(Char.Client).Write(ServerMessage(smScrollingTicker, 0, TICKER_MSG));
  {$ENDIF}
end;

procedure TChannelServer.RemovePlayer(Char: TMapleCharacter);
begin
  FPlayers.Remove(Char);
end;

procedure TChannelServer.Whisper(Sender, Target, Msg: string; Channel: Byte);
var
  DestChar: TMapleCharacter;
begin
  DestChar := GetPlayer(Target);
  if DestChar <> nil then
    TMapleClient(DestChar.Client).Write(MaplePacketCreator.Whisper(Sender, Msg, Channel));
end;

procedure TChannelServer.BuddyChat(Recipients: TList<Integer>; IDFrom: Integer;
  NameFrom, Msg: string);
var
  Char: TMapleCharacter;
  i: Integer;
begin
  for i in Recipients do
  begin
    Char := GetPlayer(i);
    if (Char <> nil) and (Char.BuddyList.ContainsVisible(IDFrom)) then
      TMapleClient(Char.Client).Write(GroupChat(NameFrom, Msg, 0));
  end;
end;

procedure TChannelServer.BuddyChanged(CID, CIDFrom: Integer; Name: string;
  Channel: Byte; Operation: TBuddyOperation);
var
  ActChar: TMapleCharacter;
begin
  ActChar := GetPlayer(CID);
  if ActChar <> nil then
    case Operation of
      boAdded:
      begin
        if ActChar.BuddyList.Contains(CIDFrom) then
        begin
          ActChar.BuddyList.Add(Name, DEFAULT_GROUP, CIDFrom, Channel, True);
          ActChar.BuddyList.SendUpdateChannel(CIDFrom, Channel);
        end;
      end;

      boDeleted:
      begin
        if ActChar.BuddyList.Contains(CIDFrom) then
        begin
          ActChar.BuddyList.Add(Name, DEFAULT_GROUP, CIDFrom, -1, ActChar.BuddyList.Buddy[CIDFrom].Visible);
          ActChar.BuddyList.SendUpdateChannel(CIDFrom, -1);
        end;
      end;
    end;
end;

function TChannelServer.RequestBuddyAdd(AddName: string; ChannelFrom: Integer;
  Request: PPendingRequest): TBuddyAddResult;
var
  AddChar: TMapleCharacter;
begin
  AddChar := GetPlayer(AddName);
  if AddChar <> nil then
  begin
    if AddChar.BuddyList.IsFull then
      Exit(brFull);

    if not AddChar.BuddyList.Contains(Request^.ID) then
      AddChar.BuddyList.AddBuddyRequest(ChannelFrom, Request)
    else
      if AddChar.BuddyList.ContainsVisible(Request^.ID) then
        Exit(brAlreadyOnList);
  end;

  Result := brOK;
end;

procedure TChannelServer.LoggedOn(Name: string; CharID, Channel: Integer;
  Buddies: TArrayofInteger);
begin
  UpdateBuddies(CharID, Channel, Buddies, False);
end;

procedure TChannelServer.LoggedOff(Name: string; CharID, Channel: Integer;
  Buddies: TArrayofInteger);
begin
  UpdateBuddies(CharID, Channel, Buddies, True);
end;

procedure TChannelServer.UpdateBuddies(CharID, Channel: Integer;
  Buddies: TArrayofInteger; Offline: Boolean);
var
  Buddy, DispChannel: Integer;
  Chr: TMapleCharacter;
  BLE: PBuddyListEntry;
begin
  for Buddy in Buddies do
  begin
    Chr := GetPlayer(Buddy);
    if Chr <> nil then
    begin
      BLE := Chr.BuddyList.Buddy[CharID];
      if (BLE <> nil) and (BLE^.Visible) then
      begin
        if Offline then
        begin
          BLE^.Channel := -1;
          DispChannel := -1;
        end
        else
        begin
          BLE^.Channel := Channel;
          DispChannel := Channel;
        end;

        Chr.BuddyList.SendUpdateChannel(BLE^.CID, DispChannel);
      end;
    end;
  end;
end;

function TChannelServer.MultiBuddyFind(CharIDFrom: Integer;
  CharIDs: TArrayofInteger): TList<Integer>;
var
  CID: Integer;
  Chr: TMapleCharacter;
begin
  Result := TList<Integer>.Create;
  for CID in CharIDs do
  begin
    Chr := GetPlayer(CID);
    if Chr <> nil then
      if Chr.BuddyList.ContainsVisible(CharIDFrom) then
        Result.Add(CID);           
  end;
end;

procedure TChannelServer.PartyChat(const Party: TMapleParty; Msg,
  NameFrom: string);
var
  MPC: TMaplePartyCharacter;
  Char: TMapleCharacter;
begin
  for MPC in Party do
    if (MPC.Channel = FIndex) and (MPC.Name <> NameFrom) then
    begin
      Char := GetPlayer(MPC.Name);
      if Char <> nil then
        TMapleClient(Char.Client).Write(GroupChat(NameFrom, Msg, 1));
    end;
end;

procedure TChannelServer.UpdateParty(const Party: TMapleParty;
  Operation: TPartyOperation; Target: TMaplePartyCharacter);
var
  MPC: TMaplePartyCharacter;
  Char: TMapleCharacter;
begin
  for MPC in Party do
    if MPC.Channel = FIndex then
    begin
      Char := Player[MPC.Name];
      if Char <> nil then
      begin
        if Operation = poDisband then
          Char.Party := nil
        else
          Char.Party := Party;

        TMapleClient(Char.Client).Write(MaplePacketCreator.UpdateParty(
          FIndex, Party, Operation, Target));
      end;
    end;

  if (Operation in [poLeave, poExpel]) and (Target.Channel = FIndex) then
  begin
    Char := Player[Target.Name];
    if Char <> nil then
    begin
      TMapleClient(Char.Client).Write(MaplePacketCreator.UpdateParty(
        FIndex, Party, Operation, Target));
      Char.Party := nil;
    end;
  end;
end;

function TChannelServer.GetPlayer(ID: Integer): TMapleCharacter;
begin
  for Result in FPlayers do
    if Result.ID = ID then
      Exit;

  Result := nil;
end;

function TChannelServer.GetPlayer(Name: string): TMapleCharacter;
begin
  for Result in FPlayers do
    if SameText(Result.Name, Name) then  // SameText ignores the case
      Exit;

  Result := nil;
end;

function TChannelServer.GetPort: Word;
begin
  Result := FServer.DefaultPort;
end;

procedure TChannelServer.ChannelAfterBind(Sender: TObject);
begin
  Log('%s-%d on port %d', [TWorldServer(FWorld).Name, FIndex + 1, TIdTCPServer(Sender).DefaultPort]);
end;

procedure TChannelServer.ChannelConnect(AContext: TIdContext);
var
  Client: TMapleClient;
begin
  Log('Client connected to %s-%d! %s:%d', [TWorldServer(FWorld).Name, FIndex + 1, AContext.Binding.PeerIP, AContext.Binding.PeerPort]);

  Client := TMapleClient.Create(AContext);
  Client.Channel := Self;
  Client.World := TWorldServer(FWorld);
  AContext.Data := Client;
end;

procedure TChannelServer.ChannelExecute(AContext: TIdContext);
var
  Con: TIdTCPConnection;
  Data: TMemoryStream;
begin
  Con := AContext.Connection;
  repeat
    if not Con.IOHandler.InputBufferIsEmpty then
    begin
      Data := TMemoryStream.Create;

      Con.IOHandler.InputBufferToStream(Data);
      //Log('%s-1 received %d bytes, handling...', [WorldNames[FWorld], Data.Size]);

      if (not Assigned(AContext.Data)) or (not (AContext.Data is TMapleClient)) then
      begin
        // didn't happen yet... I think it works fine
        Log('[Ch] FATAL - AContext.Data not assigned!');
        Con.Disconnect;
      end
      else
        try
          TMapleClient(AContext.Data).ReceivePacket(Data);
        except
          Log('[EXCEPTION - %s] %s', [TMapleClient(AContext.Data).Player.Name, Exception(ExceptObject).Message]);
          SaveLog;
        end;

      // Data will be freed by client (-> processor)
    end;
    SleepEx(1, True);
  until (not Con.Connected) or (not FServer.Active);
end;

procedure TChannelServer.ChannelDisconnect(AContext: TIdContext);
begin
  if Assigned(AContext.Data) and (AContext.Data is TMapleClient) then
  begin
    if Assigned(TMapleClient(AContext.Data).Player) then
      TMapleClient(AContext.Data).Player.SaveToDB(True);
    TMapleClient(AContext.Data).Free;
    AContext.Data := nil;
  end
  else   // very very weird error... I don't think this will ever happen
    Log('WARNING - couldn''t free client object, not assigned!');

  Log('Client disconnected from %s-%d: %s:%d',
    [TWorldServer(FWorld).Name, FIndex + 1, AContext.Binding.PeerIP, AContext.Binding.PeerPort]);
end;

procedure TChannelServer.Run;
begin
  FServer.Active := True;
end;

procedure TChannelServer.Shutdown;
begin
  FServer.Active := False;
end;

{ TCharChannelInfo }

constructor TCharChannelInfo.Create(AID, AChannel: Integer);
begin
  ID := AID;
  Channel := AChannel;
end;

end.
