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

unit WorldServer;

interface

uses SysUtils, Generics.Collections, ChannelServer, Utils, MapleParty;

type
  TWorldServer = class
  private
    FChannelServers: TList<TChannelServer>;
    FIndex: Byte;
    FName: string;
    FRunningPartyID: Integer;
    FParties: TDictionary<Integer, TMapleParty>;
  public
    constructor Create(const WorldIndex: Byte);
    destructor Destroy; override;

    procedure Run;
    procedure Shutdown;

    function Find(const ID: Integer): Integer; overload;
    function Find(const Name: string): Integer; overload;
    function IsConnected(const Name: string): Boolean;

    procedure Whisper(Sender, Target, Msg: string; Channel: Byte);
    procedure BuddyChat(Recipients: TList<Integer>; IDFrom: Integer; NameFrom, Msg: string);
    procedure PartyChat(PartyID: Integer; Msg, NameFrom: string);

    procedure LoggedOn(Name: string; CharID, Channel: Integer; Buddies: TArrayofInteger);
    procedure LoggedOff(Name: string; CharID, Channel: Integer; Buddies: TArrayofInteger);
    function MultiBuddyFind(CharIDFrom: Integer; CharIDs: TArrayofInteger): TList<TCharChannelInfo>;

    function CreateParty(Leader: TMaplePartyCharacter): TMapleParty;
    function DisbandParty(const PartyID: Integer): TMapleParty;
    function GetParty(const PartyID: Integer): TMapleParty;
    procedure UpdateParty(const PartyID: Integer; Operation: TPartyOperation; Target: TMaplePartyCharacter);

    property Index: Byte read FIndex;
    property Name: string read FName;
    property Channels: TList<TChannelServer> read FChannelServers;
  end;

implementation

uses Main, Settings, MapleServerHandler, MapleCharacter;

{ TWorldServer }

constructor TWorldServer.Create(const WorldIndex: Byte);
var
  i: Integer;
begin
  FIndex := WorldIndex;
  FName := frmSettings.LVWorlds.Items[FIndex].Caption;

  FChannelServers := TList<TChannelServer>.Create;
  for i := 0 to frmSettings.GetChannelCount(FIndex) - 1 do
  begin
    FChannelServers.Add(TChannelServer.Create(i, RunningChannelPort, Self));
    Inc(RunningChannelPort);
  end;

  FRunningPartyID := 1;
  FParties := TDictionary<Integer, TMapleParty>.Create;
end;

destructor TWorldServer.Destroy;
var
  Ch: TChannelServer;
begin
  for Ch in FChannelServers do
    Ch.Free;
  FreeAndNil(FChannelServers);
  FParties.Free;

  inherited;
end;

procedure TWorldServer.Run;
var
  Ch: TChannelServer;
begin
  for Ch in FChannelServers do
    Ch.Run;
end;

procedure TWorldServer.Shutdown;
var
  Ch: TChannelServer;
begin
  for Ch in FChannelServers do
    Ch.Shutdown;
end;

function TWorldServer.Find(const ID: Integer): Integer;
var
  Ch: TChannelServer;
begin
  for Ch in FChannelServers do
    if Ch.Player[ID] <> nil then
      Exit(Ch.Index);

  Result := -1;
end;

function TWorldServer.Find(const Name: string): Integer;
var
  Ch: TChannelServer;
begin
  for Ch in FChannelServers do
    if Ch.Player[Name] <> nil then
      Exit(Ch.Index);

  Result := -1;
end;

function TWorldServer.IsConnected(const Name: string): Boolean;
var
  Ch: TChannelServer;
  Char: TMapleCharacter;
begin
  Result := False;

  for Ch in FChannelServers do
    for Char in Ch.Players do
      if SameText(Char.Name, Name) then    // SameText ignores the case
        Exit(True);
end;

procedure TWorldServer.Whisper(Sender, Target, Msg: string; Channel: Byte);
var
  Ch: TChannelServer;
begin
  for Ch in FChannelServers do
    Ch.Whisper(Sender, Target, Msg, Channel);
end;

procedure TWorldServer.BuddyChat(Recipients: TList<Integer>; IDFrom: Integer;
  NameFrom, Msg: string);
var
  Ch: TChannelServer;
begin
  for Ch in FChannelServers do
    Ch.BuddyChat(Recipients, IDFrom, NameFrom, Msg);
end;

procedure TWorldServer.LoggedOn(Name: string; CharID, Channel: Integer;
  Buddies: TArrayofInteger);
var
  Ch: TChannelServer;
begin
  for Ch in FChannelServers do
    Ch.LoggedOn(Name, CharID, Channel, Buddies);
end;

procedure TWorldServer.LoggedOff(Name: string; CharID, Channel: Integer;
  Buddies: TArrayofInteger);
var
  Ch: TChannelServer;
begin
  for Ch in FChannelServers do
    Ch.LoggedOff(Name, CharID, Channel, Buddies);
end;

function TWorldServer.MultiBuddyFind(CharIDFrom: Integer;
  CharIDs: TArrayofInteger): TList<TCharChannelInfo>;
var
  Ch: TChannelServer;
  L: TList<Integer>;
  CID: Integer;
begin
  Result := TList<TCharChannelInfo>.Create;
  for Ch in FChannelServers do
  begin
    L := Ch.MultiBuddyFind(CharIDFrom, CharIDs);
    for CID in L do
      Result.Add(TCharChannelInfo.Create(CID, Ch.Index));
    L.Free;
  end;
end;

function TWorldServer.CreateParty(Leader: TMaplePartyCharacter): TMapleParty;
begin
  Result := TMapleParty.Create(FRunningPartyID, Leader);
  Inc(FRunningPartyID);
  FParties.Add(Result.ID, Result);
end;

function TWorldServer.DisbandParty(const PartyID: Integer): TMapleParty;
begin
  Result := FParties[PartyID];
  FParties.Remove(PartyID);
end;

function TWorldServer.GetParty(const PartyID: Integer): TMapleParty;
begin
  if not FParties.TryGetValue(PartyID, Result) then
    Result := nil;
end;

procedure TWorldServer.UpdateParty(const PartyID: Integer;
  Operation: TPartyOperation; Target: TMaplePartyCharacter);
var
  Party: TMapleParty;
  Ch: TChannelServer;
begin
  Party := GetParty(PartyID);
  if Party = nil then
    raise EArgumentException.Create('No party with the specified ID exists');

  case Operation of
    poJoin:           Party.Members.Add(Target);
    poExpel, poLeave: Party.Members.Remove(Target);
    poDisband:        DisbandParty(PartyID);
    poChangeLeader:   Party.Leader := Target;
    // update not required here
  end;

  for Ch in FChannelServers do
    Ch.UpdateParty(Party, Operation, Target);
end;

procedure TWorldServer.PartyChat(PartyID: Integer; Msg, NameFrom: string);
var
  Party: TMapleParty;
  Ch: TChannelServer;
begin
  Party := GetParty(PartyID);
  if Party = nil then
    raise EArgumentException.Create('No party with the specified ID exists');

  for Ch in FChannelServers do
    Ch.PartyChat(Party, Msg, NameFrom);
end;

end.
