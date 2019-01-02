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

unit PlayerLoggedinHandler;

interface

uses SysUtils, PacketProcessor, MapleClient, MapleStream, MaplePacketCreator, MapleCharacter,
     Generics.Collections, Utils, ChannelServer, BuddyList, MapleParty;

type
  TPlayerLoggedinHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

implementation

uses Main;

{ TPlayerLoggedinHandler }

class procedure TPlayerLoggedinHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  ID: Integer;
  Player: TMapleCharacter;
  State: TLoginState;
  CharName: string;
  AllowLogin: Boolean;
  Names: TList<string>;
  BuddyIDs: TArrayofInteger;
  OnlineBuddies: TList<TCharChannelInfo>;
  OnlineBuddy: TCharChannelInfo;
  Pending: PPendingRequest;
begin
  ID := Packet.ReadInt;
  try
    Player := TMapleCharacter.LoadFromDB(ID, C, True);
    C.Player := Player;
  except
    Log('Couldn''t load character! ' + Exception(ExceptObject).Message);
    SaveLog;
    raise;
  end;
  C.AccID := Player.AccID;

  AllowLogin := True;
  State := C.GetLoginState;
  if State = lsServerTransition then
  begin
    Names := C.LoadCharacterNames(C.World.Index);
    try
      for CharName in Names do
        if C.World.IsConnected(CharName) then
        begin
          Log('[WARNING] Attempting to doublelogin with acc ' + IntToStr(C.AccID));
          AllowLogin := False;
          Break;
        end;
    finally
      Names.Free;
    end;
  end;

  if (State <> lsServerTransition) or (not AllowLogin) then
  begin
    C.Player := nil;
    C.Disconnect;
    Exit;
  end;
  C.UpdateLoginState(lsLoggedIn);

  C.Write(GetCharInfo(Player));

  C.Channel.AddPlayer(Player);
  Player.Map.AddPlayer(Player);

  C.Write(KeymapPacket(Player.Keymap));
  C.Write(QuickSlotPacket(Player.QuickSlot));

  // Buddylist
  BuddyIDs := Player.BuddyList.GetBuddyIDs;
  C.World.LoggedOn(Player.Name, Player.ID, C.Channel.Index, BuddyIDs);
  OnlineBuddies := C.World.MultiBuddyFind(Player.ID, BuddyIDs);
  try
    for OnlineBuddy in OnlineBuddies do
      Player.BuddyList.Buddy[OnlineBuddy.ID]^.Channel := OnlineBuddy.Channel;
  finally
    OnlineBuddies.Free;
  end;
  BuddyIDs := nil;

  Player.BuddyList.SendUpdate;

  Pending := Player.BuddyList.PollPendingRequest;
  if Pending <> nil then
  begin
    Player.BuddyList.Add(Pending.Name, DEFAULT_GROUP, Pending.ID, -1, False);
    Player.BuddyList.SendRequest(Pending);
    Dispose(Pending);
  end;

  // Party
  if Player.Party <> nil then
  begin
    C.Player.SilentPartyUpdate;
    Player.UpdatePartyMemberHP;
  end;
end;

initialization
  HandlerClasses.Add('PlayerLoggedin', TPlayerLoggedinHandler);

finalization

end.
