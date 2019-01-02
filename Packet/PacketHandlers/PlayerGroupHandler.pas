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

unit PlayerGroupHandler;

interface

uses SysUtils, PacketProcessor, MapleClient, MapleStream, MaplePacketCreator, BuddyList,
     DatabaseConnection, ChannelServer, Utils, MapleParty;

type
  // Buddy
  TSmallInfo = record
    ID, BuddyCapacity: Integer;
    Name: string;

    constructor Create(AID, ABuddyCapacity: Integer; AName: string);
  end;

  TModifyMode = (mmAdd = 1, mmAccept, mmDelete);

  TModifyBuddylistHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;
  // ======

  // Party
  TPartyAction = (paCreate = 1, paLeave, paAccept, paInvite, paExpel, paChangeLeader,
                  raInvited = $16, raAlreadyInvited = $19, raDenied, raAccepted);  // New RequestAnswer opcode in 0.88+

  TPartyOperationHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TDenyPartyRequestHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;
  // ======

implementation

uses Main, MapleCharacter;

{ TModifyBuddylistHandler }

function GetInfo(AName: string; Con: TDatabaseConnection): TSmallInfo;
begin
  with Con.GetQuery do
  begin
    SQL.Text := 'SELECT id, name, buddyCapacity FROM characters WHERE name LIKE :cname';
    ParamByName('cname').Value := AnsiString(AName);
    Open;

    if not EOF then
      Result := TSmallInfo.Create(FieldByName('id').AsInteger, FieldByName('buddyCapacity').AsInteger,
                                  FieldByName('name').AsString)
    else
      Result.ID := -1;

    Free;
  end;
end;

procedure NotifyRemoteChannel(C: TMapleClient; RemoteChannel, OtherCID: Integer;
  Operation: TBuddyOperation);
begin
  if RemoteChannel > -1 then
    TChannelServer(C.World.Channels[RemoteChannel]).BuddyChanged(OtherCID,
      C.Player.ID, C.Player.Name, C.Channel.Index, Operation);
end;

class procedure TModifyBuddylistHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Mode: TModifyMode;
  Player, OtherChar: TMapleCharacter;
  AddName, Group, OtherName: string;
  BLE: PBuddyListEntry;
  Info: TSmallInfo;
  Channel, DisplayChannel, OtherCID: Integer;
  AddRes: TBuddyAddResult;
  ChannelSrv: TChannelServer;
begin
  Mode := TModifyMode(Packet.ReadByte);
  Player := C.Player;

  case Mode of
    mmAdd:
    begin
      AddName := Packet.ReadMapleAnsiString;
      BLE := Player.BuddyList[AddName];
      Group := Packet.ReadMapleAnsiString;

      // Already on list
      if (BLE <> nil) and (not BLE^.Visible) then
      begin
        BLE^.Group := Group;
        Player.BuddyList.SendUpdate;
        Exit;
      end
      else  // Maybe list full?
        if Player.BuddyList.IsFull then
        begin
          Player.BuddyList.SendError(bmFull);
          Exit;
        end;

      Info.ID := -1;
      OtherChar := C.Channel.Player[AddName];

      if OtherChar <> nil then
      begin
        Channel := C.Channel.Index;
        Info := TSmallInfo.Create(OtherChar.ID, OtherChar.Buddylist.Capacity, OtherChar.Name);
      end
      else
      begin
        Channel := C.World.Find(AddName);
        Info := GetInfo(AddName, C.DB);
      end;

      // Character we want to add doesn't exist
      if Info.ID = -1 then
      begin
        Player.BuddyList.SendError(bmNameInvalid);
        Exit;
      end;

      AddRes := brOK;
      if Channel > -1 then
      begin
        // Other player is online; can request directly
        ChannelSrv := C.World.Channels[Channel];
        AddRes := ChannelSrv.RequestBuddyAdd(AddName, C.Channel.Index,
          CreateRequest(Player.Name, Player.ID, Word(Player.Job), Player.Level));
      end
      else   // Write to database that someone wants to add the other char
        with C.DB.GetQuery do
        begin
          SQL.Text := 'SELECT COUNT(*) as buddyCount FROM buddies WHERE characterid = :id AND pending = 0';
          ParamByName('id').Value := Info.ID;
          Open;
          try
            if EOF then
              raise Exception.Create('The unbelievable happened.')
            else
              if FieldByName('buddyCount').AsInteger >= Info.BuddyCapacity then
                AddRes := brFull;
            Close;

            SQL.Text := 'SELECT pending FROM buddies WHERE characterid = :cid AND buddyid = :bid';
            ParamByName('cid').Value := Info.ID;
            ParamByName('bid').Value := Player.ID;
            Open;

            if not EOF then
              AddRes := brAlreadyOnList;
          finally
            Close;
            Free;
          end;
        end;

      if AddRes = brFull then
        Player.BuddyList.SendError(bmOppositeFull)
      else
      begin
        DisplayChannel := -1;

        if (AddRes = brAlreadyOnList) and (Channel <> -1) then
        begin
          DisplayChannel := Channel;
          NotifyRemoteChannel(C, Channel, Info.ID, boAdded);
        end
        else
        if (AddRes <> brAlreadyOnList) and (Channel = -1) then
          with C.DB.GetQuery do
            try
              SQL.Text := 'INSERT INTO buddies (characterid, `buddyid`, `group`, `pending`) VALUES (:cid, :bid, :group, 1)';
              ParamByName('cid').Value := Info.ID;
              ParamByName('bid').Value := Player.ID;
              ParamByName('group').Value := AnsiString(Group);
              ExecSQL;
            finally
              Free;
            end;

        Player.BuddyList.Add(Info.Name, Group, Info.ID, DisplayChannel, True);
        Player.BuddyList.SendUpdate;
      end;
    end;

    mmAccept:
    begin
      OtherCID := Packet.ReadInt;
      if not Player.BuddyList.IsFull then
      begin
        Channel := C.World.Find(OtherCID);
        OtherName := '';
        OtherChar := C.Channel.Player[OtherCID];

        if OtherChar = nil then
        begin
          with C.DB.GetQuery do
          begin
            SQL.Text := 'SELECT name FROM characters WHERE id = :id';
            ParamByName('id').Value := OtherCID;
            Open;

            if not EOF then
              OtherName := FieldByName('name').AsString;

            Close;
            Free;
          end;
        end
        else
          OtherName := OtherChar.Name;

        if OtherName <> '' then
        begin
          Player.BuddyList.Add(OtherName, DEFAULT_GROUP, OtherCID, Channel, True);
          Player.BuddyList.SendUpdate;
          NotifyRemoteChannel(C, Channel, OtherCID, boAdded);
        end;
      end;

      Player.BuddyList.SendNextRequest;
    end;

    mmDelete:
    begin
      OtherCID := Packet.ReadInt;

      if Player.BuddyList.ContainsVisible(OtherCID) then
        NotifyRemoteChannel(C, C.World.Find(OtherCID),
          OtherCID, boDeleted);

      Player.BuddyList.Buddies.Remove(OtherCID);
      Player.BuddyList.SendUpdate;
      Player.BuddyList.SendNextRequest;
    end;

    else Log('Unknown Buddymode: ' + IntToStr(Byte(Mode)));
  end;
end;

{ TSmallInfo }

constructor TSmallInfo.Create(AID, ABuddyCapacity: Integer; AName: string);
begin
  ID := AID;
  BuddyCapacity := ABuddyCapacity;
  Name := AName;
end;

{ TPartyOperationHandler }

class procedure TPartyOperationHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Mode: TPartyAction;
  Name: string;
  Char: TMapleCharacter;
  ID: Integer;
  Party: TMapleParty;
  Member: TMaplePartyCharacter;
begin
  Mode := TPartyAction(Packet.ReadByte);

  case Mode of
    paCreate:
    begin
      if C.Player.Party = nil then
      begin
        C.Player.PartyChar := TMaplePartyCharacter.Create(C.Player);
        C.Player.Party := C.World.CreateParty(C.Player.PartyChar);
        C.Write(PartyCreated(C.Player.Party.ID));
      end;
    end;

    paLeave:
    begin
      if C.Player.Party = nil then
        Exit;

      if C.Player.Party.Leader = C.Player.PartyChar then
      begin
        C.World.UpdateParty(C.Player.Party.ID, poDisband, C.Player.PartyChar);
        C.Player.Party.Free;
      end
      else
        C.World.UpdateParty(C.Player.Party.ID, poLeave, C.Player.PartyChar);

      C.Player.Party := nil;
    end;

    paAccept, raAccepted:
    begin
      ID := Packet.ReadInt;

      if C.Player.Party <> nil then
        Exit;

      Party := C.World.GetParty(ID);
      if Party = nil then
        Exit;

      C.Player.PartyChar := TMaplePartyCharacter.Create(C.Player);

      if Party.Members.Count < 6 then
      begin
        C.World.UpdateParty(ID, poJoin, C.Player.PartyChar);
        C.Player.ReceivePartyMemberHP;
        C.Player.UpdatePartyMemberHP;
      end
      else
        C.Write(PartyStatusMessage(pmFull));
    end;

    paInvite:
    begin
      Name := Packet.ReadMapleAnsiString;
      Char := C.Channel.Player[Name];
      if Char = nil then
      begin
        C.Write(PartyStatusMessage(pmCharNotFound));
        Exit;
      end;

      if C.Player.PartyChar <> C.Player.Party.Leader then
        Exit;

      if Char.Party <> nil then
        C.Write(PartyStatusMessage(pmAlreadyJoined))
      else
        if C.Player.Party.Members.Count < 6 then
          TMapleClient(Char.Client).Write(PartyInvite(C.Player));
    end;

    paExpel:
    begin
      ID := Packet.ReadInt;
      Member := C.Player.Party.Member[ID];
      if (C.Player.PartyChar <> C.Player.Party.Leader) or (Member = nil) then
        Exit;

      C.World.UpdateParty(C.Player.Party.ID, poExpel, Member);
      // xxx handle Event Instances
    end;

    paChangeLeader:
    begin
      if C.Player.Party = nil then
        Exit;

      ID := Packet.ReadInt;
      Member := C.Player.Party.Member[ID];

      if (C.Player.PartyChar <> C.Player.Party.Leader) or (Member = nil) or (Member = C.Player.Party.Leader) then
        Exit;

      C.World.UpdateParty(C.Player.Party.ID, poChangeLeader, Member);
    end;

    raInvited: ;  // GMS sends that annoying "You have invited X to your party" message to the leader here

    raAlreadyInvited, raDenied:
    begin
      ID := Packet.ReadInt;
      Party := C.World.GetParty(ID);
      if (Party = nil) or (Party <> C.Player.Party) then
        Exit;

      Char := C.Channel.Player[Party.Leader.ID];
      if Char <> nil then
      begin
        case Mode of
          raAlreadyInvited: Name := 'You have already invited ''' + C.Player.Name + ''' to your party.';
          raDenied: Name := C.Player.Name + ' have denied request to the party.';
        end;
        TMapleClient(Char.Client).Write(PartyStringMessage(Name));
      end;
    end;

    else Log('Unknown party op: %d', [Byte(Mode)]);
  end;
end;

{ TDenyPartyRequestHandler }

class procedure TDenyPartyRequestHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Mode: Byte;
  From: TMapleCharacter;
begin
  Mode := Packet.ReadByte;    // $16 = Already taking care of an invitation; $17 = Deny
  From := C.Channel.Player[Packet.ReadMapleAnsiString];
  if From <> nil then
    TMapleClient(From.Client).Write(PartyStatusMessage(TPartyStatusMessage(Mode),
      C.Player.Name));
end;

initialization
  HandlerClasses.Add('ModifyBuddylist', TModifyBuddylistHandler);
  HandlerClasses.Add('PartyOperation', TPartyOperationHandler);
  HandlerClasses.Add('PartyRequestAnswer', TPartyOperationHandler);
  HandlerClasses.Add('DenyPartyRequest', TDenyPartyRequestHandler);

finalization

end.
