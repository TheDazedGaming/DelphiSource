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

unit ItemActionHandler;

interface

uses MapleStream, MapleClient, SysUtils, PacketProcessor, MaplePacketCreator,
     GameLogic, MapleMapObject, ChecksumCommand, Generics.Collections, MapleParty;

type
  TMoveItemHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TUseItemHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TTakeDropHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TUseReturnScrollHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TDropMesosHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

implementation

uses Main, MapleItem, DropHandler, PlayerInventory, MapleCharacter, MapleQuest;

{ TMoveItemHandler }

class procedure TMoveItemHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  InvType: TMapleInventoryType;
  Src, Dst, Quantity: SmallInt;
begin
  Packet.Skip(4);
  InvType := TMapleInventoryType(ShortInt(Packet.ReadByte));
  Src := Packet.ReadShort;
  Dst := Packet.ReadShort;
  Quantity := Packet.ReadShort;

  if (Src < 0) and (Dst > 0) then
    C.Player.Unequip(Src, Dst)
  else
  if Dst < 0 then
    C.Player.Equip(Src, Dst)
  else
  if Dst = 0 then
    C.Player.DropItem(InvType, Src, Quantity)
  else
    C.Player.MoveItem(InvType, Src, Dst);
end;

{ TUseItemHandler }

class procedure TUseItemHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Slot: ShortInt;
  ItemID: Integer;
  ToUse: TItem;
begin
  if not C.Player.IsAlive then
  begin
    C.Write(EnableActions);
    Exit;
  end;

  Packet.Skip(4);
  Slot := Packet.ReadShort;
  ItemID := Packet.ReadInt;

  ToUse := C.Player.Inventory[miUse][Slot];
  if (ToUse <> nil) or (ToUse.Quantity > 0) then
  begin
    if ToUse.ID <> ItemID then
    begin
      Log('[Hacking] Player %s is using an item not in the slot: %d', [C.Player.Name, ItemID]);
      Exit;    // should I return false here?
    end;
    C.Player.RemoveItemFromSlot(miUse, Slot, 1);

    C.Player.UseItem(ToUse.ID);
  end
  else
    Log('[Hacking] Player %s is using an item he does not have: %d', [C.Player.Name, ItemID]);
end;

{ TTakeDropHandler }

class procedure TTakeDropHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  OID: Integer;
  Obj: TMapleMapObject;
  Drop: TDrop;
  Player, P: TMapleCharacter;
  Member: TMaplePartyCharacter;
  Members: TList<TMapleCharacter>;
begin
  Packet.Skip(9);   // Mode (1), TickCount (4), Position (2x2)

  OID := Packet.ReadInt;
  Obj := C.Player.Map.MapObject[OID];

  if (Obj = nil) or (not (Obj is TDrop)) then
  begin
    C.Write(DontTake);
    Exit;
  end;

  Drop := TDrop(Obj);
  Player := C.Player;

  if Drop.IsQuestItem then
  begin
    if (not Player.HasStartedQuest(Drop.QuestID)) or
       (Player.Inventory[GetInventory(Drop.Data)].ListByID(Drop.Data).Count >=
        TPlayerQuestStatus(Player.Quests[Drop.QuestID]).Quest.ItemRequests[Drop.Data]) then
    begin
      C.Write(DropNotAvailableForPickup);
      C.Write(DontTake);
      Exit;
    end;
  end;

  if Assigned(ChecksumCommand.Items) and (not Drop.IsMesos) then
    if Items.ContainsKey(Drop.Item.ID) then
    begin
      PItemEntry(Items[Drop.Item.ID]).Checksum := Packet.ReadInt;
      C.Write(DropNotAvailableForPickup);
      C.Write(DontTake);
      Drop.Remove(True);
      Exit;
    end;

  if Drop.IsMesos then
  begin
    if Player.Party <> nil then
    begin
      Members := TList<TMapleCharacter>.Create;
      try
        for Member in Player.Party do
        begin
          P := TMapleCharacter(Player.Map.MapObject[Member.ID]);
          if Assigned(P) then
            Members.Add(P);
        end;

        for P in Members do
          P.ModifyMesos(Drop.Data div Members.Count, True, True);
      finally
        Members.Free;
      end;
    end
    else if Player.ModifyMesos(Drop.Data, False, True) then
      C.Write(ShowMesoGain(Drop.Data, False))
    else
    begin
      C.Write(DontTake);
      Exit;
    end;
  end
  else
  begin
    if Drop.Item = nil then
    begin
      C.Write(DontTake);
      Exit;
    end;

    // xxx AutoConsume

    if not Player.GainItemByID(Drop.Item.ID, Drop.Item.Quantity, '', True) then
    begin
      C.Write(CantGetAnymoreItems);
      C.Write(DontTake);
      Exit;
    end;
  end;

  Drop.Take(Player);
end;

{ TUseReturnScrollHandler }

class procedure TUseReturnScrollHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Slot, ItemID: Integer;
  ToUse: TItem;
begin
  if not C.Player.IsAlive then
  begin
    C.Write(EnableActions);
    Exit;
  end;

  Packet.Skip(4);
  Slot := Packet.ReadShort;
  ItemID := Packet.ReadInt;

  ToUse := C.Player.Inventory[miUse][Slot];
  if (ToUse = nil) or (ToUse.Quantity < 1) or (ToUse.ID <> ItemID) then
    Exit;

  if C.Player.UseItem(ItemID) then
    C.Player.RemoveItemFromSlot(miUse, Slot, 1)
  else
    C.Write(EnableActions);
end;

{ TDropMesosHandler }

class procedure TDropMesosHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Amount: Integer;
  Drop: TDrop;
begin
  Packet.Skip(4);  // ActionID o.o
  Amount := Packet.ReadInt;

  if (not C.Player.IsAlive) or (Amount < 10) or (Amount > 50000) or
     (Amount > C.Player.Mesos) then
  begin
    C.Write(EnableActions);
    Exit;
  end;

  C.Player.ModifyMesos(-Amount, False, True);

  Drop := TDrop.Create(C.Player.Map, Amount,
                       C.Player.Position,  // xxx calc
                       C.Player.ID, True);
  Drop.DoDrop(C.Player.Position);
end;

initialization
  HandlerClasses.Add('MoveItem', TMoveItemHandler);
  HandlerClasses.Add('UseItem', TUseItemHandler);
  HandlerClasses.Add('TakeDrop', TTakeDropHandler);
  HandlerClasses.Add('UseReturnScroll', TUseReturnScrollHandler);
  HandlerClasses.Add('DropMesos', TDropMesosHandler);

finalization

end.
