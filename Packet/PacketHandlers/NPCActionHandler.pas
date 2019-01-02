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

unit NPCActionHandler;

interface

uses SysUtils, PacketProcessor, MapleClient, MapleStream, MaplePacketCreator,
     MapleNPC, MapleMapObject, ShopDataProvider, ItemDataProvider,
     GameLogic, PlayerInventory, MapleItem, ScriptHelper, NPCConversation;

type
  TNPCTalkHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TNPCTalkMoreHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TNPCAnimationHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TShopAction = (saBuy, saSell, saRecharge, saClose);
  TNPCShopActionHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

implementation

uses Main, MapleCharacter, MapleServerHandler;

{ TNPCTalkHandler }

class procedure TNPCTalkHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  ObjID: Integer;
  Obj: TMapleMapObject;
  NPC: TMapleNPC;
  Shop: PShopInfo;
  FN: string;
begin
  if C.Player.CurConversation <> nil then
    Exit;

  ObjID := Packet.ReadInt;
  Packet.Skip(4);   // unknown

  Obj := C.Player.Map.MapObject[ObjID];

  if (Obj <> nil) and (Obj is TMapleNPC) then
  begin
    NPC := TMapleNPC(Obj);
    Log('NPC talk: ' + IntToStr(NPC.ID) + '; object: ' + IntToStr(ObjID));

    if C.Player.CurConversation <> nil then
      Exit;

    if not FindScript(NPC.ID, 'NPC', FN) then
    begin
      Shop := ShopDataProv.LoadShop(NPC.ID);
      if Shop <> nil then
      begin
        C.Player.Shop := NPC.ID;
        C.Write(ShowShop(NPC.ID, Shop));
        Exit;
      end;

      C.Write(NPC.GetTalkPacket(NPC.ID, 0, 'No such script: ' + ExtractFileName(FN), [0, 0]));
      Exit;
    end;

    C.Player.CurConversation := TNPCConversation.Create(NPC.ID, C, FN);
    C.Player.CurConversation.Start;
  end;
end;

{ TNPCTalkMoreHandler }

class procedure TNPCTalkMoreHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  LastMsg, Action, Selection: ShortInt;
begin
  LastMsg := Packet.ReadByte;
  {$IFDEF VERSION89_UP}
  // in v84+ all MsgTypes after 0 were increased by one
  if LastMsg > 1 then
    Dec(LastMsg);
  {$ENDIF}
  Action := ShortInt(Packet.ReadByte);

  if (Action = -1) or ((LastMsg in [4, 14]) and (Action = 0)) then
  begin
    if Assigned(C.Player.CurConversation) then
    begin
      C.Player.CurConversation.Free;
      C.Player.CurConversation := nil;
    end;
  end
  else
  begin
    Selection := -1;

    if Packet.Size - Packet.Position > 0 then
      Selection := ShortInt(Packet.ReadByte);

    if Assigned(C.Player.CurConversation) then
      C.Player.CurConversation.Action(Action, LastMsg, Selection);
  end;
end;

{ TNPCAnimationHandler }

class procedure TNPCAnimationHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Response: TMapleStream;
  Length: Integer;
  Bytes: TBytes;
begin
  Length := Packet.Size;

  if (C.Player.MapID < 910000000) or
     (C.Player.MapID > 910000022) then
  begin
    Response := TMapleStream.Create;

    if Length = 6 then       // NPC Talk
    begin
      Response.WriteShort(OpHandler.SendOps['NPCAnimation']);
      Response.WriteInt(Packet.ReadInt);
      Response.WriteShort(Packet.ReadShort);

      C.Write(Response);
      Exit;
    end
    else
    if Length > 6 then       // NPC Move
    begin
      SetLength(Bytes, Length - 9);
      Packet.Read(Bytes[0], Length - 9);

      Response.WriteShort(OpHandler.SendOps['NPCAnimation']);
      Response.Write(Bytes[0], Length - 9);
      Bytes := nil;

      C.Write(Response);
      Exit;
    end;

    FreeAndNil(Response);
  end;
end;

{ TNPCShopActionHandler }

class procedure TNPCShopActionHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Player: TMapleCharacter;
  Action: TShopAction;
  ItemIndex, Quantity, Amount, SlotMax: Word;
  ItemID, Price, TotalPrice: Integer;
  TotalAmount: Cardinal;
  HasSlot: Boolean;
  Inv: TMapleInventoryType;
  Item: TItem;
begin
  Player := C.Player;

  if Player.Shop = -1 then
    Exit;  // hacking

  Action := TShopAction(Packet.ReadByte);
  case Action of
    saBuy:
    begin
      ItemIndex := Packet.ReadShort;
      Packet.Skip(4);   // ItemID, can't trust it
      Quantity := Packet.ReadShort;
      Packet.Skip(4);   // Price

      Amount := ShopDataProv.GetAmount(Player.Shop, ItemIndex);
      ItemID := ShopDataProv.GetItemID(Player.Shop, ItemIndex);
      Price := ShopDataProv.GetPrice(Player.Shop, ItemIndex);
      TotalAmount := Amount * Quantity;
      TotalPrice := Quantity * Price;

      if (Price = 0) or (TotalAmount > Cardinal(ItemDataProv.GetSlotMax(ItemID))) or
         (Player.Mesos < TotalPrice) then
        Exit;  // hacking

      HasSlot := Player.Inventory[GetInventory(ItemID)].HasOpenSlotsFor(ItemID, TotalAmount, True);
      if HasSlot then
      begin
        Player.GainItemByID(ItemID, TotalAmount);
        Player.ModifyMesos(-TotalPrice, False);
        C.Write(ItemBought(0));
      end
      else
        C.Write(ItemBought(3));
    end;

    saSell:
    begin
      ItemIndex := Packet.ReadShort;
      ItemID := Packet.ReadInt;
      Amount := Packet.ReadShort;
      Inv := GetInventory(ItemID);
      Item := Player.Inventory[Inv][ItemIndex];

      if (Item = nil) or ((not IsRechargeable(ItemID)) and (Amount > Item.Quantity)) then
      begin
        C.Write(ItemBought(1));
        Exit;   // hacking
      end;

      Price := ItemDataProv.GetPrice(ItemID);
      Player.ModifyMesos(Price * Amount, False);

      if IsRechargeable(ItemID) then
        Player.RemoveItemFromSlot(Inv, ItemIndex, Item.Quantity)
      else
        Player.RemoveItemFromSlot(Inv, ItemIndex, Amount);

      C.Write(ItemBought(0));
    end;

    saRecharge:
    begin
      ItemIndex := Packet.ReadShort;
      Item := Player.Inventory[miUse][ItemIndex];

      if (Item = nil) or (not IsRechargeable(Item.ID)) then
        Exit;   // hacking

      SlotMax := ItemDataProv.GetSlotMax(Item.ID);
      // xxx add rechargeable bonus

      TotalPrice := ShopDataProv.GetRechargeCost(Player.Shop, Item.ID, SlotMax);
      if (TotalPrice < 0) and (Player.Mesos >= -TotalPrice) then
      begin
        Player.ModifyMesos(TotalPrice, False);
        Item.Quantity := SlotMax;
        C.Write(UpdateInventorySlot(miUse, Item));
        C.Write(ItemBought(0));
      end;
    end;

    saClose: Player.Shop := -1;
  end;
end;

initialization
  HandlerClasses.Add('NPCTalk', TNPCTalkHandler);
  HandlerClasses.Add('NPCTalkMore', TNPCTalkMoreHandler);
  HandlerClasses.Add('NPCAnimation', TNPCAnimationHandler);
  HandlerClasses.Add('NPCShopAction', TNPCShopActionHandler);

finalization

end.
