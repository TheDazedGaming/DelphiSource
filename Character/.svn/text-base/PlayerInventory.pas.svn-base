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

unit PlayerInventory;

interface

uses SysUtils, Generics.Collections, MapleCharacter, ItemDataProvider, GameLogic,
     Math, Types, DropHandler, MapleItem;

type
  TPlayerInventory = class helper for TMapleCharacter
  public
    function AddItem(Item: TItem; Reaction: Boolean): Boolean;
    function GainItemByID(ID, Quantity: Integer; Owner: string = ''; FromDrop: Boolean = False): Boolean;

    procedure DropItem(Inv: TMapleInventoryType; Src: ShortInt; Quantity: Word);
    procedure RemoveItemFromSlot(Inv: TMapleInventoryType; Slot: ShortInt; Quantity: Word; Reaction: Boolean = False; Consume: Boolean = False);
    procedure RemoveItemByID(ID, Quantity: Integer; Reaction, Consume: Boolean);

    procedure IncreaseSlotLimit(Inv: TMapleInventoryType; const RowsToAdd: Byte);

    procedure Equip(Src, Dst: SmallInt);
    procedure Unequip(Src, Dst: SmallInt);
    procedure MoveItem(Inv: TMapleInventoryType; Src, Dst: ShortInt);

    function UseItem(const ID: Integer): Boolean;
  end;

implementation

uses Main, MapleInventory,  MapleClient, MaplePacketCreator, MapleMap,
     MaplePortal, UseSkillHandler;

{ TPlayerInventory }

function TPlayerInventory.AddItem(Item: TItem; Reaction: Boolean): Boolean;
var
  Inv: TMapleInventory;
  SlotMax, OldQ, NewQ: SmallInt;
  Existing: TList<TItem>;
  I: TItem;
begin
  Inv := Inventory[GetInventory(Item.ID)];
  if Inv.InvType <> miEquip then
  begin
    SlotMax := ItemDataProv.GetSlotMax(Item.ID);

    if not IsRechargeable(Item.ID) then
    begin
      Existing := Inv.ListByID(Item.ID);

      if Existing.Count > 0 then
        for I in Existing do
        begin
          OldQ := I.Quantity;
          if (OldQ < SlotMax) and (I.Owner = Item.Owner) then
          begin
            NewQ := Min(OldQ + Item.Quantity, SlotMax);
            Item.Quantity := Item.Quantity - (NewQ - OldQ);
            I.Quantity := NewQ;

            TMapleClient(Client).Write(UpdateInventorySlot(Inv.InvType, I, Reaction));
          end;

          if I.Quantity <= 0 then
            Break;
        end;

      Existing.Free;
    end;

    // add a new slot if there is still something left
    if (Item.Quantity > 0) or (IsRechargeable(Item.ID)) then
    begin
      if Inv.Add(Item) = -1 then
      begin
        TMapleClient(Client).Write(InventoryFull);
        TMapleClient(Client).Write(ShowInventoryFull);
        Exit(False);
      end;

      TMapleClient(Client).Write(AddInventorySlot(Inv.InvType, Item, Reaction));
    end
    else
    begin
      // EnableActions needed here?
      Log('Check if you need EnableActions');
      Exit(False);
    end;
  end
  else     // Equip
  begin
    if Inv.Add(Item) = -1 then
    begin
      TMapleClient(Client).Write(InventoryFull);
      TMapleClient(Client).Write(ShowInventoryFull);
      Exit(False);
    end;

    TMapleClient(Client).Write(AddInventorySlot(Inv.InvType, Item, Reaction));
  end;

  Result := True;
end;

function TPlayerInventory.GainItemByID(ID, Quantity: Integer; Owner: string = '';
  FromDrop: Boolean = False): Boolean;
var
  IType: TMapleInventoryType;
  SlotMax: SmallInt;
  Item: TItem;
begin
  IType := GetInventory(ID);
  SlotMax := ItemDataProv.GetSlotMax(ID);

  if (Quantity <= 0) or (Quantity > SlotMax) then
  begin
    Log('[WTF] Quantity (%d) > SlotMax (%d) for %d', [Quantity, SlotMax, ID]);
    Exit(False);
  end;

  if IType <> miEquip then
    Item := MapleItem.TItem.Create(ID, 0, Quantity)
  else
    Item := ItemDataProv.LoadEquip(ID);
  Item.Owner := Owner;
  Result := AddItem(Item, FromDrop);

  if Result and FromDrop then
    TMapleClient(Client).Write(ShowItemGain(ID, Quantity, False));
end;

procedure TPlayerInventory.RemoveItemByID(ID, Quantity: Integer; Reaction,
  Consume: Boolean);
var
  Items: TList<TItem>;
  Item: TItem;
  Rem: Integer;
begin
  Items := Inventory[GetInventory(ID)].ListByID(ID);
  Rem := Quantity;
  for Item in Items do
    if Rem <= Item.Quantity then
    begin
      RemoveItemFromSlot(GetInventory(ID), Item.Position, Rem, Reaction, Consume);
      Rem := 0;
      Break;
    end
    else
    begin
      Dec(Rem, Item.Quantity);
      RemoveItemFromSlot(GetInventory(ID), Item.Position, Item.Quantity, Reaction, Consume);
    end;

  Items.Free;

  if Rem > 0 then
    raise Exception.Create('RemoveItemByID failed');
end;

procedure TPlayerInventory.RemoveItemFromSlot(Inv: TMapleInventoryType; Slot: ShortInt;
   Quantity: Word; Reaction: Boolean = False; Consume: Boolean = False);
var
  Item: TItem;
  AllowZero: Boolean;
begin
  Item := Inventory[Inv][Slot];
  AllowZero := Consume and IsRechargeable(Item.ID);
  Inventory[Inv].RemoveItem(Slot, Quantity, AllowZero);

  if (Item.Quantity = 0) and (not AllowZero) then
  begin
    TMapleClient(Client).Write(MaplePacketCreator.ClearInventorySlot(Inv, Item.Position, Reaction));
    Item.Free;
  end
  else
    TMapleClient(Client).Write(MaplePacketCreator.UpdateInventorySlot(Inv, Item, Reaction))
end;

procedure TPlayerInventory.IncreaseSlotLimit(Inv: TMapleInventoryType;
  const RowsToAdd: Byte);
begin
  Inventory[Inv].SlotLimit := Inventory[Inv].SlotLimit + RowsToAdd * 4;
  TMapleClient(Client).Write(MaplePacketCreator.UpdateInventorySlotLimit(Inventory[Inv]));
end;

function TPlayerInventory.UseItem(const ID: Integer): Boolean;
var
  CI: PConsumeInfo;
  Target: TMapleMap;
  MapDiv, CharDiv: Integer;
begin
  if not ItemDataProv.LoadConsumeInfo(ID, CI) then
    Exit(False);

  if CI^.HP > 0 then
    AddHP(CI^.HP, True);

  if CI^.MP > 0 then
    AddMP(CI^.MP, True);

  if CI^.HPRate > 0 then
    AddHP(Trunc(CI^.HPRate * (MaxHP / 100)), True);

  if CI^.MPRate > 0 then
    AddMP(Trunc(CI^.MPRate * (MaxMP / 100)), True);

  if (CI^.Time > 0) and (CI^.Chance = 0) then
    AddItemBuff(Self, ID, CI^.Time);  // call redirector in UseSkillHandler (can't use a class helper in a class helper)

  if CI^.MoveTo > 0 then
  begin
    if CI^.MoveTo = NO_MAP then  // nearest town
    begin
      if (Map.ReturnMap <> NO_MAP) and (Map.ID <> Map.ReturnMap) then
        Target := TMapleClient(Client).Channel.MapProvider.LoadMap(Map.ReturnMap)
      else
        Exit(False);
    end
    else
    begin
      Target := TMapleClient(Client).Channel.MapProvider.LoadMap(CI^.MoveTo);

      MapDiv := Target.ID div 10000000;
      CharDiv := Map.ID div 10000000;
      if not CI^.IgnoreContinent then
        if (MapDiv <> 60) and (CharDiv <> 61) then
          if (MapDiv <> 21) and (CharDiv <> 20) then
            if (MapDiv <> 12) and (CharDiv <> 10) then     // Nautilus -> Normal Vic
              if (MapDiv <> 10) and (CharDiv <> 12) then   // Normal Vic -> Nautilus
                if MapDiv <> CharDiv then
                  Exit(False);
    end;
    ChangeMap(Target);
  end
  else
    Exit(False);

  Result := True;
end;

procedure TPlayerInventory.DropItem(Inv: TMapleInventoryType; Src: ShortInt;
  Quantity: Word);
var
  Source, DropItem: TItem;
  Drop: TDrop;
begin
  if Src < 0 then
    Inv := miEquipped;

  Source := Inventory[Inv][Src];
  if (Quantity <= 0) or (Source = nil) and (not IsRechargeable(Source.ID)) then
  begin
    Log('[FATAL] %s dropping %d of %d->%d [Source = nil ? %s]', [Name, Quantity, Integer(Inv), Src, BoolToStr(Source = nil, True)]);
    TMapleClient(Self.Client).Disconnect;
    Exit;
  end;

  if (Quantity < Source.Quantity) and (not IsRechargeable(Source.ID)) then
  begin
    DropItem := Source.Copy;
    DropItem.Quantity := Quantity;
    Source.Quantity := Source.Quantity - Quantity;

    TMapleClient(Client).Write(DropInventoryItemUpdate(Inv, Source));
  end
  else
  begin
    Inventory[Inv].RemoveSlot(Src);

    if Src < 0 then
    begin
      TMapleClient(Client).Write(DropInventoryItem(miEquip, Src));
      EquipsChanged;
    end
    else
      TMapleClient(Client).Write(DropInventoryItem(Inv, Src));

    DropItem := Source;
  end;

  Drop := TDrop.Create(Map, DropItem, Position, ID, True);
  // xxx IsTradeable
  Drop.DoDrop(Position);
end;

procedure TPlayerInventory.Equip(Src, Dst: SmallInt);
var
  Source, Target, Top, Bottom, Weapon, Shield: TItem;
begin
  Source := Inventory[miEquip][Src];
  Target := Inventory[miEquipped][Dst];

  if Source = nil then
    Exit;

  // Unequip items that can't be equipped together with the new item.
  if Dst = Int8(esTop) then
  begin
    Bottom := Inventory[miEquipped][esBottom];
    if (Bottom <> nil) and (IsOverall(Source.ID)) then
    begin
      if Inventory[miEquip].IsFull then
      begin
        TMapleClient(Client).Write(InventoryFull);
        TMapleClient(Client).Write(ShowInventoryFull);
        Exit;
      end;

      Unequip(Int8(esBottom), Inventory[miEquip].NextFreeSlot);
    end;
  end
  else if Dst = Int8(esBottom) then
  begin
    Top := Inventory[miEquipped][esTop];
    if (Top <> nil) and (IsOverall(Top.ID)) then
    begin
      if Inventory[miEquip].IsFull then
      begin
        TMapleClient(Client).Write(InventoryFull);
        TMapleClient(Client).Write(ShowInventoryFull);
        Exit;
      end;

      Unequip(Int8(esTop), Inventory[miEquip].NextFreeSlot);
    end;
  end
  else if Dst = Int8(esShield) then     // check if weapon is two-handed
  begin
    if (not IsShield(Source.ID)) and (not IsKatara(Source.ID)) then
      Exit;

    Weapon := Inventory[miEquipped][esWeapon];
    if (Weapon <> nil) and (IsTwoHandedWeapon(Weapon.ID)) then
    begin
      if Inventory[miEquip].IsFull then
      begin
        TMapleClient(Client).Write(InventoryFull);
        TMapleClient(Client).Write(ShowInventoryFull);
        Exit;
      end;

      Unequip(Int8(esWeapon), Inventory[miEquip].NextFreeSlot);
    end;
  end
  else if Dst = Int8(esWeapon) then
  begin
    if (not IsWeapon(Source.ID)) or (IsKatara(Source.ID)) then
      Exit;

    Shield := Inventory[miEquipped][esShield];
    if (Shield <> nil) and (IsTwoHandedWeapon(Source.ID)) then
    begin
      if Inventory[miEquip].IsFull then
      begin
        TMapleClient(Client).Write(InventoryFull);
        TMapleClient(Client).Write(ShowInventoryFull);
        Exit;
      end;

      Unequip(Int8(esShield), Inventory[miEquip].NextFreeSlot);
    end;
  end;

  Inventory[miEquip].RemoveSlot(Src);
  if Target <> nil then
    Inventory[miEquipped].RemoveSlot(Dst);

  Source.Position := Dst;
  Inventory[miEquipped].AddFromDB(Source);
  if Target <> nil then
  begin
    Target.Position := Src;
    Inventory[miEquip].AddFromDB(Target);
  end;

  // xxx cancel booster

  TMapleClient(Client).Write(MoveInventoryItem(miEquip, Src, Dst, 2));
  EquipsChanged;
end;

procedure TPlayerInventory.Unequip(Src, Dst: SmallInt);
var
  Source, Target: TItem;
begin
  Source := Inventory[miEquipped][Src];
  Target := Inventory[miEquip][Dst];

  if Source = nil then
    Exit;

  if (Target <> nil) and (Src <= 0) then
  begin
    // do not allow switching with equip
    TMapleClient(Client).Write(InventoryFull);
    Exit;
  end;

  Inventory[miEquipped].RemoveSlot(Src);
  if Target <> nil then
    Inventory[miEquip].RemoveSlot(Dst);
  Source.Position := Dst;
  Inventory[miEquip].AddFromDB(Source);

  if Target <> nil then
  begin
    Target.Position := Src;
    Inventory[miEquipped].AddFromDB(Target);
  end;

  TMapleClient(Client).Write(MoveInventoryItem(miEquip, Src, Dst, 1));
  EquipsChanged;
end;

procedure TPlayerInventory.MoveItem(Inv: TMapleInventoryType; Src,
  Dst: ShortInt);
var
  Source, Target: TItem;
  OldSrcQ, OldDstQ, SlotMax: SmallInt;
begin
  if (Src < 0) or (Dst < 0) then
    Exit;

  Source := Inventory[Inv][Src];
  Target := Inventory[Inv][Dst];
  if Source = nil then
    Exit;

  OldDstQ := -1;
  if Target <> nil then
    OldDstQ := Target.Quantity;
  OldSrcQ := Source.Quantity;
  SlotMax := ItemDataProv.GetSlotMax(Source.ID);

  Inventory[Inv].Move(Src, Dst, SlotMax);

  if (Inv <> miEquip) and (Target <> nil) and (Target.ID = Source.ID) and
    (not IsRechargeable(Source.ID)) then
  begin
    if OldDstQ + OldSrcQ > SlotMax then
      TMapleClient(Client).Write(MoveAndMergeWithRestInventoryItem(
        Inv, Src, Dst, (OldDstQ + OldSrcQ) - SlotMax, SlotMax))
    else
      TMapleClient(Client).Write(MoveAndMergeInventoryItem(
        Inv, Src, Dst, Inventory[Inv][Dst].Quantity));
  end
  else
    TMapleClient(Client).Write(MoveInventoryItem(Inv, Src, Dst));
end;

end.
