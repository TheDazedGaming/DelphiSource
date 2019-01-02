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

unit MapleInventory;

interface

uses Classes, SysUtils, Generics.Collections, Generics.Defaults, GameLogic,
     MapleItem, ItemDataProvider;

type
  TInventoryEnumerator = TDictionary<ShortInt, TItem>.TValueEnumerator;

  TMapleInventory = class
  private
    FInventory: TDictionary<ShortInt, TItem>;
    FSlotLimit: Byte;
    FType: TMapleInventoryType;

    function Get(Slot: ShortInt): TItem;
    function GetE(Slot: TEquipSlot): TItem;
    procedure Swap(Source, Target: TItem);
  public
    constructor Create(InvType: TMapleInventoryType; SlotLimit: Byte);
    destructor Destroy; override;

    function Add(Item: TItem): ShortInt;
    procedure AddFromDB(Item: TItem);
    function ContainsID(ID: Integer): Boolean;

    function GetEnumerator: TInventoryEnumerator;

    function HasOpenSlotsFor(const ItemID: Integer; Amount: Word; CanStack: Boolean): Boolean;

    function ListByID(const ItemID: Integer): TList<TItem>;

    function IsFull(Margin: Byte = 0): Boolean;
    function NextFreeSlot: ShortInt;

    procedure Move(Src, Dst: ShortInt; SlotMax: SmallInt);

    procedure RemoveItem(Slot: ShortInt); overload;
    procedure RemoveItem(Slot: ShortInt; Quantity: Word; AllowZero: Boolean); overload;
    procedure RemoveSlot(const Slot: ShortInt);

    property Items[Slot: ShortInt]: TItem read Get; default;  // This is very nice :D
    property Items[Slot: TEquipSlot]: TItem read GetE; default;
    property InvType: TMapleInventoryType read FType;
    property SlotLimit: Byte read FSlotLimit write FSlotLimit;
  end;

implementation

uses Main, MapleClient, MaplePacketCreator;

{ TMapleInventory }

constructor TMapleInventory.Create(InvType: TMapleInventoryType;
  SlotLimit: Byte);
begin
  FType := InvType;
  FSlotLimit := SlotLimit;
  FInventory := TDictionary<ShortInt, TItem>.Create;
end;

destructor TMapleInventory.Destroy;
var
  Item: TItem;
begin
  for Item in FInventory.Values do
    Item.Free;

  FreeAndNil(FInventory);

  inherited;
end;

function TMapleInventory.Add(Item: TItem): ShortInt;
begin
  Result := NextFreeSlot;
  if Result = -1 then
    Exit;

  FInventory.Add(Result, Item);
  Item.Position := Result;
end;

procedure TMapleInventory.AddFromDB(Item: TItem);
begin
  if (Item.Position < 0) and (FType <> miEquipped) then
  begin
    Log('Item with negative position in non-equipped Inv WTF?');
    Exit;
  end;

  FInventory.Add(Item.Position, Item);
end;

function TMapleInventory.ContainsID(ID: Integer): Boolean;
var
  Item: TItem;
begin
  for Item in Self do
    if Item.ID = ID then
      Exit(True);

  Result := False;
end;

function TMapleInventory.Get(Slot: ShortInt): TItem;
begin
  if not FInventory.TryGetValue(Slot, Result) then
    Result := nil;
end;

function TMapleInventory.GetE(Slot: TEquipSlot): TItem;
begin
  if not FInventory.TryGetValue(ShortInt(Slot), Result) then
    Result := nil;
end;

function TMapleInventory.GetEnumerator: TInventoryEnumerator;
begin
  // FUCKING ERROR INSIGHT!!!!
  Result := FInventory.Values.GetEnumerator;
end;

function TMapleInventory.HasOpenSlotsFor(const ItemID: Integer; Amount: Word;
  CanStack: Boolean): Boolean;
var
  Required, MaxSlot, Existing: Word;
begin
  Required := 0;

  if (FType = miEquip) or (IsRechargeable(ItemID)) then
    Required := Amount   // These aren't stackable
  else
  begin
    MaxSlot := ItemDataProv.GetSlotMax(ItemID);
    with ListByID(ItemID) do
    begin
      Existing := Count mod MaxSlot;
      Free;
    end;

    if CanStack and (Existing > 0) then
    begin
      Inc(Existing, Amount);
      if Existing > MaxSlot then
      begin
        Required := Existing div MaxSlot;
        if Existing mod MaxSlot > 0 then
          Inc(Required);
      end;
    end
    else
    begin
      Required := Amount div MaxSlot;
      if Amount mod MaxSlot > 0 then
        Inc(Required);
    end;
  end;

  Result := SlotLimit - FInventory.Count >= Required;
end;

function TMapleInventory.IsFull(Margin: Byte = 0): Boolean;
begin
  Result := FInventory.Count + Margin >= FSlotLimit;
end;

function TMapleInventory.ListByID(const ItemID: Integer): TList<TItem>;
var
  Item: TItem;
begin
  Result := TList<TItem>.Create(TComparer<TItem>.Construct(
    function(const Left, Right: TItem): Integer
    begin
      if Left.Position < Right.Position then
        Result := -1
      else
      if Left.Position > Right.Position then
        Result := 1
      else
        Result := 0;
    end));

  for Item in Self do
    if Item.ID = ItemID then
      Result.Add(Item);

  Result.Sort;   // Sort using Position
end;

function TMapleInventory.NextFreeSlot: ShortInt;
var
  i: Integer;
begin
  Result := -1;

  if IsFull then
    Exit;

  for i := 1 to SlotLimit do
    if not FInventory.ContainsKey(i) then
      Exit(i);
end;

procedure TMapleInventory.Move(Src, Dst: ShortInt; SlotMax: SmallInt);
var
  Source, Target: TItem;
  Rest: SmallInt;
begin
  Source := Get(Src);
  Target := Get(Dst);

  if Source = nil then
    raise EArgumentException.Create('Trying to move empty slot!');

  if Target = nil then
  begin
    Source.Position := Dst;
    FInventory.Add(Dst, Source);
    FInventory.Remove(Src);
  end
  else
  if (Target.ID = Source.ID) and (not IsRechargeable(Source.ID)) then
  begin
    if FType = miEquip then
      Swap(Target, Source);

    if Source.Quantity + Target.Quantity > SlotMax then
    begin
      Rest := (Source.Quantity + Target.Quantity) - SlotMax;
      Source.Quantity := Rest;
      Target.Quantity := SlotMax;
    end
    else
    begin
      Target.Quantity := Source.Quantity + Target.Quantity;
      FInventory.Remove(Src);
    end;
  end
  else
    Swap(Target, Source);
end;

procedure TMapleInventory.RemoveItem(Slot: ShortInt);
begin
  RemoveItem(Slot, 1, False);
end;

procedure TMapleInventory.RemoveItem(Slot: ShortInt; Quantity: Word;
  AllowZero: Boolean);
var
  Item: TItem;
begin
  if not FInventory.TryGetValue(Slot, Item) then
    Exit;

  if Item.Quantity - Quantity < 0 then
    Item.Quantity := 0
  else
    Item.Quantity := Item.Quantity - Quantity;

  if (Item.Quantity = 0) and (not AllowZero) then
    RemoveSlot(Slot);
end;

procedure TMapleInventory.RemoveSlot(const Slot: ShortInt);
begin
  if not FInventory.ContainsKey(Slot) then
    Exit;

  FInventory.Remove(Slot);
end;

procedure TMapleInventory.Swap(Source, Target: TItem);
var
  SwapPos: ShortInt;
begin
  FInventory.Remove(Source.Position);
  FInventory.Remove(Target.Position);
  SwapPos := Source.Position;
  Source.Position := Target.Position;
  Target.Position := SwapPos;
  FInventory.Add(Source.Position, Source);
  FInventory.Add(Target.Position, Target);
end;

end.
