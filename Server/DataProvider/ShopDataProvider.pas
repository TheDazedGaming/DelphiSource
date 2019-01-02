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

unit ShopDataProvider;

interface

uses SysUtils, Generics.Collections, ZDataset;

type
  TShopItemInfo = record
    ItemID: Integer;
    Price: Integer;
    Quantity: Word;
  end;
  PShopItemInfo = ^TShopItemInfo;

  TShopInfo = record
    NPC: Integer;
    Items: TList<PShopItemInfo>;
    RechargeTier: Byte;
  end;
  PShopInfo = ^TShopInfo;

  TShopDataProvider = class
  private
    FShops: TDictionary<Integer, PShopInfo>;
    FRechargeCosts: TDictionary<Byte, TDictionary<Integer, Double>>;

    procedure LoadRechargeTiers;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadShop(const ShopID: Integer): PShopInfo;

    function GetAmount(const ShopID: Integer; Index: Word): Word;
    function GetItemID(const ShopID: Integer; Index: Word): Integer;
    function GetPrice(const ShopID: Integer; Index: Word): Integer;
    function GetRechargeCost(const ShopID, ItemID: Integer; Amount: Word): Integer;

    property RechargeCosts: TDictionary<Byte, TDictionary<Integer, Double>> read FRechargeCosts;
  end;

var
  ShopDataProv: TShopDataProvider;   // Singleton

implementation

uses Main, MapleServerHandler;

{ TShopDataProvider }

constructor TShopDataProvider.Create;
begin
  FShops := TDictionary<Integer, PShopInfo>.Create;
  FRechargeCosts := TDictionary<Byte, TDictionary<Integer, Double>>.Create;

  LoadRechargeTiers;
end;

destructor TShopDataProvider.Destroy;
var
  Info: PShopInfo;
  Item: PShopItemInfo;
  Cost: TDictionary<Integer, Double>;
begin
  for Info in FShops.Values do
  begin
    // Free all items first
    for Item in Info^.Items do
      Dispose(Item);

    Info^.Items.Free;

    Dispose(Info);
  end;
  FShops.Free;

  for Cost in FRechargeCosts.Values do
    Cost.Free;
  FRechargeCosts.Free;

  inherited;
end;

procedure TShopDataProvider.LoadRechargeTiers;
var
  Tier: Byte;
begin
  with MCDB.GetQuery do
  begin
    SQL.Text := 'SELECT * FROM shop_recharge_data';
    Open;

    try
      while not EOF do
      begin
        Tier := FieldByName('tierid').AsInteger;
        if not FRechargeCosts.ContainsKey(Tier) then
          FRechargeCosts.Add(Tier, TDictionary<Integer, Double>.Create);

        FRechargeCosts[Tier].AddOrSetValue(FieldByName('itemid').AsInteger,
                                           FieldByName('price').AsFloat);

        Next;
      end;
    finally
      Close;
      Free;
    end;
  end;
end;

function TShopDataProvider.LoadShop(const ShopID: Integer): PShopInfo;
var
  Item: PShopItemInfo;
begin
  if FShops.ContainsKey(ShopID) then
    Exit(FShops[ShopID]);

  with MCDB.GetQuery do
  begin
    SQL.Text := 'SELECT * FROM shop_data WHERE shopid = ' + IntToStr(ShopID);
    Open;

    try
      if EOF then
        Exit(nil);

      New(Result);
      Result^.NPC := FieldByName('npcid').AsInteger;
      Result^.RechargeTier := FieldByName('recharge_tier').AsInteger;
      Result^.Items := TList<PShopItemInfo>.Create;

      FShops.Add(ShopID, Result);
    finally
      Close;
      Free;
    end;
  end;

  with MCDB.GetQuery do
  begin
    SQL.Text := Format('SELECT * FROM shop_items WHERE shopid = %d ORDER BY sort desc', [ShopID]);
    Open;

    try
      if EOF then
      begin
        Log('[Shops] Strange thing, there''s a shop, but no items for %d', [ShopID]);
        Exit;
      end;

      while not EOF do
      begin
        New(Item);
        Item^.ItemID := FieldByName('itemid').AsInteger;
        Item^.Quantity := FieldByName('quantity').AsInteger;
        Item^.Price := FieldByName('price').AsInteger;

        PShopInfo(FShops[ShopID])^.Items.Add(Item);

        Next;
      end;
    finally
      Close;
      Free;
    end;
  end;
end;

function TShopDataProvider.GetAmount(const ShopID: Integer; Index: Word): Word;
begin
  if Index < PShopInfo(FShops[ShopID])^.Items.Count then
    Result := PShopItemInfo(PShopInfo(FShops[ShopID])^.Items[Index])^.Quantity
  else
    Result := 0;
end;

function TShopDataProvider.GetItemID(const ShopID: Integer;
  Index: Word): Integer;
begin
  if Index < PShopInfo(FShops[ShopID])^.Items.Count then
    Result := PShopItemInfo(PShopInfo(FShops[ShopID])^.Items[Index])^.ItemID
  else
    Result := 0;
end;

function TShopDataProvider.GetPrice(const ShopID: Integer;
  Index: Word): Integer;
begin
  if Index < PShopInfo(FShops[ShopID])^.Items.Count then
    Result := PShopItemInfo(PShopInfo(FShops[ShopID])^.Items[Index])^.Price
  else
    Result := 0;
end;

function TShopDataProvider.GetRechargeCost(const ShopID, ItemID: Integer;
  Amount: Word): Integer;
var
  Tier: Byte;
begin
  Result := 1;
  if FShops.ContainsKey(ShopID) then
  begin
    Tier := PShopInfo(FShops[ShopID])^.RechargeTier;
    if FRechargeCosts.ContainsKey(Tier) then
      if FRechargeCosts[Tier].ContainsKey(ItemID) then
        Result := Trunc(-1 * FRechargeCosts[Tier][ItemID] * Amount);
  end;
end;

end.
