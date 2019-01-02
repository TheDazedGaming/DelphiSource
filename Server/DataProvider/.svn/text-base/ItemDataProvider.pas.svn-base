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

unit ItemDataProvider;

interface

uses SysUtils, MapleItem, ZDataset, Generics.Collections;

type
  TItemInfo = record
    CashItem: Boolean;
    Price: Integer;
    SlotMax: Word;
  end;
  PItemInfo = ^TItemInfo;

  TItemDataProvider = class
  private
    FEquipCache: TDictionary<Integer, PEquipInfo>;
    FConsumeCache: TDictionary<Integer, PConsumeInfo>;
    FItems: TDictionary<Integer, PItemInfo>;

    function LoadItem(const ID: Integer): PItemInfo;
  public
    constructor Create;
    destructor Destroy; override;

    function IsCashItem(const ID: Integer): Boolean;
    function IsItem(const ID: Integer): Boolean;

    function GetPrice(const ID: Integer): Integer;
    function GetSlotMax(const ID: Integer): SmallInt;

    function LoadEquip(const ID: Integer): TEquip;
    function LoadConsumeInfo(const ID: Integer; var Info: PConsumeInfo): Boolean;
  end;

var
  ItemDataProv: TItemDataProvider;

implementation

uses MapleServerHandler, BuffDataProvider, Main;

{ TItemDataProvider }

constructor TItemDataProvider.Create;
begin
  FEquipCache := TDictionary<Integer, PEquipInfo>.Create;
  FConsumeCache := TDictionary<Integer, PConsumeInfo>.Create;
  FItems := TDictionary<Integer, PItemInfo>.Create;
end;

destructor TItemDataProvider.Destroy;
var
  Eq: PEquipInfo;
  PCI: PConsumeInfo;
  PII: PItemInfo;
begin
  for Eq in FEquipCache.Values do
    Dispose(Eq);
  FreeAndNil(FEquipCache);

  for PCI in FConsumeCache.Values do
    Dispose(PCI);
  FreeAndNil(FConsumeCache);

  for PII in FItems.Values do
    Dispose(PII);
  FreeAndNil(FItems);

  inherited;
end;

function TItemDataProvider.IsCashItem(const ID: Integer): Boolean;
var
  Item: PItemInfo;
begin
  if ID div 1000000 = 5 then
    Exit(True);

  Item := LoadItem(ID);
  if Item <> nil then
    Result := Item^.CashItem
  else
    Result := False;
end;

function TItemDataProvider.GetPrice(const ID: Integer): Integer;
var
  Item: PItemInfo;
begin
  Item := LoadItem(ID);
  if Item <> nil then
    Result := Item^.Price
  else
    Result := -1;
end;

function TItemDataProvider.GetSlotMax(const ID: Integer): SmallInt;
var
  Item: PItemInfo;
begin
  Item := LoadItem(ID);
  if Item <> nil then
    Result := Item^.SlotMax
  else
    Result := -1;
end;

function TItemDataProvider.IsItem(const ID: Integer): Boolean;
begin
  if FItems.ContainsKey(ID) then
    Exit(True);

  with MCDB.GetQuery do
  begin
    SQL.Text := 'SELECT price FROM item_data WHERE itemid = ' + IntToStr(ID);
    Open;

    Result := not EOF;

    Close;
    Free;
  end;
end;

function TItemDataProvider.LoadItem(const ID: Integer): PItemInfo;
begin
  if FItems.ContainsKey(ID) then
    Exit(FItems[ID]);

  with MCDB.GetQuery do
  begin
    SQL.Text := 'SELECT flags, max_slot_quantity, price FROM item_data WHERE itemid = ' + IntToStr(ID);
    Open;

    try
      if EOF then
        Exit(nil);

      New(Result);
      Result^.CashItem := Pos('cash_item', FieldByName('flags').AsString) > 0;
      Result^.Price := FieldByName('price').AsInteger;
      Result^.SlotMax := FieldByName('max_slot_quantity').AsInteger;

      FItems.Add(ID, Result);
    finally
      Close;
      Free;
    end;
  end;
end;

function TItemDataProvider.LoadEquip(const ID: Integer): TEquip;
var
  Q: TZQuery;
  Stats: PEquipInfo;
begin
  if FEquipCache.ContainsKey(ID) then
  begin
    Result := TEquip.Create(ID, 0);
    Result.AssignStats(FEquipCache[ID]);
    Exit;
  end;

  Result := nil;

  Q := MCDB.GetQuery;
  Q.SQL.Text := 'SELECT * FROM item_equip_data WHERE itemid = ' + IntToStr(ID);
  Q.Open;
  try
    if not Q.EOF then
    begin
      New(Stats);
      with Stats^, Q do
      begin
        STR := FieldByName('strength').AsInteger;
        DEX := FieldByName('dexterity').AsInteger;
        INT := FieldByName('intelligence').AsInteger;
        LUK := FieldByName('luck').AsInteger;
        HP := FieldByName('hp').AsInteger;
        MP := FieldByName('mp').AsInteger;
        WAtk := FieldByName('weapon_attack').AsInteger;
        MAtk := FieldByName('magic_attack').AsInteger;
        WDef := FieldByName('weapon_defense').AsInteger;
        MDef := FieldByName('magic_defense').AsInteger;
        Acc := FieldByName('accuracy').AsInteger;
        Avoid := FieldByName('avoid').AsInteger;
        Hands := FieldByName('hands').AsInteger;
        Speed := FieldByName('speed').AsInteger;
        Jump := FieldByName('jump').AsInteger;
        UpgradeSlots := FieldByName('scroll_slots').AsInteger;
      end;

      Result := TEquip.Create(ID, 0);
      Result.AssignStats(Stats);
      FEquipCache.Add(ID, Stats);
    end
    else
      raise Exception.Create('Tried to load a non-existing equipment: ' + IntToStr(ID));
  finally
    Q.Close;
    Q.Free;
  end;
end;

function TItemDataProvider.LoadConsumeInfo(const ID: Integer; var Info: PConsumeInfo): Boolean;
var
  Q: TZQuery;
begin
  if FConsumeCache.ContainsKey(ID) then
  begin
    Info := FConsumeCache[ID];
    Exit(True);
  end;

  Q := MCDB.GetQuery;
  Q.SQL.Text := 'SELECT * FROM item_consume_data WHERE itemid = ' + IntToStr(ID);
  Q.Open;

  Result := not Q.EOF;

  if Result then
  begin
    New(Info);
    with Info^, Q do
    begin
      AutoConsume := Pos('auto_consume', FieldByName('flags').AsString) > 0;
      HP := FieldByName('hp').AsInteger;
      MP := FieldByName('mp').AsInteger;
      HPRate := FieldByName('hp_percentage').AsInteger;
      MPRate := FieldByName('mp_percentage').AsInteger;
      Time := FieldByName('buff_time').AsInteger;
      Chance := FieldByName('prob').AsInteger;
      WAtk := FieldByName('weapon_attack').AsInteger;
      MAtk := FieldByName('magic_attack').AsInteger;
      WDef := FieldByName('weapon_defense').AsInteger;
      MDef := FieldByName('magic_defense').AsInteger;
      Acc := FieldByName('accuracy').AsInteger;
      Avoid := FieldByName('avoid').AsInteger;
      Speed := FieldByName('speed').AsInteger;
      Jump := FieldByName('jump').AsInteger;
      // xxx

      // Return scrolls
      MoveTo := FieldByName('move_to').AsInteger;
      IgnoreContinent := Pos('ignore_continent', FieldByName('flags').AsString) > 0;
    end;
    BuffDataProv.AddItemInfo(ID, Info);
  end;

  Q.Close;
  FreeAndNil(Q);

  FConsumeCache.Add(ID, Info);
end;

end.
