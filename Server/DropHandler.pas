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

unit DropHandler;

interface

uses Windows, Types, DropDataProvider, GameLogic, MapleItem, MapleMapObject,
     MapleCharacter, MapleMap, MTRand, ItemDataProvider;

type
  TDropHandler = class
  public
    class procedure DoDrops(Player: TMapleCharacter; Map: TMapleMap; DropperID,
      DropperOID: Integer; Origin: TPoint; FreeForAll: Boolean = False);
  end;

  TDropType = (dtNormal, dtParty, dtFreeForAll, dtExplosive);
  TDrop = class(TMapleMapObject)
  private
    FType: TDropType;
    FMesos, FOwner, FDropperOID: Integer;
    FDropped: Cardinal;
    FTradeable, FPlayerDrop: Boolean;
    FMap: TMapleMap;
    FItem: TItem;

    // These are used for quests only
    FPlayer: TMapleCharacter;
    FQuestID: Word;

    procedure Init(Map: TMapleMap; Pos: TPoint; Owner: Integer; PlayerDrop: Boolean);

    function GetData: Integer;
  public
    constructor Create(Map: TMapleMap; Mesos: Integer; Pos: TPoint; Owner: Integer; PlayerDrop: Boolean = False); overload;
    constructor Create(Map: TMapleMap; Item: TItem; Pos: TPoint; Owner: Integer; PlayerDrop: Boolean = False); overload;

    function GetType: TObjectType; override;
    function SendSpawnDataTo(Client: TObject): Boolean; override;
    procedure SendDestroyDataTo(Client: TObject); override;

    procedure DoDrop(Origin: TPoint);
    procedure Remove(SendPacket: Boolean);
    procedure Take(Player: TMapleCharacter);

    function IsMesos: Boolean;
    function IsQuestItem: Boolean;

    property Data: Integer read GetData;
    property DropperOID: Integer read FDropperOID write FDropperOID;
    property Dropped: Cardinal read FDropped;
    property DType: TDropType read FType write FType;
    property Item: TItem read FItem;
    property Owner: Integer read FOwner write FOwner;
    property IsTradeable: Boolean read FTradeable write FTradeable;
    property PlayerDrop: Boolean read FPlayerDrop;

    property Player: TMapleCharacter read FPlayer write FPlayer;
    property QuestID: Word read FQuestID write FQuestID;
  end;

implementation

uses Main, MapleClient, MapleQuest, MaplePacketCreator, Settings, ChecksumCommand;

{ TDropHandler }

class procedure TDropHandler.DoDrops(Player: TMapleCharacter; Map: TMapleMap;
  DropperID, DropperOID: Integer; Origin: TPoint; FreeForAll: Boolean = False);
var
  Drops: TDropsInfo;
  Info: TDropInfo;
  Amount, d: Smallint;
  PlayerID: Integer;
  Ran: Cardinal;
  Pos: TPoint;
  Item: TItem;
  Drop: TDrop;
begin
  if not DropDataProv.HasDrops(DropperID) then
    Exit;

  d := 0;

  if Player <> nil then
    PlayerID := Player.ID
  else
    PlayerID := -1;

  Drops := DropDataProv.GetDrops(DropperID);
  Drops.Reverse;
  for Info in Drops do
  begin
    Amount := Rand.RandInt(Info.MaxAmount - Info.MinAmount) + Info.MinAmount;
    Drop := nil;

    Ran := Rand.RandInt(999999);
    //Log('[Drop-Chance] %d < %d ?', [Ran, Info.Chance * Cardinal(frmSettings.seDropRate.Value)]);
    if Ran < Info.Chance * Cardinal(frmSettings.seDropRate.Value) then
    begin
      if d mod 2 > 0 then
        Pos.X := Origin.X + (25 * (d + 1) div 2)
      else
        Pos.X := Origin.X - (25 * (d div 2));
      Pos.Y := Origin.Y;

      Pos := Map.CalcDropPos(Pos);

      //Log('[Drop-Chance] Chosen: %d | %d new: %d', [Info.ItemID, Origin.X, Pos.X]);

      if not Info.IsMesos then
      begin
        if Info.QuestID > 0 then
        begin
          if (Player = nil) or (not Player.HasStartedQuest(Info.QuestID)) then
            Continue;

          if TPlayerQuestStatus(Player.Quests[Info.QuestID]).Quest.ItemRequests.ContainsKey(Info.ItemID) and
            (Player.Inventory[GetInventory(Info.ItemID)].ListByID(Info.ItemID).Count >=
             TPlayerQuestStatus(Player.Quests[Info.QuestID]).Quest.ItemRequests[Info.ItemID]) then
            Continue;
        end;

        if GetInventory(Info.ItemID) = miEquip then
          Item := ItemDataProv.LoadEquip(Info.ItemID)
        else
          Item := TItem.Create(Info.ItemID, 0, Amount);

        Drop := TDrop.Create(Map, Item, Pos, PlayerID);

        if Info.QuestID > 0 then
        begin
          Drop.Player := Player;
          Drop.QuestID := Info.QuestID;
        end;
      end
      else
        Drop := TDrop.Create(Map, Amount * frmSettings.seMesoRate.Value, Pos, PlayerID);
    end;

    if Assigned(Drop) then
    begin
      if FreeForAll then
        Drop.DType := dtFreeForAll
      else
      if (Player <> nil) and (Player.Party <> nil) then
      begin
        Drop.DType := dtParty;
        Drop.Owner := Player.Party.ID;
      end;

      Drop.DropperOID := DropperOID;
      Drop.DoDrop(Origin);

      Inc(d);
    end;
  end;
end;

{ TDrop }

constructor TDrop.Create(Map: TMapleMap; Mesos: Integer; Pos: TPoint;
  Owner: Integer; PlayerDrop: Boolean = False);
begin
  FMesos := Mesos;
  FItem := nil;

  Init(Map, Pos, Owner, PlayerDrop);
end;

constructor TDrop.Create(Map: TMapleMap; Item: TItem; Pos: TPoint;
  Owner: Integer; PlayerDrop: Boolean = False);
begin
  FMesos := 0;
  FItem := Item;

  Init(Map, Pos, Owner, PlayerDrop);
end;

procedure TDrop.Init(Map: TMapleMap; Pos: TPoint; Owner: Integer;
  PlayerDrop: Boolean);
begin
  FType := dtNormal;
  FOwner := Owner;
  FMap := Map;
  FDropped := MAXINT;
  FPlayerDrop := PlayerDrop;
  FTradeable := True;
  FPosition := Pos;

  FQuestID := 0;
  FPlayer := nil;
end;

function TDrop.GetData: Integer;
begin
  if IsMesos then
    Result := FMesos
  else
    Result := FItem.ID;
end;

function TDrop.GetType: TObjectType;
begin
  Result := otItem;
end;

function TDrop.IsMesos: Boolean;
begin
  Result := FMesos > 0;
end;

function TDrop.IsQuestItem: Boolean;
begin
  Result := FQuestID > 0;
end;

procedure TDrop.DoDrop(Origin: TPoint);
begin
  if not Assigned(ChecksumCommand.Items) then
    FDropped := GetTickCount;

  if not IsQuestItem then
  begin
    if not FTradeable then
    begin
      FMap.BroadcastMessage(ShowDrop(Self, dsUntradeable, Origin));
      Remove(False);
    end
    else
    begin
      FMap.SpawnAndAddRangedMapObject(Self, procedure (C: TObject) begin
        TMapleClient(C).Write(ShowDrop(Self, dsShowNewFirst, Origin));
        TMapleClient(C).Write(ShowDrop(Self, dsShowNewSecond, Origin));
      end );
    end;
  end
  else
    if (FPlayer <> nil) and (FPlayer.Map.ID = FMap.ID) then
    begin
      FMap.AddMapObject(Self);
      TMapleClient(FPlayer.Client).Write(ShowDrop(Self, dsShowNewFirst, Origin));
      TMapleClient(FPlayer.Client).Write(ShowDrop(Self, dsShowNewSecond, Origin));
    end;
end;

procedure TDrop.Remove(SendPacket: Boolean);
begin
  FMap.RemoveMapObject(Self);

  if SendPacket then
    FMap.BroadcastMessage(MaplePacketCreator.RemoveDrop(Self));

  Free;
end;

procedure TDrop.Take(Player: TMapleCharacter);
begin
  FMap.RemoveMapObject(Self);

  if not IsQuestItem then
    FMap.BroadcastMessage(MaplePacketCreator.TakeDrop(FObjectID, Player.ID))
  else
    TMapleClient(Player.Client).Write(MaplePacketCreator.TakeDrop(FObjectID, Player.ID));

  Free;
end;

function TDrop.SendSpawnDataTo(Client: TObject): Boolean;
begin
  if (IsQuestItem) and (Assigned(FPlayer)) and (TMapleClient(Client).Player.ID <> FPlayer.ID) then
    Exit(False);

  TMapleClient(Client).Write(ShowDrop(Self, dsShowExisting, FPosition));
  Result := True;
end;

procedure TDrop.SendDestroyDataTo(Client: TObject);
begin
  if (IsQuestItem) and (Assigned(FPlayer)) and (TMapleClient(Client).Player.ID <> FPlayer.ID) then
    Exit;

  TMapleClient(Client).Write(MaplePacketCreator.RemoveDrop(Self));
end;

end.
