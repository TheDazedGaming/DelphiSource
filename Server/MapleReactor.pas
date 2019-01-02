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

unit MapleReactor;

interface

uses Windows, Generics.Collections, Types, MapleMapObject;

type
  TReactorStateInfo = record
    NextState: Byte;
    SType: SmallInt;
    ItemID: Integer;
    ItemQuantity: SmallInt;
    Timeout: Integer;
    Lt, Rb: TPoint;
  end;
  PReactorStateInfo = ^TReactorStateInfo;

  TReactorStats = record
    MaxStates: Byte;
    Link: Integer;
    RemoveInFieldSet: Boolean;
    ActivateByTouch: Boolean;
    States: TDictionary<Byte, TList<PReactorStateInfo>>;
  end;
  PReactorStats = ^TReactorStats;

  TMapleReactor = class(TLoadedLife)
  private
    FAlive: Boolean;
    FRespawnTime: Integer;
    FSpawnAt: Int64;
    FState: Byte;
    FStats: PReactorStats;
  public
    constructor Create(const LifeID: Integer); override;
    function GetType: TObjectType; override;
    function SendSpawnDataTo(Client: TObject): Boolean; override;
    procedure SendDestroyDataTo(Client: TObject); override;

    procedure Hit(CharPos: Integer; Stance: SmallInt; Client: TObject);
    procedure Revive;

    property IsAlive: Boolean read FAlive write FAlive;
    property RespawnTime: Integer read FRespawnTime write FRespawnTime;
    property SpawnAt: Int64 read FSpawnAt write FSpawnAt;
    property State: Byte read FState write FState;
  end;

implementation

uses Main, MapleClient, MaplePacketCreator, ReactorDataProvider, MapleMap,
     DropHandler;

{ TMapleReactor }

constructor TMapleReactor.Create(const LifeID: Integer);
begin
  inherited;

  FAlive := False;
  FSpawnAt := 0;
  FState := 0;
  FStats := ReactorDataProv.GetReactorStats(LifeID);
end;

function TMapleReactor.GetType: TObjectType;
begin
  Result := otReactor;
end;

procedure TMapleReactor.Hit(CharPos: Integer; Stance: SmallInt;
  Client: TObject);
var
  REvent: PReactorStateInfo;
  Map: TMapleMap;
begin
  if (not FAlive) or (FStats = nil) or (FState >= FStats^.MaxStates - 1) then
    Exit;

  Log('[Reactor] Hitting');
  REvent := FStats^.States[FState][0];
  if REvent^.NextState < FStats^.MaxStates - 1 then
  begin
    if REvent^.SType = 100 then
      Exit;

    Map := TMapleClient(Client).Player.Map;
    // Trigger
    Map.BroadcastMessage(TriggerReactor(Self));
    // Update state
    FState := REvent^.NextState;
    // Trigger again with new state
    Map.BroadcastMessage(TriggerReactor(Self));
  end
  else
  begin
    Map := TMapleClient(Client).Player.Map;

    // xxx run script if required?

    TDropHandler.DoDrops(TMapleClient(Client).Player, Map, FID, FObjectID,
      Point(FPosition.X, TMapleClient(Client).Player.Position.Y));

    FState := REvent^.NextState;
    if RespawnTime > -1 then
      FSpawnAt := GetTickCount + Cardinal(RespawnTime) * 1000
    else
      FSpawnAt := -1;
    FAlive := False;

    Map.RemoveMapObject(Self);
    Map.BroadcastMessage(DestroyReactor(Self));
  end;
end;

procedure TMapleReactor.Revive;
begin
  FAlive := True;
  FState := 0;
end;

function TMapleReactor.SendSpawnDataTo(Client: TObject): Boolean;
begin
  if FAlive then
    TMapleClient(Client).Write(SpawnReactor(Self));

  Result := FAlive;
end;

procedure TMapleReactor.SendDestroyDataTo(Client: TObject);
begin
  if FAlive then  // don't remove it if it isn't even spawned
    TMapleClient(Client).Write(DestroyReactor(Self));
end;

end.
