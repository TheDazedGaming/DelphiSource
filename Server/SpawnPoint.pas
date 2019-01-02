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

unit SpawnPoint;

interface

uses Windows;

type
  TSpawnPoint = class
  private
    FMonsterID: Integer;
    FPos: TPoint;
    FNextPossibleSpawn: Cardinal;
    FMobTime, FSpawnedMonsters: Integer;
    FFacesRight: Boolean;
    FFoothold: Integer;
  public
    constructor Create(AMonsterID: Integer; APos: TPoint; AMobTime, AFh: Integer; AFacesRight: Boolean);

    function ShouldSpawn: Boolean;
    procedure SpawnMonster(Map: TObject);

    procedure Killed;

    property Position: TPoint read FPos write FPos;
    property RespawnTime: Integer read FMobTime;
  end;

implementation

uses MapleMap, MapleMonster;

{ TSpawnPoint }

constructor TSpawnPoint.Create(AMonsterID: Integer; APos: TPoint; AMobTime, AFh: Integer; AFacesRight: Boolean);
begin
  FMonsterID := AMonsterID;
  FPos := APos;
  FMobTime := AMobTime;
  FNextPossibleSpawn := GetTickCount;
  FSpawnedMonsters := 0;
  FFacesRight := AFacesRight;
  FFoothold := AFh;
end;

procedure TSpawnPoint.Killed;
begin
  FNextPossibleSpawn := GetTickCount;
  if RespawnTime > 0 then
    Inc(FNextPossibleSpawn, RespawnTime * 1000)
  else
    Inc(FNextPossibleSpawn, 7000);  // xxx DieAnimationTime
  Dec(FSpawnedMonsters);
end;

function TSpawnPoint.ShouldSpawn: Boolean;
begin
  if FMobTime < 0 then
    Exit(False);

  if ((FMobTime <> 0) and (FSpawnedMonsters > 0)) or (FSpawnedMonsters > 2) then
    Exit(False);

  Result := FNextPossibleSpawn <= GetTickCount;
end;

procedure TSpawnPoint.SpawnMonster(Map: TObject);
var
  Life: TMapleMonster;
begin
  Inc(FSpawnedMonsters);
  Life := TMapleMonster.Create(FMonsterID);
  Life.Position := FPos;
  Life.FacesRight := FFacesRight;
  Life.Foothold := FFoothold;
  Life.OriginFh := FFoothold;
  Life.Map := TMapleMap(Map);
  Life.Map.SpawnMonster(Life);
  Life.Spawnpoint := Self;

  if FMobTime = 0 then
    FNextPossibleSpawn := GetTickCount + 5000;
end;

end.
