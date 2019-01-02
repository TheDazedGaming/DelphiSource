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

unit MapDataProvider;

interface

uses Windows, Types, SysUtils, ZDataset, Generics.Collections, Footholds,
     MapleMap, MaplePortal, MapleMapObject, MapleNPC, MapleReactor;

type
  TLoadedMaps = TDictionary<Integer, TMapleMap>.TValueCollection;

  TMapDataProvider = class
  private
    FMaps: TDictionary<Integer, TMapleMap>;

    function GetMaps: TLoadedMaps;
    function LoadFootholds(Q: TZQuery): TFootholdTree;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadMap(ID: Integer): TMapleMap;

    property Maps: TLoadedMaps read GetMaps;
  end;

implementation

uses MapleServerHandler, MapleMonster, SpawnPoint, LifeDataProvider, MapDebug;

{ TMapDataProvider }

constructor TMapDataProvider.Create;
begin
  FMaps := TDictionary<Integer, TMapleMap>.Create;
end;

destructor TMapDataProvider.Destroy;
var
  Map: TMapleMap;
begin
  for Map in FMaps.Values do
    Map.Free;
  FMaps.Free;

  inherited;
end;

function TMapDataProvider.GetMaps: TLoadedMaps;
begin
  Result := FMaps.Values;
end;

function TMapDataProvider.LoadMap(ID: Integer): TMapleMap;
var
  Q: TZQuery;
  Portal: TMaplePortal;
  LID, Link: Integer;
  Life: TLoadedLife;
  Pos: TPoint;
begin
  if FMaps.ContainsKey(ID) then
    Exit(FMaps[ID]);

  Q := MCDB.GetQuery;
  Q.SQL.Text := 'SELECT * FROM map_data WHERE mapid = ' + IntToStr(ID);
  Q.Open;

  if not Q.EOF then
  begin
    Result := TMapleMap.Create(ID, Q.FieldByName('mob_rate').AsSingle);
    Result.Link := Q.FieldByName('link').AsInteger;
    Result.ReturnMap := Q.FieldByName('return_map').AsInteger;
    Result.ForcedReturn := Q.FieldByName('forced_return_map').AsInteger;
    // xxx
  end
  else  // no data concerning this map
  begin
    Q.Close;
    Q.Free;
    Exit(nil);
  end;
  Q.Close;

  Link := Result.Link;
  if Link = 0 then
    Link := ID;

  // xxx seats?

  Q.SQL.Text := 'SELECT * FROM map_footholds WHERE mapid = :id';
  Q.ParamByName('id').AsInteger := Link;
  Q.Open;
  Result.Footholds := LoadFootholds(Q);
  Q.Close;

  Q.SQL.Text := 'SELECT * FROM map_portals WHERE mapid = :id';
  Q.ParamByName('id').AsInteger := Link;
  Q.Open;
  while not Q.EOF do
  begin
    Portal := TMaplePortal.Create(Q.FieldByName('id').AsInteger);
    Portal.Name := Q.FieldByName('label').AsString;
    Portal.Position := Point(Q.FieldByName('x_pos').AsInteger, Q.FieldByName('y_pos').AsInteger);
    Portal.ToMap := Q.FieldByName('destination').AsInteger;
    Portal.ToName := Q.FieldByName('destination_label').AsString;
    Portal.Script := Q.FieldByName('script').AsString;
    Portal.OnlyOnce := System.Pos('only_once', Q.FieldByName('flags').AsString) > 0;

    Result.AddPortal(Portal);

    Q.Next;
  end;

  Q.Close;

  Q.SQL.Text := 'SELECT * FROM map_life WHERE mapid = ' + IntToStr(ID);
  Q.Open;
  while not Q.EOF do
  begin
    Pos := Point(Q.FieldByName('x_pos').AsInteger, Q.FieldByName('y_pos').AsInteger);
    LID := Q.FieldByName('lifeid').AsInteger;

    if Q.FieldByName('life_type').AsString = 'npc' then
    begin
      Life := TMapleNPC.Create(LID, Q.FieldByName('life_name').AsString);
      Life.Position := Pos;
      Life.Foothold := Q.FieldByName('foothold').AsInteger;
      Life.MinClickPos := Q.FieldByName('min_click_pos').AsInteger;
      Life.MaxClickPos := Q.FieldByName('max_click_pos').AsInteger;
      Life.FacesRight := Q.FieldByName('flags').AsString <> 'faces_left';

      Result.AddMapObject(Life);
    end
    else
    if Q.FieldByName('life_type').AsString = 'mob' then
    begin
      Result.AddMobSpawn(TSpawnPoint.Create(
        LID, Pos,
        Q.FieldByName('respawn_time').AsInteger,
        Q.FieldByName('foothold').AsInteger,
        Q.FieldByName('flags').AsString <> 'faces_left')
      );
    end
    else
    if Q.FieldByName('life_type').AsString = 'reactor' then
    begin
      Life := TMapleReactor.Create(LID);
      Life.FacesRight := Q.FieldByName('flags').AsString <> 'faces_left';
      Life.Position := Pos;
      TMapleReactor(Life).RespawnTime := Q.FieldByName('respawn_time').AsInteger;

      Result.AddReactor(TMapleReactor(Life));
    end;

    Q.Next;
  end;

  FreeAndNil(Q);

  FMaps.Add(ID, Result);

 // if (Result.ID = 104040000) and (frmMapDebug.Map = nil) then
 //   frmMapDebug.Map := Result;
end;

function TMapDataProvider.LoadFootholds(Q: TZQuery): TFootholdTree;
var
  FH: TFoothold;
  FHs: TList<TFoothold>;
  LBound, UBound: TPoint;
  X1, X2, Y1, Y2: Integer;
begin
  FHs := TList<TFoothold>.Create;
  LBound := Point(10000, 10000);
  UBound := Point(-10000, -10000);
  while not Q.EOF do
  begin
    X1 := Q.FieldByName('x1').AsInteger;
    X2 := Q.FieldByName('x2').AsInteger;
    Y1 := Q.FieldByName('y1').AsInteger;
    Y2 := Q.FieldByName('y2').AsInteger;
    FH := TFoothold.Create(Point(X1, Y1), Point(X2, Y2), Q.FieldByName('id').AsInteger);
    FH.Next := Q.FieldByName('nextid').AsInteger;
    FH.Prev := Q.FieldByName('previousid').AsInteger;

    if X1 < LBound.X then
      LBound.X := X1;
    if X2 > UBound.X then
      UBound.X := X2;
    if Y1 < LBound.Y then
      LBound.Y := Y1;
    if Y2 > UBound.Y then
      LBound.Y := Y2;

    FHs.Add(FH);
    Q.Next;
  end;

  Result := TFootholdTree.Create(LBound, UBound);
  for FH in FHs do
    Result.Insert(FH);
end;

end.
