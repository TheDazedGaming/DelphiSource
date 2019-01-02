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

unit MapleMap;

interface

uses Windows, Types, SysUtils, Generics.Collections, MaplePortal, MapleStream,
     Generics.Defaults, MapleMapObject, Math, Utils, MTRand, Scheduler,
     MapleReactor, SyncObjs, SpawnPoint, Footholds;

type
  TSendAction = reference to procedure(C: TObject);

  TObjectTypes = set of TObjectType;

var
  RangedMapObjects: TObjectTypes = [otItem, otMonster, otReactor];

type
  TMapleMap = class
  private
    FID: Integer;
    FLink: Integer;
    FReturnMap: Integer;
    FForcedReturn: Integer;
    FEnterScript: string;

    FMonsterRate: Single;
    FRunningOID: Integer;
    FSpawnedMonsters: Integer;

    FMapTimer: THandle;

    FCharacters: TList<TObject>;    // uuuuniit shiit references
    FFootholds: TFootholdTree;
    FPortals: TDictionary<Integer, TMaplePortal>;
    FMapObjects: TDictionary<Integer, TMapleMapObject>;

    FMobSpawns: TList<TSpawnPoint>;
    FReactors: TList<TMapleReactor>;

    FCharCrit, FObjCrit: TCriticalSection;

    function GetMapObject(Index: Integer): TMapleMapObject;

    procedure SendObjectsToClient(Client: TObject);

    function IsNonRangedType(T: TObjectType): Boolean;

    procedure UpdateMapObjectVisibility(Chr, Obj: TMapleMapObject);
    procedure UpdateMonsterController(Mob: TLoadedLife; NewSpawn: Boolean = False);

    procedure RunTimer;
    procedure CheckMobSpawn(const CurTime: Cardinal);
    procedure CheckReactorSpawn(const CurTime: Cardinal);
    procedure ClearDrops(CurTime: Cardinal);

    function CalcPointBelow(const P: TPoint): TPoint;
  public
    constructor Create(MapID: Integer; MonsterRate: Single);
    destructor Destroy; override;

    procedure AddPlayer(Char: TObject);
    procedure RemovePlayer(Char: TObject);
    procedure MovePlayer(Char: TAnimatedMapObject; Position: TPoint);

    procedure AddPortal(Portal: TMaplePortal);
    function GetPortal(ID: Integer): TMaplePortal; overload;
    function GetPortal(Name: string): TMaplePortal; overload;

    function CalcDropPos(const P: TPoint): TPoint;
    function FindNearestSpawnpoint(From: TPoint): TMaplePortal;

    procedure AddMapObject(Obj: TMapleMapObject);
    function GetMapObjectsInRange(From: TPoint; RangeSq: Double; Types: TObjectTypes): TList<TMapleMapObject>;
    function GetMapObjects: TDictionary<Integer, TMapleMapObject>.TValueCollection;
    procedure RemoveMapObject(Obj: TMapleMapObject);

    procedure BroadcastMessage(Packet: TMapleStream); overload;
    procedure BroadcastMessage(Source: TObject; Packet: TMapleStream); overload;
    procedure BroadcastMessage(Source: TObject; Packet: TMapleStream; RepeatToSource: Boolean); overload;

    procedure AddMobSpawn(Mob: TSpawnPoint);
    function DamageMonster(Chr, Mob: TObject; Damage: Integer): Boolean;
    procedure ForceRespawn;
    procedure KillMonster(Mob, Chr: TObject; WithDrops: Boolean);
    procedure KillAllMonsters;
    procedure MoveMonster(Mob: TLoadedLife; ReportedPos: TPoint);
    procedure SpawnMonster(Mob: TLoadedLife); overload;
    procedure SpawnMonster(const ID: Integer; Pos: TPoint; FH: SmallInt; Effect: Byte = 0); overload;

    procedure SpawnAndAddRangedMapObject(Obj: TMapleMapObject; SendPacket: TSendAction);

    procedure AddReactor(Reactor: TMapleReactor);

    property ID: Integer read FID write FID;
    property Link: Integer read FLink write FLink;
    property ReturnMap: Integer read FReturnMap write FReturnMap;
    property ForcedReturn: Integer read FForcedReturn write FForcedReturn;

    property Characters: TList<TObject> read FCharacters;
    property Footholds: TFootholdTree read FFootholds write FFootholds;
    property MapObject[Index: Integer]: TMapleMapObject read GetMapObject;
  end;

implementation

uses Main, MapleCharacter, MaplePacketCreator, MapleClient, MapleMonster, MapleSummon,
     DropHandler, MapleNPC, MapleServerHandler, PortalScript, ScriptHelper;

{ TMapleMap }

constructor TMapleMap.Create(MapID: Integer; MonsterRate: Single);
begin
  FID := MapID;

  FRunningOID := 650000;
  FSpawnedMonsters := 0;
  FMonsterRate := MonsterRate;

  FCharCrit := TCriticalSection.Create;
  FObjCrit := TCriticalSection.Create;

  FCharacters := TList<TObject>.Create;
  FPortals := TDictionary<Integer, TMaplePortal>.Create;
  FMapObjects := TDictionary<Integer, TMapleMapObject>.Create;

  FMobSpawns := TList<TSpawnPoint>.Create;
  FReactors := TList<TMapleReactor>.Create;

  FMapTimer := Sched.AddRepeatedJob(10000, RunTimer);

  with MCDB.GetQuery do
  begin
    SQL.Text := 'SELECT script FROM scripts WHERE script_type = ''map_enter'' AND objectid = :id';
    ParamByName('id').Value := FID;
    Open;

    if not EOF then
      FEnterScript := FieldByName('script').AsString
    else
      FEnterScript := '';

    Close;
    Free;
  end;
end;

destructor TMapleMap.Destroy;
var
  i: Integer;
  o: TMapleMapObject;
begin
  Sched.RemoveRepeatedJob(FMapTimer);

  FreeAndNil(FCharacters);

  for i := 0 to FPortals.Count - 1 do
    FPortals[i].Free;
  FreeAndNil(FPortals);

  for o in FMapObjects.Values do
    if (not (o is TMapleCharacter)) and (not (o is TMapleReactor)) then   // reactors are freed separately, characters anyway
      o.Free;
  FreeAndNil(FMapObjects);

  for i := 0 to FMobSpawns.Count - 1 do
    FMobSpawns[i].Free;
  FreeAndNil(FMobSpawns);

  for i := 0 to FReactors.Count - 1 do
    FReactors[i].Free;
  FreeAndNil(FReactors);

  FreeAndNil(FCharCrit);
  FreeAndNil(FObjCrit);

  inherited;
end;

procedure TMapleMap.AddMapObject(Obj: TMapleMapObject);
begin
  if Obj = nil then
  begin
    Log('[FATAL] MapObject = nil!');
    Exit;
  end;

  Obj.ObjectID := FRunningOID;
  Inc(FRunningOID);

  FMapObjects.Add(Obj.ObjectID, Obj);
end;

procedure TMapleMap.AddMobSpawn(Mob: TSpawnPoint);
var
  NewPos: TPoint;
begin
  NewPos := CalcPointBelow(Mob.Position);
  Dec(NewPos.Y, 1);
  Mob.Position := NewPos;

  FMobSpawns.Add(Mob);

  if Mob.RespawnTime = -1 then
    Mob.SpawnMonster(Self);
end;

procedure TMapleMap.AddPlayer(Char: TObject);
var
  F: string;
  S: TMapleSummon;
begin
  FCharCrit.Enter;
  try
    FCharacters.Add(Char);
  finally
    FCharCrit.Leave;
  end;

  // Run the map-enter script if it exists
  if FEnterScript <> '' then
  begin
    F := ExtractFilePath(ParamStr(0)) + 'Scripts\Map\' + FEnterScript + SCRIPT_EXT;
    if FileExists(F) then
    begin
      Log('[MapEnterScript] Executing: %s%s', [FEnterScript, SCRIPT_EXT]);

      with TPortalScript.Create(F, TMapleCharacter(Char).Client, FID) do
      begin
        Enter;
        Free;
      end;
    end
    else
      Log('[MapEnterScript] File not found: %s%s', [FEnterScript, SCRIPT_EXT]);
  end;

  // Let all the other players & NPCs etc spawn for US
  SendObjectsToClient(TMapleCharacter(Char).Client);

  FMapObjects.Add(TMapleCharacter(Char).ObjectID, TMapleCharacter(Char));

  // Tell all the OTHERS that we are there
  BroadcastMessage(Char, SpawnPlayer(TMapleCharacter(Char)), False);

  for S in TMapleCharacter(Char).Summons do
  begin
    S.Position := TMapleCharacter(Char).Position;
    AddMapObject(S);
    BroadcastMessage(S.GetSpawnPacket(False));
  end;

  // Show green costume :D
  {$IFNDEF EMS}
  if FID in [1, 2] then
    TMapleClient(TMapleCharacter(Char).Client).Write(ShowEquipEffect);
  {$ENDIF}

  // The client doesn't automatically reset it, so if you've done the Aran tutorial and then play another char, it would also have these stats
  TMapleClient(TMapleCharacter(Char).Client).Write(RemoveTemporaryStats);

  // Max stats for Aran Tutorial mob maps
  if (FID >= 914000200) and (FID <= 914000220) then
    TMapleClient(TMapleCharacter(Char).Client).Write(AranTutorialMaxStats);

  // Show Tutorial Summon (Cygnus & Aran Intro)
  if ((FID >= 140090100) and (FID <= 140090500)) or ((FID >= 130000000) and (FID < 131000000) and (TMapleCharacter(Char).Level < 10)) then
  begin
    // Don't spawn it in the first Cygnus map - there's a special spawn point
    if ((FID = 130030000) and (TMapleCharacter(Char).AreaData.ContainsKey(20021))) or (FID <> 130030000) then
      TMapleClient(TMapleCharacter(Char).Client).Write(SpawnTutorialSummon(True));
  end;

  TMapleCharacter(Char).ReceivePartyMemberHP;
end;

procedure TMapleMap.AddPortal(Portal: TMaplePortal);
begin
  FPortals.Add(Portal.ID, Portal);
end;

procedure TMapleMap.AddReactor(Reactor: TMapleReactor);
begin
  FReactors.Add(Reactor);
end;

procedure TMapleMap.BroadcastMessage(Packet: TMapleStream);
begin
  BroadcastMessage(nil, Packet);
end;

procedure TMapleMap.BroadcastMessage(Source: TObject; Packet: TMapleStream);
var
  Char: TObject;
begin
  FCharCrit.Enter;
  try
    for Char in FCharacters do
      if TMapleCharacter(Char) <> TMapleCharacter(Source) then
        TMapleClient(TMapleCharacter(Char).Client).Write(Packet, False);   // Don't free the packet or there will be an exception @ the next char
  finally
    FCharCrit.Leave;
  end;

  FreeAndNil(Packet);
end;

procedure TMapleMap.BroadcastMessage(Source: TObject; Packet: TMapleStream;
  RepeatToSource: Boolean);
begin
  if RepeatToSource then
    BroadcastMessage(nil, Packet)
  else
    BroadcastMessage(Source, Packet);
end;

function TMapleMap.GetPortal(ID: Integer): TMaplePortal;
begin
  if not FPortals.ContainsKey(ID) then
    Result := nil
  else
    Result := FPortals[ID];
end;

function TMapleMap.GetMapObject(Index: Integer): TMapleMapObject;
begin
  Result := nil;

  if FMapObjects.ContainsKey(Index) then
    Result := FMapObjects[Index];
end;

function TMapleMap.GetMapObjects: TDictionary<Integer, TMapleMapObject>.TValueCollection;
begin
  Result := FMapObjects.Values;
end;

function TMapleMap.GetMapObjectsInRange(From: TPoint; RangeSq: Double;
  Types: TObjectTypes): TList<TMapleMapObject>;
var
  O: TMapleMapObject;
begin
  Result := TList<TMapleMapObject>.Create;
  FObjCrit.Enter;
  try
    for O in FMapObjects.Values do
      if O.GetType in Types then
        if DistanceSq(From, O.Position) <= RangeSq then
          Result.Add(O);
  finally
    FObjCrit.Leave;
  end;
end;

function TMapleMap.GetPortal(Name: string): TMaplePortal;
var
  Portal: TMaplePortal;
begin
  for Portal in FPortals.Values do
    if Portal.Name = Name then
      Exit(Portal);

  Result := nil;
end;

function TMapleMap.IsNonRangedType(T: TObjectType): Boolean;
begin
  Result := T in [otNPC, otPlayer, otMist];   // xxx otHiredMerchant
end;

function TMapleMap.DamageMonster(Chr, Mob: TObject; Damage: Integer): Boolean;
begin
  if not TMapleMonster(Mob).IsAlive then
    Exit(True);

  TMapleMonster(Mob).Damage(TMapleCharacter(Chr), Damage, True);

  Result := not TMapleMonster(Mob).IsAlive;
  if Result then
    KillMonster(Mob, Chr, True);
end;

procedure TMapleMap.KillAllMonsters;
var
  L: TList<TMapleMonster>;
  Obj: TMapleMapObject;
  M: TMapleMonster;
begin
  L := TList<TMapleMonster>.Create;
  try
    for Obj in FMapObjects.Values do
      if Obj is TMapleMonster then
        L.Add(TMapleMonster(Obj));

    for M in L do
    begin
      if Assigned(M.Controller) then
        M.Controller.StopControllingMonster(M);
      RemoveMapObject(M);
      BroadcastMessage(MaplePacketCreator.KillMonster(M.ObjectID, False));

      if M.Spawnpoint <> nil then
        M.Spawnpoint.Killed;

      M.Free;
    end;
  finally
    L.Free;
  end;
end;

procedure TMapleMap.KillMonster(Mob, Chr: TObject; WithDrops: Boolean);
var
  DropOwner: TMapleCharacter;
  Summon: Integer;
begin
  TMapleMonster(Mob).HP := 0;
  BroadcastMessage(MaplePacketCreator.KillMonster(TMapleMonster(Mob).ObjectID, True));
  RemoveMapObject(TMapleMonster(Mob));
  Dec(FSpawnedMonsters);

  DropOwner := TMapleMonster(Mob).KillBy(TMapleCharacter(Chr));

  if WithDrops then
  begin
    if DropOwner = nil then
      DropOwner := TMapleCharacter(Chr);

    TDropHandler.DoDrops(DropOwner, Self, TMapleMonster(Mob).ID, TMapleMonster(Mob).ObjectID,
      TMapleMonster(Mob).Position, TMapleMonster(Mob).Stats^.PublicReward);
  end;

  if Assigned(TMapleMonster(Mob).Stats^.Summons) then
    for Summon in TMapleMonster(Mob).Stats^.Summons do
      SpawnMonster(Summon, TMapleMonster(Mob).Position, TMapleMonster(Mob).Foothold);

  // ==== Respawning ====
  try
    if TMapleMonster(Mob).Spawnpoint <> nil then
      TMapleMonster(Mob).Spawnpoint.Killed;
  finally
    FreeAndNil(TMapleMonster(Mob));
  end;
end;

procedure TMapleMap.MoveMonster(Mob: TLoadedLife; ReportedPos: TPoint);
var
  Chr: TObject;
begin
  Mob.Position := ReportedPos;
  FCharCrit.Enter;
  try
    for Chr in FCharacters do
      UpdateMapObjectVisibility(TMapleMapObject(Chr), Mob);
  finally
    FCharCrit.Leave;
  end;
end;

procedure TMapleMap.MovePlayer(Char: TAnimatedMapObject; Position: TPoint);
var
  Obj: TMapleMapObject;
  Range: TList<TMapleMapObject>;
begin
  Char.Position := Position;

  for Obj in TMapleCharacter(Char).VisibleMapObjects do
    if FMapObjects.ContainsValue(Obj) then
      if FMapObjects[Obj.ObjectID] = Obj then
        UpdateMapObjectVisibility(Char, Obj)
      else
        TMapleCharacter(Char).VisibleMapObjects.Remove(Obj);

  Range := GetMapObjectsInRange(Char.Position, MAX_VIEW_RANGE_SQ, RangedMapObjects);
  try
    for Obj in Range do
      if Obj <> nil then
        if not TMapleCharacter(Char).VisibleMapObjects.Contains(Obj) then
        begin
          Obj.SendSpawnDataTo(TMapleCharacter(Char).Client);
          TMapleCharacter(Char).VisibleMapObjects.Add(Obj);

          if (Obj is TMapleMonster) and (TMapleMonster(Obj).Controller = nil) then
            UpdateMonsterController(TLoadedLife(Obj), False);
        end;
  finally
    Range.Free;
  end;
end;

function TMapleMap.FindNearestSpawnpoint(From: TPoint): TMaplePortal;
var
  ShortestDist, Dist: Double;
  Port: TMaplePortal;
begin
  Result := nil;
  ShortestDist := MaxDouble;
  for Port in FPortals.Values do
  begin
    Dist := DistanceSq(Port.Position, From);
    if (Port.Name = 'sp') and (Dist < ShortestDist) then
    begin
      Result := Port;
      ShortestDist := Dist;
    end;
  end;
end;

procedure TMapleMap.ForceRespawn;
var
  SP: TSpawnPoint;
begin
  for SP in FMobSpawns do
    SP.SpawnMonster(Self);
end;

procedure TMapleMap.RemoveMapObject(Obj: TMapleMapObject);
begin
  FObjCrit.Enter;
  try
    FMapObjects.Remove(Obj.ObjectID);
  finally
    FObjCrit.Leave;
  end;
end;

procedure TMapleMap.RemovePlayer(Char: TObject);
var
  Obj: TObject;
  S, Rem: TMapleSummon;
begin
  FCharCrit.Enter;
  try
    FCharacters.Remove(Char);
  finally
    FCharCrit.Leave;
  end;
  FMapObjects.Remove(TMapleCharacter(Char).ObjectID);

  Rem := nil;
  for S in TMapleCharacter(Char).Summons do
  begin
    RemoveMapObject(S);
    BroadcastMessage(S.GetRemovePacket(rmNone));

    if S.IsStatic then
      Rem := S;  // Can't delete it in this loop as list would be changed
  end;

  if Rem <> nil then
  begin
    TMapleCharacter(Char).Summons.Remove(Rem);
    Rem.Free;
  end;

  // Tell the others that we left
  BroadcastMessage(Char, MaplePacketCreator.RemovePlayer(TMapleCharacter(Char).ID));

  for Obj in TMapleCharacter(Char).ControlledMonsters do
  begin
    TMapleMonster(Obj).Controller := nil;
    UpdateMonsterController(TMapleMonster(Obj));
  end;

  TMapleCharacter(Char).LeaveMap;
end;

procedure TMapleMap.SendObjectsToClient(Client: TObject);
var
  Obj: TMapleMapObject;
  Range: TList<TMapleMapObject>;
begin
  // Make all other players etc. visible for this Client
  for Obj in FMapObjects.Values do
    try
      if IsNonRangedType(TMapleMapObject(Obj).GetType) then
        TMapleMapObject(Obj).SendSpawnDataTo(Client);
    except
      Log('SendObjectsToClient - Opcode missing? ' + Exception(ExceptObject).Message);
    end;

  Range := GetMapObjectsInRange(TMapleClient(Client).Player.Position, MAX_VIEW_RANGE_SQ,
    RangedMapObjects);
  try
    for Obj in Range do
      try
        if TMapleMapObject(Obj).SendSpawnDataTo(Client) then
          TMapleClient(Client).Player.VisibleMapObjects.Add(Obj);
      except
        Log('SendObjectsToClient - Opcode missing? ' + Exception(ExceptObject).Message);
      end;
  finally
    Range.Free;
  end;

  for Obj in TMapleClient(Client).Player.VisibleMapObjects do
    if Obj is TMapleMonster then
      UpdateMonsterController(TMapleMonster(Obj), False);
end;

procedure TMapleMap.RunTimer;
var
  Time: Cardinal;
begin
  Time := GetTickCount;
  CheckMobSpawn(Time);
  CheckReactorSpawn(Time);
  ClearDrops(Time);
end;

procedure TMapleMap.CheckReactorSpawn(const CurTime: Cardinal);
var
  R: TMapleReactor;
begin
  for R in FReactors do
    if (not R.IsAlive) and (R.SpawnAt > -1) and (R.SpawnAt <= CurTime) then
    begin
      R.Revive;
      SpawnAndAddRangedMapObject(R, procedure(C: TObject) begin
        TMapleClient(C).Write(SpawnReactor(R));
      end );
    end;
end;

procedure TMapleMap.CheckMobSpawn(const CurTime: Cardinal);
var
  ToSpawn: Integer;
  SP: TSpawnpoint;
  A: array of TSpawnPoint;
  i: Integer;

  procedure Swap(I1, I2: Integer);
  var
    Tmp: TSpawnPoint;
  begin
    Tmp := A[I1];
    A[I1] := A[I2];
    A[I2] := Tmp;
  end;

begin
  if FCharacters.Count = 0 then
    Exit;

  ToSpawn := Round((FMobSpawns.Count / FMonsterRate) - FSpawnedMonsters);
  if ToSpawn <= 0 then
    Exit;

  // Randomize SPs
  SetLength(A, FMobSpawns.Count);
  for i := 0 to High(A) do
    A[i] := FMobSpawns[i];
  for i := Length(A) downto 2 do
    Swap(i - 1, Rand.RandInt(i - 1));
  FMobSpawns.Clear;
  FMobSpawns.AddRange(A);

  for SP in FMobSpawns do
    if SP.ShouldSpawn then
    begin
      SP.SpawnMonster(Self);

      Dec(ToSpawn);
      if ToSpawn = 0 then
        Break;
    end;
end;

procedure TMapleMap.ClearDrops(CurTime: Cardinal);
var
  MO: TMapleMapObject;
begin
  Dec(CurTime, 180000);   // 3 Minutes
  for MO in FMapObjects.Values do
    if MO is TDrop then
      if TDrop(MO).Dropped < CurTime then
        TDrop(MO).Remove(True);
end;

function TMapleMap.CalcDropPos(const P: TPoint): TPoint;
begin
  Result := CalcPointBelow(Point(P.X, P.Y - 99));
  if (Result.X = 0) and (Result.Y = 0) then
    Result := P;
end;

function TMapleMap.CalcPointBelow(const P: TPoint): TPoint;
var
  F: TFoothold;
  S1, S2, S5: Double;
begin
  F := FFootholds.FindBelow(P);
  if F = nil then
  begin
    Log('Point below not found');
    Exit(Point(0, 0));
  end;

  Result.X := P.X;
  Result.Y := F.Y1;
  if (not F.IsWall) and (F.Y1 <> F.Y2) then
  begin
    S1 := Abs(F.Y2 - F.Y1);
    S2 := Abs(F.X2 - F.X1);
    S5 := Cos(ArcTan(S2 / S1)) * (Abs(P.X - F.X1) / Cos(ArcTan(S1 / S2)));
    if F.Y2 < F.Y1 then
      Result.Y := F.Y1 - Trunc(S5)
    else
      Result.Y := F.Y1 + Trunc(S5);
  end;
end;

procedure TMapleMap.SpawnAndAddRangedMapObject(Obj: TMapleMapObject;
  SendPacket: TSendAction);
var
  Chr: TObject;
begin
  if Obj = nil then
  begin
    Log('[FATAL] MapObject = nil!');
    Exit;
  end;

  Obj.ObjectID := FRunningOID;
  Inc(FRunningOID);

  FCharCrit.Enter;
  try
    for Chr in FCharacters do
      if DistanceSq(TMapleCharacter(Chr).Position, Obj.Position) <= MAX_VIEW_RANGE_SQ then
      begin
        SendPacket(TMapleCharacter(Chr).Client);
        TMapleCharacter(Chr).VisibleMapObjects.Add(Obj);
      end;
  finally
    FCharCrit.Leave;
  end;

  FMapObjects.Add(Obj.ObjectID, Obj);
end;

procedure TMapleMap.SpawnMonster(const ID: Integer; Pos: TPoint; FH: SmallInt; Effect: Byte = 0);
var
  Mob: TMapleMonster;
begin
  Mob := TMapleMonster.Create(ID);
  Mob.Position := Pos;
  Mob.Foothold := FH;
  Mob.OriginFH := FH;
  Mob.Map := Self;

  AddMapObject(Mob);

  BroadcastMessage(MaplePacketCreator.SpawnMonster(Mob, False, Effect));

  UpdateMonsterController(Mob, True);
end;

procedure TMapleMap.SpawnMonster(Mob: TLoadedLife);
begin
  Inc(FSpawnedMonsters);
  SpawnAndAddRangedMapObject(Mob,
    procedure(C: TObject)
    begin
      TMapleClient(C).Write(MaplePacketCreator.SpawnMonster(TMapleMonster(Mob), True));
    end );
  UpdateMonsterController(Mob, True);
end;

procedure TMapleMap.UpdateMonsterController(Mob: TLoadedLife; NewSpawn: Boolean = False);
var
  CurDif, MaxDif: Integer;
  NewController: TMapleCharacter;
  Chr: TObject;
begin
  MaxDif := MAX_VIEW_RANGE_SQ;
  NewController := nil;

  if not TMapleMonster(Mob).IsAlive then
    Exit;

  if TMapleMonster(Mob).Controller <> nil then
    if TMapleMonster(Mob).Controller.Map <> Self then
      TMapleMonster(Mob).Controller.StopControllingMonster(Mob)
    else
      Exit;

  FCharCrit.Enter;
  try
    for Chr in FCharacters do
    begin
      CurDif := DistanceSq(TMapleCharacter(Chr).Position, Mob.Position);
      if CurDif < MaxDif then
      begin
        MaxDif := CurDif;
        NewController := TMapleCharacter(Chr);
      end;
    end;
  finally
    FCharCrit.Leave;
  end;

  if NewController <> nil then
    NewController.ControlMonster(Mob, False, NewSpawn);
end;

procedure TMapleMap.UpdateMapObjectVisibility(Chr, Obj: TMapleMapObject);
begin
  if not TMapleCharacter(Chr).VisibleMapObjects.Contains(Obj) then
  begin
    if DistanceSq(Chr.Position, Obj.Position) <= MAX_VIEW_RANGE_SQ then
    begin
      TMapleCharacter(Chr).VisibleMapObjects.Add(Obj);
      Obj.SendSpawnDataTo(TMapleCharacter(Chr).Client);
    end;
  end
  else
    if DistanceSq(Chr.Position, Obj.Position) > MAX_VIEW_RANGE_SQ then
    begin
      if (Obj is TMapleMonster) and (TMapleMonster(Obj).Controller = TMapleCharacter(Chr)) then
      begin
        TMapleMonster(Obj).Controller := nil;
        TMapleCharacter(Chr).StopControllingMonster(Obj);
        UpdateMonsterController(TLoadedLife(Obj), False);
      end;

      TMapleCharacter(Chr).VisibleMapObjects.Remove(Obj);
      Obj.SendDestroyDataTo(TMapleCharacter(Chr).Client);
    end;
end;

end.
