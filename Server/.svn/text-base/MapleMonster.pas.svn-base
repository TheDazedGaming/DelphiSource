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

unit MapleMonster;

interface

uses Windows, SysUtils, Math, Generics.Collections, Scheduler, GameLogic, MapleStream,
     MapleMapObject, MapleCharacter, MapleMap, ChannelServer, DropDataProvider,
     MapleParty, LifeDataProvider, SpawnPoint, SkillDataProvider, MTRand;

type
  TAttackingMapleCharacter = record
    Char: TMapleCharacter;
    LastAttackTime: Cardinal;

    constructor Create(AChar: TMapleCharacter; ALastAttackTime: Cardinal);
  end;

  TOnePartyAttacker = record
    LastKnownParty: TMapleParty;
    Damage: Integer;
    LastAttackTime: Cardinal;

    constructor Create(Party: TMapleParty; ADamage: Integer);
  end;

  TMobStatus = record
    Status: Cardinal;
    Val, SkillID, Time: Integer;

    constructor Create(AStatus: Cardinal; AVal, ASkillID, ATime: Integer);
  end;

  TMapleMonster = class(TLoadedLife)
  private
    FStats: PMonsterStats;
    FController, FHighestDamageChar: TMapleCharacter;
    FOriginFh: Integer;
    FHP, FMP: Integer;
    FMPEaterCount: Byte;
    FStatus: Cardinal;
    FStati: TDictionary<Cardinal, TMobStatus>;
    FStatusTimers: TList<THandle>;
    FMap: TMapleMap;
    FSpawnpoint: TSpawnpoint;

    type
      TAttackerEntry = class
      protected
        FDamage: Integer;
        FCServ: TChannelServer;
      public
        procedure AddDamage(From: TMapleCharacter; Damage: Integer; UpdateAttackTime: Boolean); virtual; abstract;
        function GetAttackers: TList<TAttackingMapleCharacter>; virtual; abstract;
        procedure KilledMob(Mob: TMapleMonster; Map: TMapleMap; BaseExp: Integer; MostDamage: Boolean); virtual; abstract;

        property Damage: Integer read FDamage;
      end;

      TSingleAttackerEntry = class(TAttackerEntry)
      private
        FChrID: Integer;
        FLastAttackTime: Cardinal;
      public
        constructor Create(From: TMapleCharacter; CServ: TChannelServer);

        procedure AddDamage(From: TMapleCharacter; Damage: Integer; UpdateAttackTime: Boolean); override;
        function GetAttackers: TList<TAttackingMapleCharacter>; override;
        procedure KilledMob(Mob: TMapleMonster; Map: TMapleMap; BaseExp: Integer; MostDamage: Boolean); override;

        property CharID: Integer read FChrID;
      end;

      TPartyAttackerEntry = class(TAttackerEntry)
      private
        FAttackers: TDictionary<Integer, TOnePartyAttacker>;
        FPartyID: Integer;

        function ResolveAttackers: TDictionary<TMapleCharacter, TOnePartyAttacker>;
      public
        constructor Create(APartyID: Integer; CServ: TChannelServer);
        destructor Destroy; override;

        procedure AddDamage(From: TMapleCharacter; Damage: Integer; UpdateAttackTime: Boolean); override;
        function GetAttackers: TList<TAttackingMapleCharacter>; override;
        procedure KilledMob(Mob: TMapleMonster; Map: TMapleMap; BaseExp: Integer; MostDamage: Boolean); override;

        property PartyID: Integer read FPartyID;
      end;
  private
    FAttackers: TList<TAttackerEntry>;
  public
    constructor Create(const LifeID: Integer); reintroduce;
    destructor Destroy; override;

    procedure AddStatus(Info: TList<TMobStatus>);
    function HasStatus(AStatus: Cardinal): Boolean;
    procedure StatusPacket(var Stream: TMapleStream);

    procedure Damage(From: TMapleCharacter; Damage: Integer; UpdateAttackTime: Boolean);
    function KillBy(Killer: TMapleCharacter): TMapleCharacter;

    procedure GiveExpToCharacter(Attacker: TMapleCharacter; Exp, ExpSharerCount: Integer; HighestDamage: Boolean);

    function MPEat(Player: TMapleCharacter; Skill: PSkillLevelInfo): Boolean;

    function SendSpawnDataTo(Client: TObject): Boolean; override;
    procedure SendDestroyDataTo(Client: TObject); override;

    function GetType: TObjectType; override;
    function IsAlive: Boolean;

    property Stats: PMonsterStats read FStats;
    property Controller: TMapleCharacter read FController write FController;
    property OriginFh: Integer read FOriginFh write FOriginFh;
    property Spawnpoint: TSpawnpoint read FSpawnpoint write FSpawnpoint;

    property HP: Integer read FHP write FHP;
    property MP: Integer read FMP write FMP;
    property Map: TMapleMap read FMap write FMap;

    property Status: Cardinal read FStatus write FStatus;
    property Statuses: TDictionary<Cardinal, TMobStatus> read FStati;
  end;

implementation

uses Main, MapleClient, MaplePacketCreator;

{ TMobStatus }

constructor TMobStatus.Create(AStatus: Cardinal; AVal, ASkillID, ATime: Integer);
begin
  Status := AStatus;
  Val := AVal;
  SkillID := ASkillID;
  Time := ATime;
end;

{ TMapleMonster }

constructor TMapleMonster.Create(const LifeID: Integer);
begin
  inherited Create(LifeID);

  FSpawnpoint := nil;
  FController := nil;

  FAttackers := TList<TAttackerEntry>.Create;
  FStati := TDictionary<Cardinal, TMobStatus>.Create;
  FStatusTimers := TList<THandle>.Create;
  FStatus := 0;

  FStats := LifeDataProv.GetMonsterStats(FID);
  if FStats = nil then
    raise Exception.CreateFmt('Couldn''t find monster in database: %d', [FID]);

  FHP := FStats^.HP;
  FMP := FStats^.MP;
  FMPEaterCount := 0;
end;

destructor TMapleMonster.Destroy;
var
  AE: TAttackerEntry;
  Tmr: THandle;
begin
  for AE in FAttackers do
    AE.Free;
  FreeAndNil(FAttackers);
  FStati.Free;

  for Tmr in FStatusTimers do
    Sched.CancelSchedule(Tmr);
  FStatusTimers.Free;

  inherited;
end;

procedure TMapleMonster.AddStatus(Info: TList<TMobStatus>);
var
  AddedStatus, CurStat: Cardinal;
  Timer: THandle;
  Stat: TMobStatus;
begin
  AddedStatus := 0;
  for Stat in Info do
  begin
    CurStat := Stat.Status;
    FStati.AddOrSetValue(CurStat, Stat);
    Inc(AddedStatus, CurStat);

    Timer := Sched.AddSchedule(Stat.Time * 1000, procedure
    begin
      if HasStatus(CurStat) and (FHP > 0) then
      begin
        FStatus := FStatus xor CurStat;
        FStati.Remove(CurStat);
        FStatusTimers.Remove(Timer);
        FMap.BroadcastMessage(RemoveMobStatus(FObjectID, CurStat));
      end;
    end);
    FStatusTimers.Add(Timer);
  end;

  FStatus := 0;
  for CurStat in FStati.Keys do
    FStatus := FStatus or CurStat;

  FMap.BroadcastMessage(ApplyMobStatus(FObjectID, AddedStatus, Info, 300));
end;

function TMapleMonster.HasStatus(AStatus: Cardinal): Boolean;
begin
  Result := (FStatus and AStatus) <> 0;
end;

procedure TMapleMonster.StatusPacket(var Stream: TMapleStream);
var
  i: Cardinal;
  Stats: TList<Cardinal>;
begin
  {$IF DEFINED(CHAOS)}  // UINT256
  Stream.WriteInt64(0);
  Stream.WriteInt64(0);
  {$ELSEIF DEFINED(AFTERSHOCK)}  // UINT160
  Stream.WriteInt(0);
  {$IFEND}  // UINT128
  Stream.WriteInt64(0);
  Stream.WriteInt64(Int64(FStatus) shl 32);  // Equals WriteInt(0); WriteInt(Status);

  // Must be sorted ascending by status
  Stats := TList<Cardinal>.Create(FStati.Keys);
  try
    Stats.Sort;

    for i in Stats do
      with TMobStatus(FStati[i]) do
      begin
        Stream.WriteShort(Val);
        if SkillID >= 0 then
          Stream.WriteInt(SkillID)
        else
        begin
          // xxx Mobskills
          // Stream.WriteShort(MobSkill);
		   	  // Stream.WriteShort(Level);
        end;
        Stream.WriteShort(1);
      end;
  finally
    Stats.Free;
  end;
end;

procedure TMapleMonster.Damage(From: TMapleCharacter; Damage: Integer;
  UpdateAttackTime: Boolean);
var
  Attacker, AE: TAttackerEntry;
  Found: Boolean;
  RDamage, RemHPPercentage: Integer;
  OKTime: Cardinal;
  AChars: TList<TAttackingMapleCharacter>;
  AChar: TAttackingMapleCharacter;
begin
  Attacker := nil;
  Found := False;
  for AE in FAttackers do
  begin
    if AE is TSingleAttackerEntry then
      Found := TSingleAttackerEntry(AE).CharID = From.ID
    else
    if (AE is TPartyAttackerEntry) and (From.Party <> nil) then
      Found := TPartyAttackerEntry(AE).PartyID = From.Party.ID
    else
      Found := False;

    if Found then
    begin
      Attacker := AE;
      Break;
    end;
  end;

  if not Found then
  begin
    if From.Party = nil then
      Attacker := TSingleAttackerEntry.Create(From, TMapleClient(From.Client).Channel)
    else
      Attacker := TPartyAttackerEntry.Create(From.Party.ID, TMapleClient(From.Client).Channel);
    FAttackers.Add(Attacker);
  end;

  RDamage := Max(0, Min(Damage, FHP));
  Attacker.AddDamage(From, RDamage, UpdateAttackTime);
  Dec(FHP, RDamage);

  RemHPPercentage := Ceil((FHP * 100) / FStats^.HP);
  if RemHPPercentage < 1 then
    RemHPPercentage := 1;

  OKTime := GetTickCount - 4000;
  // xxx Boss HP bar handling
  for AE in FAttackers do
  begin
    AChars := AE.GetAttackers;
    try
      for AChar in AChars do
        if (AChar.Char.Map = From.Map) and (AChar.LastAttackTime >= OKTime) then
          TMapleClient(AChar.Char.Client).Write(ShowMonsterHP(FObjectID, RemHPPercentage));
    finally
      AChars.Free;
    end;
  end;
end;

function TMapleMonster.KillBy(Killer: TMapleCharacter): TMapleCharacter;
var
  TotalBaseExp, HighestDamage, BaseExp: Integer;
  Highest, AE: TAttackerEntry;
begin
  TotalBaseExp := FStats^.EXP;
  Highest := nil;
  HighestDamage := 0;

  for AE in FAttackers do
    if AE.Damage > HighestDamage then
    begin
      Highest := AE;
      HighestDamage := AE.Damage;
    end;

  for AE in FAttackers do
  begin
    BaseExp := Ceil(TotalBaseExp * (AE.Damage / FStats^.HP));
    AE.KilledMob(Self, Killer.Map, BaseExp, AE = Highest);
  end;

  if FController <> nil then
    FController.StopControllingMonster(Self);

  Result := FHighestDamageChar;
  FHighestDamageChar := nil;    // may not keep hard references to MapleCharacter outside of MapleMap & ChannelServer
end;

function TMapleMonster.MPEat(Player: TMapleCharacter; Skill: PSkillLevelInfo): Boolean;
var
  Val: Integer;
begin
  Result := (FMPEaterCount < 3) and (FMP > 0) and (Rand.RandInt(99) < Skill.Prop);
  if Result then
  begin
    Val := Min(Stats.MP * Skill.X div 100, FMP);
    Dec(FMP, Val);

    Player.AddMP(Val);
  end;
end;

procedure TMapleMonster.GiveExpToCharacter(Attacker: TMapleCharacter; Exp,
  ExpSharerCount: Integer; HighestDamage: Boolean);
begin
  if HighestDamage then
    FHighestDamageChar := Attacker;

  if Attacker.HP = 0 then
    Exit;

  if Exp < 0 then
  begin
    Log('[WARNING] Exp < 0 --> %d', [Exp]);
    Exp := MAXINT;
  end;

  Attacker.GainEXP(Exp, True, False, HighestDamage);
  Attacker.MobKilled(FID);
end;

function TMapleMonster.GetType: TObjectType;
begin
  Result := otMonster;
end;

function TMapleMonster.IsAlive: Boolean;
begin
  Result := FHP > 0;
end;

function TMapleMonster.SendSpawnDataTo(Client: TObject): Boolean;
begin
  TMapleClient(Client).Write(SpawnMonster(Self, False));

  Result := True;
end;

procedure TMapleMonster.SendDestroyDataTo(Client: TObject);
begin
  TMapleClient(Client).Write(KillMonster(Self.ObjectID, False));
end;

{ TAttackingMapleCharacter }

constructor TAttackingMapleCharacter.Create(AChar: TMapleCharacter;
  ALastAttackTime: Cardinal);
begin
  Char := AChar;
  LastAttackTime := ALastAttackTime;
end;

{ TSingleAttackerEntry }

constructor TMapleMonster.TSingleAttackerEntry.Create(From: TMapleCharacter;
  CServ: TChannelServer);
begin
  FChrID := From.ID;
  FCServ := CServ;
  FDamage := 0;
  FLastAttackTime := GetTickCount;
end;

procedure TMapleMonster.TSingleAttackerEntry.AddDamage(From: TMapleCharacter; Damage: Integer;
  UpdateAttackTime: Boolean);
begin
  if FChrID = From.ID then
    Inc(FDamage, Damage)
  else
    raise EArgumentException.CreateFmt('Not the attacker of this entry: %d <> %d', [FChrID, From.ID]);

  if UpdateAttackTime then
    FLastAttackTime := GetTickCount;
end;

function TMapleMonster.TSingleAttackerEntry.GetAttackers: TList<TAttackingMapleCharacter>;
var
  Char: TMapleCharacter;
begin
  Result := TList<TAttackingMapleCharacter>.Create;
  Char := FCServ.Player[FChrID];
  if Char <> nil then
    Result.Add(TAttackingMapleCharacter.Create(Char, FLastAttackTime));
end;

procedure TMapleMonster.TSingleAttackerEntry.KilledMob(Mob: TMapleMonster;
  Map: TMapleMap; BaseExp: Integer; MostDamage: Boolean);
var
  Char: TMapleCharacter;
begin
  Char := FCServ.Player[FChrID];

  { This is quite poor in Delphi: Although this is a subclass of TMapleMonster,
    I can't access its methods & fields and therefore have to use an extra parameter }
  if (Char <> nil) and (Char.Map = Map) then
    Mob.GiveExpToCharacter(Char, BaseExp, 1, MostDamage);
end;

{ TOnePartyAttacker }

constructor TOnePartyAttacker.Create(Party: TMapleParty; ADamage: Integer);
begin
  LastKnownParty := Party;
  Damage := ADamage;
  LastAttackTime := GetTickCount;
end;

{ TMapleMonster.TPartyAttackerEntry }

constructor TMapleMonster.TPartyAttackerEntry.Create(APartyID: Integer;
  CServ: TChannelServer);
begin
  FPartyID := APartyID;
  FCServ := CServ;
  FAttackers := TDictionary<Integer, TOnePartyAttacker>.Create;
end;

destructor TMapleMonster.TPartyAttackerEntry.Destroy;
begin
  FAttackers.Free;

  inherited;
end;

procedure TMapleMonster.TPartyAttackerEntry.AddDamage(From: TMapleCharacter;
  Damage: Integer; UpdateAttackTime: Boolean);
var
  Attacker: TOnePartyAttacker;
begin
  if FAttackers.TryGetValue(From.ID, Attacker) then
  begin
    Attacker.Damage := Attacker.Damage + Damage;
    Attacker.LastKnownParty := From.Party;
    if UpdateAttackTime then
      Attacker.LastAttackTime := GetTickCount;
    FAttackers[From.ID] := Attacker;
  end
  else
  begin
    Attacker := TOnePartyAttacker.Create(From.Party, Damage);
    FAttackers.Add(From.ID, Attacker);
    if not UpdateAttackTime then
      Attacker.LastAttackTime := 0;
  end;
  Inc(FDamage, Damage);
end;

function TMapleMonster.TPartyAttackerEntry.GetAttackers: TList<TAttackingMapleCharacter>;
var
  ID: Integer;
  Char: TMapleCharacter;
begin
  Result := TList<TAttackingMapleCharacter>.Create;
  for ID in FAttackers.Keys do
  begin
    Char := FCServ.Player[ID];
    if Char <> nil then
      Result.Add(TAttackingMapleCharacter.Create(Char, TOnePartyAttacker(FAttackers[ID]).LastAttackTime));
  end;
end;

function TMapleMonster.TPartyAttackerEntry.ResolveAttackers: TDictionary<TMapleCharacter, TOnePartyAttacker>;
var
  ID: Integer;
  Char: TMapleCharacter;
begin
  Result := TDictionary<TMapleCharacter, TOnePartyAttacker>.Create;
  for ID in FAttackers.Keys do
  begin
    Char := FCServ.Player[ID];
    if Char <> nil then
      Result.Add(Char, FAttackers[ID]);
  end;
end;

procedure TMapleMonster.TPartyAttackerEntry.KilledMob(Mob: TMapleMonster;
  Map: TMapleMap; BaseExp: Integer; MostDamage: Boolean);
var
  Attackers: TDictionary<TMapleCharacter, TOnePartyAttacker>;
  Highest: TMapleCharacter;
  HighestDmg, iDamage, Exp: Integer;
  ExpMap: TDictionary<TMapleCharacter, Integer>;
  Char, PChr, ExpReceiver: TMapleCharacter;
  Party: TMapleParty;
  AveragePartyLevel, ExpBonus, InnerBaseExp, ExpFraction, ExpWeight, LevelMod: Double;
  ExpApplicable: TList<TMapleCharacter>;
  MPC: TMaplePartyCharacter;
begin
  Attackers := ResolveAttackers;
  Highest := nil;
  HighestDmg := 0;
  ExpMap := TDictionary<TMapleCharacter, Integer>.Create;

  for Char in Attackers.Keys do
  begin
    Party := TOnePartyAttacker(Attackers[Char]).LastKnownParty;
    AveragePartyLevel := 0;
    ExpApplicable := TList<TMapleCharacter>.Create;

    for MPC in Party do
      if (Char.Level - MPC.Level <= 5) or (Mob.Stats^.Level - MPC.Level <= 5) then
      begin
        PChr := FCServ.Player[MPC.ID];
        if (PChr <> nil) and (PChr.IsAlive) and (PChr.Map = Map) then
        begin
          ExpApplicable.Add(PChr);
          AveragePartyLevel := AveragePartyLevel + PChr.Level;
        end;
      end;

    ExpBonus := 1;
    if ExpApplicable.Count > 1 then
    begin
      ExpBonus := 1.1 + 0.05 * ExpApplicable.Count;
      AveragePartyLevel := AveragePartyLevel / ExpApplicable.Count;
    end;

    iDamage := TOnePartyAttacker(Attackers[Char]).Damage;
    if iDamage > HighestDmg then
    begin
      Highest := Char;
      HighestDmg := iDamage;
    end;

    InnerBaseExp := BaseExp * (iDamage / FDamage);
    ExpFraction := (InnerBaseExp * ExpBonus) / (ExpApplicable.Count + 1);

    for ExpReceiver in ExpApplicable do
    begin
      if not ExpMap.TryGetValue(ExpReceiver, Exp) then
        Exp := 0;

      if ExpReceiver = Char then
        ExpWeight := 2
      else
        ExpWeight := 1;
      LevelMod := ExpReceiver.Level / AveragePartyLevel;
      if (LevelMod > 1.0) or (FAttackers.ContainsKey(ExpReceiver.ID)) then
        LevelMod := 1;

      Inc(Exp, Round(ExpFraction * ExpWeight * LevelMod));
      ExpMap.AddOrSetValue(ExpReceiver, Exp);
    end;
  end;

  for Char in ExpMap.Keys do
    if MostDamage then
      Mob.GiveExpToCharacter(Char, ExpMap[Char], ExpMap.Count, Char = Highest)
    else
      Mob.GiveExpToCharacter(Char, ExpMap[Char], ExpMap.Count, False);
end;

end.
