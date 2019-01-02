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

unit PlayerBuffs;

interface

uses Windows, Generics.Collections, MapleMapObject, BuffDataProvider, Scheduler,
     SkillDataProvider, GameLogic, BuffStat;

type
  TActiveBuff = record
    Stat: TBuffStat;
    Vals: TList<SmallInt>;
    HasMapBuff: Boolean;

    constructor Create(Dummy: Integer);
  end;
  PActiveBuff = ^TActiveBuff;

  // I have to declare them here because of unitfuck
  TActiveMapBuff = record
    Stats: TList<TBuffStat>;
    Values: TList<SmallInt>;
    UseVals: TList<Boolean>;
    Debuff: Boolean;
    Stat: TBuffStat;

    constructor Create(Dummy: Integer);
  end;
  PActiveMapBuff = ^TActiveMapBuff;

  TMapEntryVals = record
    Use: Boolean;
    Debuff: Boolean;
    Val, Skill: Smallint;
  end;

  TMapEntryBuffs = record
    Stat: TBuffStat;
    Values: TDictionary<TBuffStat, TMapEntryVals>;

    constructor Create(Dummy: Integer);
  end;
  PMapEntryBuffs = ^TMapEntryBuffs;

  TActiveBuffsByStat = TDictionary<TBuffStat, Integer>;

  TPlayerActiveAct = class;
  TPlayerActiveBuff = class;

  TPlayerActiveBuffs = class
  private
    FPlayer: TAnimatedMapObject;
    FBuffs: TDictionary<Integer, TPlayerActiveBuff>;
    FActiveBuffsByStat: TActiveBuffsByStat;
    FMapBuffs: PMapEntryBuffs;
    FActiveLevels: TDictionary<Integer, Byte>;
    FSkillActs: TDictionary<Integer, TPlayerActiveAct>;
    FCombo, FEnergyCharge, FCharge: Integer;
    FEnergyChargeTimer: THandle;
  public
    constructor Create(Player: TAnimatedMapObject);
    destructor Destroy; override;

    procedure AddBuff(SkillID, Time: Integer);
    procedure AddBuffInfo(SkillID: Integer; Buffs: TList<TBuff>);
    procedure AddMapEntryBuffInfo(Buff: PActiveMapBuff);

    procedure DeleteMapEntryBuffInfo(Buff: PActiveMapBuff);

    procedure RemoveBuff(SkillID: Integer; FromTimer: Boolean);
    function RemoveBuffInfo(SkillID: Integer; Buffs: TList<TBuff>): TBuffStat;

    procedure AddAct(SkillID: Integer; Act: TAct; Value: SmallInt; Time: Integer);
    procedure RemoveAct(SkillID: Integer);

    function GetActiveSkillLevel(const SkillID: Integer): Byte;
    function GetActiveSkillInfo(const SkillID: Integer): PSkillLevelInfo;
    function IsActive(const SkillID: Integer): Boolean;
    procedure SetActiveSkillLevel(SkillID: Integer; Level: Byte);

    procedure AddCombo;
    procedure SetCombo(Value: Integer; SendPacket: Boolean);

    procedure IncreaseEnergyChargeLevel(Targets: Byte);
    procedure DecreaseEnergyChargeLevel;

    function GetComboAttack: Integer;
    function GetEnergyCharge: Integer;
    function GetMPEater: Integer;
    function HasShadowPartner: Boolean;
    function HasSoulArrow: Boolean;

    property Charge: Integer read FCharge write FCharge;
    property Combo: Integer read FCombo;
    property EnergyCharge: Integer read FEnergyCharge write FEnergyCharge;
    property MapEntryBuffs: PMapEntryBuffs read FMapBuffs;
    property Player: TAnimatedMapObject read FPlayer;
  end;

  // This class provides methods that can be called by a timer (It can't handle methods with parameters)
  TTimerClass = class
  private
    FBuffs: TPlayerActiveBuffs;
    FSkill: Integer;
    FTimer: THandle;
    FStartTime: Cardinal;
  public
    constructor Create(const Buffs: TPlayerActiveBuffs; SkillID: Integer);

    property Timer: THandle read FTimer write FTimer;
    property StartTime: Cardinal read FStartTime write FStartTime;
  end;

  TPlayerActiveBuff = class(TTimerClass)
  public
    procedure StopSkill;
  end;

  TPlayerActiveAct = class(TTimerClass)
  private
    FAct: TAct;
    FValue: SmallInt;
  public
    procedure RunAct;

    property Act: TAct read FAct write FAct;
    property Value: SmallInt read FValue write FValue;
  end;

procedure FreeActiveBuff(var AB: PActiveBuff);
procedure FreeActiveMapBuff(var AMB: PActiveMapBuff);

implementation

uses Main, MapleClient, MapleCharacter, MaplePacketCreator, Buffs, Skills;

{ TActiveBuff }

constructor TActiveBuff.Create(Dummy: Integer);
begin
  Stat := TBuffStat.Create64(1, 0);
  Vals := TList<Smallint>.Create;
  HasMapBuff := False;
end;

procedure FreeActiveBuff(var AB: PActiveBuff);
begin
  AB^.Vals.Free;
  Dispose(AB);
end;

{ TActiveMapBuff }

constructor TActiveMapBuff.Create(Dummy: Integer);
begin
  Stat := TBuffStat.Create64(1, 0); // Empty

  Stats := TList<TBuffStat>.Create;
  Values := TList<SmallInt>.Create;
  UseVals := TList<Boolean>.Create;
  Debuff := False;
end;

procedure FreeActiveMapBuff(var AMB: PActiveMapBuff);
begin
  AMB^.Stats.Free;
  AMB^.Values.Free;
  AMB^.UseVals.Free;
  Dispose(AMB);
end;

{ TMapEntryBuffs }

constructor TMapEntryBuffs.Create(Dummy: Integer);
begin
  Stat := TBuffStat.Create64(1, 0);  // Empty
  Values := TDictionary<TBuffStat, TMapEntryVals>.Create;
end;

{ TTimerClass }

constructor TTimerClass.Create(const Buffs: TPlayerActiveBuffs;
  SkillID: Integer);
begin
  FBuffs := Buffs;
  FSkill := SkillID;
  FStartTime := GetTickCount;
end;

{ TPlayerActiveBuff }

procedure TPlayerActiveBuff.StopSkill;
begin
  // Also change TCancelBuffHandler when changing this!!
  if FBuffs.GetActiveSkillLevel(FSkill) = 0 then
    Exit;

  FBuffs.RemoveBuff(FSkill, True);

  // EndBuff() is declared in the class helper "TBuffs"
  TMapleCharacter(FBuffs.Player).EndBuff(FSkill);
  Free;
end;

{ TPlayerActiveAct }

procedure TPlayerActiveAct.RunAct;
var
  Player: TMapleCharacter;
begin
  Player := TMapleCharacter(FBuffs.Player);
  case FAct of
    actHeal:
    begin
      if (Player.HP < Player.CurrentMaxHP) and Player.IsAlive then
      begin
        Player.AddHP(Value);
        TMapleClient(Player.Client).Write(ShowHPHealBySkill(Value));
      end;
    end;

    actHurt:
    begin
      if Player.HP - Value > 1 then
      begin
        Player.AddHP(-Value);
        TMapleClient(Player.Client).Write(ShowOwnBuffEffect(Player, FSkill, FBuffs.GetActiveSkillLevel(FSkill), 5));
        // xxx IsHiding
        Player.Map.BroadcastMessage(ShowSkill(Player, FSkill, FBuffs.GetActiveSkillLevel(FSkill), 5));
      end
      else
      begin
        FBuffs.RemoveBuff(FSkill, False);
        Player.EndBuff(FSkill);
      end;
    end;
  end;
end;

{ TPlayerActiveBuffs }

constructor TPlayerActiveBuffs.Create(Player: TAnimatedMapObject);
begin
  FPlayer := Player;
  FBuffs := TDictionary<Integer, TPlayerActiveBuff>.Create;
  FActiveBuffsByStat := TActiveBuffsByStat.Create;
  New(FMapBuffs);
  FMapBuffs^ := TMapEntryBuffs.Create(0);
  FActiveLevels := TDictionary<Integer, Byte>.Create;
  FSkillActs := TDictionary<Integer, TPlayerActiveAct>.Create;

  FCombo := 0;
  FEnergyCharge := 0;
  FEnergyChargeTimer := 0;
end;

destructor TPlayerActiveBuffs.Destroy;
var
  i: Integer;
  PAB: TPlayerActiveBuff;
begin
  for PAB in FBuffs.Values do
  begin
    // Cancel all active buffs, so there won't be a crash when a client disconnects
    // and the timer triggers
    if Assigned(PAB) then
    begin
      Sched.CancelSchedule(PAB.Timer);
      PAB.Free;
    end;
  end;
  FBuffs.Free;

  FActiveBuffsByStat.Free;
  FActiveLevels.Free;

  for i in FSkillActs.Keys do
    RemoveAct(i);  // also so that the timer is cancelled
  FSkillActs.Free;

  FMapBuffs^.Values.Free;
  Dispose(FMapBuffs);

  inherited;
end;

procedure TPlayerActiveBuffs.AddBuff(SkillID, Time: Integer);
var
  SkillExpire: Integer;
  PAB: TPlayerActiveBuff;
begin
  // Only use a timer when there is a time
  if Time > 0 then
  begin
    SkillExpire := Time * 1000;
    PAB := TPlayerActiveBuff.Create(Self, SkillID);
    PAB.Timer := Sched.AddSchedule(SkillExpire, PAB.StopSkill);
  end
  else
    PAB := nil;

  FBuffs.Add(SkillID, PAB);
end;

procedure TPlayerActiveBuffs.RemoveBuff(SkillID: Integer; FromTimer: Boolean);
begin
  if (not FromTimer) and (FBuffs.ContainsKey(SkillID)) and (Assigned(FBuffs[SkillID])) then
  begin
    Sched.CancelSchedule(TPlayerActiveBuff(FBuffs[SkillID]).Timer);
    FBuffs[SkillID].Free;
  end;

  RemoveAct(SkillID);
  FBuffs.Remove(SkillID);
end;

procedure TPlayerActiveBuffs.AddAct(SkillID: Integer; Act: TAct;
  Value: SmallInt; Time: Integer);
var
  PAA: TPlayerActiveAct;
begin
  PAA := TPlayerActiveAct.Create(Self, SkillID);
  PAA.Act := Act;
  PAA.Value := Value;
  PAA.Timer := Sched.AddRepeatedJob(Time, PAA.RunAct);
  FSkillActs.Add(SkillID, PAA);
end;

procedure TPlayerActiveBuffs.RemoveAct(SkillID: Integer);
var
  PAA: TPlayerActiveAct;
begin
  if not FSkillActs.ContainsKey(SkillID) then
    Exit;

  PAA := FSkillActs[SkillID];
  Sched.RemoveRepeatedJob(PAA.Timer);
  PAA.Free;
  FSkillActs.Remove(SkillID);
end;

procedure TPlayerActiveBuffs.AddBuffInfo(SkillID: Integer; Buffs: TList<TBuff>);
var
  Buff: TBuff;
begin
  for Buff in Buffs do
    FActiveBuffsByStat.AddOrSetValue(Buff.Stat, SkillID);
end;

function TPlayerActiveBuffs.RemoveBuffInfo(SkillID: Integer;
  Buffs: TList<TBuff>): TBuffStat;
var
  Buff: TBuff;
begin
  Result := TBuffStat.Create64(1, 0);

  for Buff in Buffs do
    if FActiveBuffsByStat.ContainsKey(Buff.Stat) and (FActiveBuffsByStat[Buff.Stat] = SkillID) then
      Result := Result or Buff.Stat;

  for Buff in Buffs do
    if FActiveBuffsByStat.ContainsKey(Buff.Stat) and (FActiveBuffsByStat[Buff.Stat] = SkillID) then
      FActiveBuffsByStat.Remove(Buff.Stat);
end;

procedure TPlayerActiveBuffs.AddMapEntryBuffInfo(Buff: PActiveMapBuff);
var
  i, Vals: Integer;
  Val: TMapEntryVals;
begin
  Vals := 0;
  for i := 0 to Buff.Stats.Count - 1 do
  begin
    if not (FMapBuffs.Stat and Buff.Stats[i]) then
      FMapBuffs.Stat := FMapBuffs.Stat or Buff.Stats[i];

    Val.Use := Buff^.UseVals[i];

    if Val.Use then
      if Buff^.Debuff then
      begin
        Val.Debuff := True;
        Val.Skill := Buff^.Values[Vals];
        Inc(Vals);
        Val.Val := Buff^.Values[Vals];
        Inc(Vals);
      end
      else
      begin
        Val.Debuff := False;
        Val.Val := Buff^.Values[Vals];
        Inc(Vals);
      end;

    if not FMapBuffs^.Values.ContainsKey(Buff^.Stats[i]) then
      FMapBuffs^.Values.AddOrSetValue(Buff^.Stats[i], Val);
  end;
end;

procedure TPlayerActiveBuffs.DeleteMapEntryBuffInfo(Buff: PActiveMapBuff);
var
  i: Integer;
begin
  for i := 0 to Buff.Stats.Count - 1 do
  begin
    FMapBuffs.Stat := FMapBuffs.Stat xor Buff.Stats[i];
    FMapBuffs.Values.Remove(Buff.Stats[i]);
  end;
end;

function TPlayerActiveBuffs.GetActiveSkillLevel(const SkillID: Integer): Byte;
begin
  if FActiveLevels.ContainsKey(SkillID) then
    Result := FActiveLevels[SkillID]
  else
    Result := 0;
end;

function TPlayerActiveBuffs.GetComboAttack: Integer;
begin
  case TMapleCharacter(FPlayer).Job of
    mjCrusader, mjHero: Result := Crusader.ComboAttack;
    mjDawnWarrior3, mjDawnWarrior4: Result := DawnWarrior.ComboAttack;
    else Result := 0;
  end;
end;

function TPlayerActiveBuffs.GetEnergyCharge: Integer;
begin
  case TMapleCharacter(FPlayer).Job of
    mjMarauder, mjBuccaneer: Result := Marauder.EnergyCharge;
    mjThunderBreaker3, mjThunderBreaker4: Result := ThunderBreaker.EnergyCharge;
    else Result := 0;
  end;
end;

function TPlayerActiveBuffs.GetMPEater: Integer;
begin
  case TMapleCharacter(FPlayer).Job of
    mjFPWizard, mjFPMage, mjFPArchMage: Result := FPWizard.MPEater;
    mjILWizard, mjILMage, mjILArchMage: Result := ILWizard.MPEater;
    mjCleric, mjPriest, mjBishop: Result := Cleric.MPEater;
    else Result := 0;
  end;
end;

function TPlayerActiveBuffs.HasShadowPartner: Boolean;
begin
  Result := IsActive(Hermit.ShadowPartner) or IsActive(ChiefBandit.ShadowPartner) or
            IsActive(NightWalker.ShadowPartner);
end;

function TPlayerActiveBuffs.HasSoulArrow: Boolean;
begin
  Result := IsActive(Hunter.SoulArrow) or IsActive(Crossbowman.SoulArrow) or
            IsActive(WindArcher.SoulArrow) or IsActive(WildHunter.SoulArrow);
end;

function TPlayerActiveBuffs.GetActiveSkillInfo(
  const SkillID: Integer): PSkillLevelInfo;
var
  Lv: Byte;
begin
  Lv := GetActiveSkillLevel(SkillID);
  if Lv > 0 then
    Result := SkillDataProv.GetPlayerSkill(SkillID).Effects[Lv]
  else
    Result := nil;
end;

function TPlayerActiveBuffs.IsActive(const SkillID: Integer): Boolean;
begin
  Result := GetActiveSkillLevel(SkillID) > 0;
end;

procedure TPlayerActiveBuffs.SetActiveSkillLevel(SkillID: Integer; Level: Byte);
begin
  FActiveLevels.AddOrSetValue(SkillID, Level);
end;

procedure TPlayerActiveBuffs.AddCombo;
var
  CA: PSkillLevelInfo;
begin
  CA := GetActiveSkillInfo(GetComboAttack);
  if (CA <> nil) and (FCombo < CA.X) then
  begin
    Inc(FCombo);
    SetCombo(FCombo, True);
  end;
end;

procedure TPlayerActiveBuffs.SetCombo(Value: Integer; SendPacket: Boolean);
var
  SID, TimeLeft, Level: Integer;
  PSkill: PActiveBuff;
  MSkill: PActiveMapBuff;
begin
  FCombo := Value;
  if SendPacket then
  begin
    SID := GetComboAttack;
    TimeLeft := GetActiveSkillInfo(SID).Time - (GetTickCount - TPlayerActiveBuff(FBuffs[SID]).StartTime) div 1000;
    Level := GetActiveSkillLevel(SID);
    PSkill := TMapleCharacter(FPlayer).ParseBuffInfo(SID, Level);
    MSkill := TMapleCharacter(FPlayer).ParseBuffMapInfo(SID, Level, False);
    MaplePacketCreator.UseBuff(TMapleClient(TMapleCharacter(FPlayer).Client), SID, TimeLeft, PSkill, MSkill, 0);
  end;
end;

procedure TPlayerActiveBuffs.IncreaseEnergyChargeLevel(Targets: Byte);
var
  Skill: TSkill;
begin
  if (FEnergyCharge < 10000) and (Targets > 0) then
  begin
    if FEnergyChargeTimer > 0 then
      Sched.CancelSchedule(FEnergyChargeTimer);
    FEnergyChargeTimer := Sched.AddSchedule(10000, DecreaseEnergyChargeLevel);

    Skill := SkillDataProv.GetPlayerSkill(GetEnergyCharge);
    Inc(FEnergyCharge, PSkillLevelInfo(Skill.Effects[TMapleCharacter(FPlayer).GetSkillLevel(Skill)]).X * Targets);
    if FEnergyCharge >= 10000 then
    begin
      FEnergyCharge := 10000;
      Sched.CancelSchedule(FEnergyChargeTimer);
    end;

    TMapleCharacter(Player).AddBuff(Skill.ID, TMapleCharacter(FPlayer).GetSkillLevel(Skill), 0);
  end;
end;

procedure TPlayerActiveBuffs.DecreaseEnergyChargeLevel;
var
  Skill: TSkill;
begin
  Dec(FEnergyCharge, 200);
  if FEnergyCharge <= 0 then
    FEnergyCharge := 0
  else
    FEnergyChargeTimer := Sched.AddSchedule(10000, DecreaseEnergyChargeLevel);

  Skill := SkillDataProv.GetPlayerSkill(GetEnergyCharge);
  TMapleCharacter(Player).AddBuff(Skill.ID, TMapleCharacter(FPlayer).GetSkillLevel(Skill), 0);
end;

end.
