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

unit BuffDataProvider;

interface

uses SysUtils, Generics.Collections, MapleItem, BuffStat;

const
  ByteQuantity = 16;
  EntryByteQuantity = 8;

type
  TAct = (actHeal, actHurt);

  TSkillValue = (svNone, svSpecialProc, svX, svY, svSpeed, svJump, svWAtk,
                 svWDef, svMAtk, svMDef, svAcc, svAvo, svProp, svMorph, svLv);

  TBuff = record
    Stat: TBuffStat;
    Value: TSkillValue;

    constructor Create(AStat: TBuffStat; AValue: TSkillValue);
  end;

  TBuffInfo = record
    Buff: TBuff;
    ItemVal: SmallInt;
    HasMapVal, HasMapEntry, UseVal, OnlyMap: Boolean;

    procedure Clear;
  end;

  TBuffMapInfo = record
    Buff: TBuff;
    UseVal: Boolean;

    constructor Create(ABuff: TBuff; AUseVal: Boolean = True);
  end;

  TBuffAct = record
    Act: TAct;
    Value: TSkillValue;
    Time: Integer;

    constructor Create(AAction: TAct; AValue: TSkillValue; ATime: Integer);
  end;

  TSkillInfo = record
    Player: TList<TBuffInfo>;
    Map: TList<TBuffMapInfo>;
    Act: TBuffAct;
    BAct: Boolean;

    constructor Create(APlayer: TBuffInfo; CreateMap: Boolean = False); overload;
    constructor Create(APlayer: TBuffInfo; AMap: TBuffMapInfo); overload;
  end;
  PSkillInfo = ^TSkillInfo;

  TBuffDataProvider = class
  private
    FSkillsInfo: TDictionary<Integer, PSkillInfo>;

    procedure LoadData;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddItemInfo(ItemID: Integer; const CI: PConsumeInfo);
    function GetSkillInfo(const SkillID: Integer): PSkillInfo;
    function IsBuff(const SkillID: Integer): Boolean;
  end;

var
  BuffDataProv: TBuffDataProvider;   // singleton

implementation

uses Skills;

{ TSkillInfo }

constructor TSkillInfo.Create(APlayer: TBuffInfo; CreateMap: Boolean = False);
begin
  BAct := False;

  if not CreateMap then
    Map := nil
  else
    Map := TList<TBuffMapInfo>.Create;

  Player := TList<TBuffInfo>.Create;
  Player.Add(APlayer);
end;

constructor TSkillInfo.Create(APlayer: TBuffInfo; AMap: TBuffMapInfo);
begin
  BAct := False;

  Player := TList<TBuffInfo>.Create;
  Map := TList<TBuffMapInfo>.Create;

  Player.Add(APlayer);
  Map.Add(AMap);
end;

{ TBuff }

constructor TBuff.Create(AStat: TBuffStat; AValue: TSkillValue);
begin
  Stat := AStat;
  Value := AValue;
end;

{ TBuffMapInfo }

constructor TBuffMapInfo.Create(ABuff: TBuff; AUseVal: Boolean = True);
begin
  Buff := ABuff;
  UseVal := AUseVal;
end;

{ TBuffInfo }

procedure TBuffInfo.Clear;
begin
  ItemVal := 0;
  HasMapVal := False;
  HasMapEntry := False;
  UseVal := False;
  OnlyMap := False;
end;

{ TBuffAct }

constructor TBuffAct.Create(AAction: TAct; AValue: TSkillValue; ATime: Integer);
begin
  Act := AAction;
  Value := AValue;
  Time := ATime;
end;

{ TBuffDataProvider }

constructor TBuffDataProvider.Create;
begin
  FSkillsInfo := TDictionary<Integer, PSkillInfo>.Create;
  LoadData;
end;

destructor TBuffDataProvider.Destroy;
var
  SI: PSkillInfo;
  Freed: TList<PSkillInfo>;
begin
  Freed := TList<PSkillInfo>.Create;

  for SI in FSkillsInfo.Values do
    if not Freed.Contains(SI) then
    begin
      SI^.Player.Free;
      if Assigned(SI^.Map) then
        SI^.Map.Free;

      Dispose(SI);

      Freed.Add(SI);
    end;

  Freed.Free;
  FSkillsInfo.Free;

  inherited;
end;

function TBuffDataProvider.GetSkillInfo(const SkillID: Integer): PSkillInfo;
begin
  Result := FSkillsInfo[SkillID];
end;

function TBuffDataProvider.IsBuff(const SkillID: Integer): Boolean;
begin
  Result := FSkillsInfo.ContainsKey(SkillID);
end;

procedure TBuffDataProvider.LoadData;
var
  Player: TBuffInfo;
  SI: PSkillInfo;
begin
  Player.Clear;

  // Boosters
  Player.Buff := TBuff.Create(BOOSTER, svX);
  New(SI);
  SI^ := TSkillInfo.Create(Player);
  FSkillsInfo.Add(Fighter.SwordBooster, SI);
  FSkillsInfo.Add(Fighter.AxeBooster, SI);
  FSkillsInfo.Add(Page.SwordBooster, SI);
  FSkillsInfo.Add(Page.BwBooster, SI);
  FSkillsInfo.Add(Spearman.SpearBooster, SI);
  FSkillsInfo.Add(Spearman.PolearmBooster, SI);
  FSkillsInfo.Add(FPMage.SpellBooster, SI);
  FSkillsInfo.Add(ILMage.SpellBooster, SI);
  FSkillsInfo.Add(Hunter.BowBooster, SI);
  FSkillsInfo.Add(Crossbowman.CrossbowBooster, SI);
  FSkillsInfo.Add(Assassin.ClawBooster, SI);
  FSkillsInfo.Add(Bandit.DaggerBooster, SI);
  FSkillsInfo.Add(Brawler.KnucklerBooster, SI);
  FSkillsInfo.Add(Gunslinger.GunBooster, SI);
  FSkillsInfo.Add(DawnWarrior.SwordBooster, SI);
  FSkillsInfo.Add(BlazeWizard.SpellBooster, SI);
  FSkillsInfo.Add(WindArcher.BowBooster, SI);
  FSkillsInfo.Add(NightWalker.ClawBooster, SI);
  FSkillsInfo.Add(ThunderBreaker.KnucklerBooster, SI);
  FSkillsInfo.Add(Aran.PolearmBooster, SI);
  FSkillsInfo.Add(WildHunter.CrossbowBooster, SI);

  // Magic Guard
  Player.Buff := TBuff.Create(MAGIC_GUARD, svX);
  New(SI);
  SI^ := TSkillInfo.Create(Player);
  FSkillsInfo.Add(Magician.MagicGuard, SI);
  FSkillsInfo.Add(BlazeWizard.MagicGuard, SI);

  // Magic Armor, Iron Body
  Player.Buff := TBuff.Create(WDEF, svWDef);
  New(SI);
  SI^ := TSkillInfo.Create(Player);
  FSkillsInfo.Add(Magician.MagicArmor, SI);
  FSkillsInfo.Add(Warrior.IronBody, SI);
  FSkillsInfo.Add(BlazeWizard.MagicArmor, SI);
  FSkillsInfo.Add(DawnWarrior.IronBody, SI);

  // Focus
  Player.Buff := TBuff.Create(ACC, svAcc);
  New(SI);
  SI^ := TSkillInfo.Create(Player);
  FSkillsInfo.Add(Bowman.Focus, SI);
  FSkillsInfo.Add(WindArcher.Focus, SI);
  Player.Buff := TBuff.Create(AVOID, svAvo);
  SI^.Player.Add(Player);

  // Rage
  Player.Buff := TBuff.Create(WATK, svWAtk);
  New(SI);
  SI^ := TSkillInfo.Create(Player);
  FSkillsInfo.Add(Fighter.Rage, SI);
  FSkillsInfo.Add(DawnWarrior.Rage, SI);
  Player.Buff := TBuff.Create(WDEF, svWDef);
  SI^.Player.Add(Player);

  // Power Guard
  Player.Buff := TBuff.Create(POWER_GUARD, svX);
  New(SI);
  SI^ := TSkillInfo.Create(Player);
  FSkillsInfo.Add(Fighter.PowerGuard, SI);
  FSkillsInfo.Add(Page.PowerGuard, SI);

  // Iron Will
  Player.Buff := TBuff.Create(WDEF, svWDef);
  New(SI);
  SI^ := TSkillInfo.Create(Player);
  FSkillsInfo.Add(Spearman.IronWill, SI);
  Player.Buff := TBuff.Create(MDEF, svMDef);
  SI^.Player.Add(Player);

  // Hyper Body
  Player.Buff := TBuff.Create(HYPERBODY_HP, svX);
  New(SI);
  SI^ := TSkillInfo.Create(Player);
  FSkillsInfo.Add(Spearman.HyperBody, SI);
  Player.Buff := TBuff.Create(HYPERBODY_MP, svY);
  SI^.Player.Add(Player);

  // Meditation
	Player.Buff := TBuff.Create(MATK, svMAtk);
  New(SI);
  SI^ := TSkillInfo.Create(Player);
	FSkillsInfo.Add(FPWizard.Meditation, SI);
	FSkillsInfo.Add(ILWizard.Meditation, SI);
	FSkillsInfo.Add(BlazeWizard.Meditation, SI);

  // Invincible
  Player.Buff := TBuff.Create(INVINCIBLE, svX);
  New(SI);
  SI^ := TSkillInfo.Create(Player);
  FSkillsInfo.Add(Cleric.Invincible, SI);

  // Bless
  Player.Buff := TBuff.Create(WDEF, svWDef);
  New(SI);
  SI^ := TSkillInfo.Create(Player);
  FSkillsInfo.Add(Cleric.Bless, SI);
  Player.Buff := TBuff.Create(MDEF, svMDef);
  SI^.Player.Add(Player);
  Player.Buff := TBuff.Create(ACC, svAcc);
  SI^.Player.Add(Player);
  Player.Buff := TBuff.Create(AVOID, svAvo);
  SI^.Player.Add(Player);

  // Begin map buffs
  // Nimble feet
  Player.Buff := TBuff.Create(SPEED, svSpeed);
  Player.HasMapVal := True;
  Player.HasMapEntry := True;
  New(SI);
  SI^ := TSkillInfo.Create(Player, TBuffMapInfo.Create(Player.Buff));
  FSkillsInfo.Add(Beginner.NimbleFeet, SI);
  FSkillsInfo.Add(Noblesse.NimbleFeet, SI);
  FSkillsInfo.Add(Legend.AgileBody, SI);

  // Infiltrate
  Player.Buff := TBuff.Create(SPEED, svSpeed);
  Player.HasMapVal := True;
  Player.HasMapEntry := True;
  New(SI);
  SI^ := TSkillInfo.Create(Player, TBuffMapInfo.Create(Player.Buff));
  FSkillsInfo.Add(Citizen.Infiltrate, SI);
  Player.Buff := TBuff.Create(INFILTRATE, svX);
  SI^.Player.Add(Player);
  SI^.Map.Add(TBuffMapInfo.Create(Player.Buff, False));

  // Charges
  Player.Buff := TBuff.Create(MATK, svMAtk);
  Player.HasMapVal := False;
  Player.HasMapEntry := False;
  New(SI);
  SI^ := TSkillInfo.Create(Player, True);
  Player.Buff := TBuff.Create(WK_CHARGE, svX);
  Player.HasMapVal := True;
  Player.HasMapEntry := True;
  SI^.Player.Add(Player);
  SI^.Map.Add(TBuffMapInfo.Create(Player.Buff));
  FSkillsInfo.Add(WhiteKnight.FireChargeSword, SI);
  FSkillsInfo.Add(WhiteKnight.FireChargeBW, SI);
  FSkillsInfo.Add(WhiteKnight.IceChargeSword, SI);
  FSkillsInfo.Add(WhiteKnight.IceChargeBW, SI);
  FSkillsInfo.Add(ThunderBreaker.LightningCharge, SI);

  Player.Clear;

  // Lightning Charge: Stacks with others
 (* Player.Buff := TBuff.Create(MATK, svMAtk);
  New(SI);
  SI^ := TSkillInfo.Create(Player, True);
  Player.Buff := TBuff.Create($10, Byte11, svX);
  SI^.Player.Add(Player);
  // 3rd party gets $40 (same as other charges)
  Player.Buff := TBuff.Create(WK_CHARGE, svX);
  Player.HasMapVal := True;
  Player.HasMapEntry := True;
  Player.OnlyMap := True;
  SI^.Player.Add(Player);
  SI^.Map.Add(TBuffMapInfo.Create(Player.Buff));
  FSkillsInfo.Add(WhiteKnight.LightningChargeSword, SI);
  FSkillsInfo.Add(WhiteKnight.LightningChargeBW, SI);    *)

  Player.Clear;

  // Dash
  Player.Buff := TBuff.Create(DASH_X, svX);
  Player.HasMapVal := True;
  New(SI);
  SI^ := TSkillInfo.Create(Player, TBuffMapInfo.Create(Player.Buff));
  FSkillsInfo.Add(Pirate.Dash, SI);
  FSkillsInfo.Add(ThunderBreaker.Dash, SI);
  Player.Buff := TBuff.Create(DASH_Y, svY);
  Player.HasMapVal := True;
  SI^.Player.Add(Player);
  SI^.Map.Add(TBuffMapInfo.Create(Player.Buff));

  // Haste
  Player.Buff := TBuff.Create(SPEED, svSpeed);
  Player.HasMapVal := True;
  Player.HasMapEntry := True;
  New(SI);
  SI^ := TSkillInfo.Create(Player, TBuffMapInfo.Create(Player.Buff));
  FSkillsInfo.Add(Assassin.Haste, SI);
  FSkillsInfo.Add(Bandit.Haste, SI);
  FSkillsInfo.Add(GM.Haste, SI);
  FSkillsInfo.Add(SuperGM.Haste, SI);
  FSkillsInfo.Add(NightWalker.Haste, SI);
  Player.Buff := TBuff.Create(JUMP, svJump);
  Player.HasMapVal := False;
  Player.HasMapEntry := False;
  SI^.Player.Add(Player);

  // Dark Sight
  Player.Buff := TBuff.Create(SPEED, svSpeed);
  Player.HasMapVal := True;
  Player.HasMapEntry := True;
  New(SI);
  SI^ := TSkillInfo.Create(Player, TBuffMapInfo.Create(Player.Buff));
  FSkillsInfo.Add(Thief.DarkSight, SI);
  FSkillsInfo.Add(NightWalker.DarkSight, SI);
  Player.Buff := TBuff.Create(DARK_SIGHT, svX);
  Player.HasMapVal := True;
  Player.HasMapEntry := True;
  SI^.Player.Add(Player);
  SI^.Map.Add(TBuffMapInfo.Create(Player.Buff, False));

  // Soul Arrow
  Player.Buff := TBuff.Create(SOUL_ARROW, svX);
  Player.HasMapVal := True;
  Player.HasMapEntry := True;
  New(SI);
  SI^ := TSkillInfo.Create(Player, TBuffMapInfo.Create(Player.Buff, False));
  FSkillsInfo.Add(Hunter.SoulArrow, SI);
  FSkillsInfo.Add(Crossbowman.SoulArrow, SI);
  FSkillsInfo.Add(WindArcher.SoulArrow, SI);
  FSkillsInfo.Add(WildHunter.SoulArrow, SI);

  // Oak Barrel
  Player.Buff := TBuff.Create(MORPH, svMorph);
  Player.HasMapVal := True;
  Player.HasMapEntry := True;
  New(SI);
  SI^ := TSkillInfo.Create(Player, TBuffMapInfo.Create(Player.Buff));
  FSkillsInfo.Add(Brawler.OakBarrel, SI);

  // Energy Charge
  Player.Buff := TBuff.Create(ENERGY_CHARGE, svSpecialProc);
  Player.HasMapVal := True;
  Player.HasMapEntry := True;
  New(SI);
  SI^ := TSkillInfo.Create(Player, TBuffMapInfo.Create(Player.Buff, False));
  FSkillsInfo.Add(Marauder.EnergyCharge, SI);
  FSkillsInfo.Add(ThunderBreaker.EnergyCharge, SI);

  // Shadow Partner
  Player.Buff := TBuff.Create(SHADOW_PARTNER, svSpecialProc);
  Player.HasMapVal := True;
  Player.HasMapEntry := True;
  New(SI);
  SI^ := TSkillInfo.Create(Player, TBuffMapInfo.Create(Player.Buff, False));
  FSkillsInfo.Add(Hermit.ShadowPartner, SI);
  FSkillsInfo.Add(ChiefBandit.ShadowPartner, SI);
  FSkillsInfo.Add(NightWalker.ShadowPartner, SI);

  // Combo Attack
  Player.Buff := TBuff.Create(COMBO, svSpecialProc);
  Player.HasMapVal := True;
  Player.HasMapEntry := True;
  New(SI);
  SI^ := TSkillInfo.Create(Player, TBuffMapInfo.Create(Player.Buff));
  FSkillsInfo.Add(Crusader.ComboAttack, SI);
  FSkillsInfo.Add(DawnWarrior.ComboAttack, SI);
  // End map buffs

  Player.Clear;

  // Begin mount buffs
  Player.Buff := TBuff.Create(MONSTER_RIDING, svSpecialProc);
  Player.HasMapVal := True;
  New(SI);
  SI^ := TSkillInfo.Create(Player, TBuffMapInfo.Create(Player.Buff, False));
  FSkillsInfo.Add(WildHunter.JaguarRider, SI);
  // End mount buffs

  Player.Clear;

  // Begin act buffs
  // Recovery
  Player.Buff := TBuff.Create(RECOVERY, svX);
  New(SI);
  SI^ := TSkillInfo.Create(Player);
  SI^.Act := TBuffAct.Create(actHeal, svX, 4900);
  SI^.BAct := True;
  FSkillsInfo.Add(Beginner.Recovery, SI);
  FSkillsInfo.Add(Noblesse.Recovery, SI);
  FSkillsInfo.Add(Legend.Recovery, SI);

  // Dragon Blood
 (* Player.Buff := TBuff.Create(WATK, svWAtk);
  New(SI);
  SI^ := TSkillInfo.Create(Player);
  Player.Buff := TBuff.Create($80, Byte3, svLv);
  SI^.Player.Add(Player);
  SI^.Act := TBuffAct.Create(actHurt, svX, 4000);
  SI^.BAct := True;
  FSkillsInfo.Add(DragonKnight.DragonBlood, SI);   *)
  // End act buffs
end;

procedure TBuffDataProvider.AddItemInfo(ItemID: Integer; const CI: PConsumeInfo);
var
  Stats: TList<TBuffStat>;
  Values: TList<Word>;
  Buff: TBuff;
  Player: TBuffInfo;
  Map: TBuffMapInfo;
  SI: PSkillInfo;
  i: Integer;
begin
  Stats := TList<TBuffStat>.Create;
  Values := TList<Word>.Create;

  if CI^.WAtk > 0 then
  begin
    Stats.Add(WATK);
    Values.Add(CI^.WAtk);
  end;
  if CI^.WDef > 0 then
  begin
    Stats.Add(WDEF);
    Values.Add(CI^.WDef);
  end;
  if CI^.MAtk > 0 then
  begin
    Stats.Add(MATK);
    Values.Add(CI^.MAtk);
  end;
  if CI^.MDef > 0 then
  begin
    Stats.Add(MDEF);
    Values.Add(CI^.MDef);
  end;
  if CI^.Acc > 0 then
  begin
    Stats.Add(ACC);
    Values.Add(CI^.Acc);
  end;
  if CI^.Avoid > 0 then
  begin
    Stats.Add(AVOID);
    Values.Add(CI^.Avoid);
  end;
  if CI^.Speed > 0 then
  begin
    Stats.Add(SPEED);
    Values.Add(CI^.Speed);
  end;
  if CI^.Jump > 0 then
  begin
    Stats.Add(JUMP);
    Values.Add(CI^.Jump);
  end;

  // xxx Morph

  if Stats.Count > 0 then
  begin
    ItemID := -ItemID;

    if IsBuff(ItemID) then
    begin
      PSkillInfo(FSkillsInfo[ItemID])^.Player.Free;
      if Assigned(PSkillInfo(FSkillsInfo[ItemID])^.Map) then
        PSkillInfo(FSkillsInfo[ItemID])^.Map.Free;
      Dispose(FSkillsInfo[ItemID]);
      FSkillsInfo.Remove(ItemID);
    end;

    for i := 0 to Stats.Count - 1 do
    begin
      Player.Clear;
      Buff.Stat := Stats[i];
      Buff.Value := svNone;
      Player.Buff := Buff;
      Player.ItemVal := Values[i];
      Player.UseVal := True;

      if i = 0 then
        New(SI)
      else
        SI := nil;

      if (Buff.Stat = SPEED) or (Buff.Stat = MORPH) then
      begin
        Player.HasMapVal := True;
        Player.HasMapEntry := True;
        Map.Buff := Buff;
        Map.UseVal := True;
        if i = 0 then
          SI^ := TSkillInfo.Create(Player, Map)
        else
        begin
          PSkillInfo(FSkillsInfo[ItemID])^.Player.Add(Player);
          if PSkillInfo(FSkillsInfo[ItemID])^.Map = nil then
            PSkillInfo(FSkillsInfo[ItemID])^.Map := TList<TBuffMapInfo>.Create;
          PSkillInfo(FSkillsInfo[ItemID])^.Map.Add(Map);
        end;
      end
      else
        if i = 0 then
          SI^ := TSkillInfo.Create(Player)
        else
          PSkillInfo(FSkillsInfo[ItemID])^.Player.Add(Player);

      if i = 0 then
        FSkillsInfo.Add(ItemID, SI);
    end;
  end;
end;

end.
