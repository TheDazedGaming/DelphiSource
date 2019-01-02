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

unit Skills;

interface

uses SysUtils, MapleCharacter, SkillDataProvider, MTRand, GameLogic;

type
  Beginner = class
  const
    BlessingOfTheFairy = 12;
    ThreeSnails = 1000;
    Recovery = 1001;
    NimbleFeet = 1002;
    LegendarySpirit = 1003;
    MonsterRider = 1004;
  end;

  Warrior = class
  const
    ImprovedHPRecovery = 1000000;
    ImprovedMaxHPIncrease = 1000001;
    HPBoost = 1000006;
    IronBody = 1001003;
  end;

  Fighter = class
  const
  	SwordMastery = 1100000;
    AxeMastery = 1100001;
    SwordBooster = 1101004;
   	AxeBooster = 1101005;
    Rage = 1101006;
	 	PowerGuard = 1101007;
  end;

  Page = class
  const
  	SwordMastery = 1200000;
  	BwMastery = 1200001;
    SwordBooster = 1201004;
    BwBooster = 1201005;
    Threaten = 1201006;
	 	PowerGuard = 1201007;
  end;

  Spearman = class
  const
  	SpearMastery = 1300000;
    PolearmMastery = 1300001;
    SpearBooster = 1301004;
    PolearmBooster = 1301005;
    IronWill = 1301006;
		HyperBody = 1301007;
  end;

  Crusader = class
  const
    ComboAttack = 1111002;
    PanicSword = 1111003;
    PanicAxe = 1111004;
    ComaSword = 1111005;
    ComaAxe = 1111006;
    Shout = 1111008;
  end;

  WhiteKnight = class
  const
    ChargedBlow = 1211002;
    FireChargeSword = 1211003;
    FireChargeBW = 1211004;
    IceChargeSword = 1211005;
    IceChargeBW = 1211006;
    LightningChargeSword = 1211007;
    LightningChargeBW = 1211008;
  end;

  DragonKnight = class
  const
    DragonBlood = 1311008;
  end;

  Magician = class
  const
	  ImprovedMaxMPIncrease = 2000001;
    MPBoost = 2000006;
    MagicGuard = 2001002;
		MagicArmor = 2001003;
  end;

  FPWizard = class
  const
  	MPEater = 2100000;
		Meditation = 2101001;
    Slow = 2101003;
		PoisonBreath = 2101005;
  end;

  ILWizard = class
  const
  	MPEater = 2200000;
    Meditation = 2201001;
    Slow = 2201003;
 		ColdBeam = 2201004;
  end;

  Cleric = class
  const
    MPEater = 2300000;
    Heal = 2301002;
    Invincible = 2301003;
    Bless = 2301004;
  end;

  FPMage = class
  const
    Seal = 2111004;
    SpellBooster = 2111005;
  end;

  ILMage = class
  const
    Seal = 2211004;
    SpellBooster = 2211005;
  end;

  Priest = class
  const
    Doom = 2311005;
    SummonDragon = 2311006;
  end;

  Bowman = class
  const
    Focus = 3001003;
  end;

  Hunter = class
  const
    BowMastery = 3100000;
    BowBooster = 3101002;
    SoulArrow = 3101004;
		ArrowBomb = 3101005;
  end;

  Crossbowman = class
  const
   	CrossbowMastery = 3200000;
 		CrossbowBooster = 3201002;
	 	SoulArrow = 3201004;
  end;

  Ranger = class
  const
    Puppet = 3111002;
    SilverHawk = 3111005;
  end;

  Sniper = class
  const
    Puppet = 3211002;
    GoldenEagle = 3211005;
  end;

  Thief = class
  const
    Disorder = 4001002;
    DarkSight = 4001003;
  end;

  Assassin = class
  const
  	ClawMastery = 4100000;
    CriticalThrow = 4100001;
		ClawBooster = 4101003;
    Haste = 4101004;
	 	Drain = 4101005;
  end;

  Bandit = class
  const
  	DaggerMastery = 4200000;
		DaggerBooster = 4201002;
		Haste = 4201003;
    Steal = 4201004;
	 	SavageBlow = 4201005;
  end;

  Hermit = class
  const
    ShadowPartner = 4111002;
  end;

  ChiefBandit = class
  const
    Assaulter = 4211002;
    ShadowPartner = 4211008;
  end;

  Pirate = class
  const
    Dash = 5001005;
  end;

  Brawler = class
  const
    ImproveMaxHP = 5100000;
    HPBoost = 5100009;
    KnucklerMastery = 5100001;
    BackspinBlow = 5101002;
    DoubleUppercut = 5101003;
    CorkscrewBlow = 5101004;
    MPRecovery = 5101005;
    KnucklerBooster = 5101006;
    OakBarrel = 5101007;
  end;

  Gunslinger = class
  const
    GunMastery = 5200000;
    Grenade = 5201002;
    GunBooster = 5201003;
    BlankShot = 5201004;
  end;

  Marauder = class
  const
    EnergyCharge = 5110001;
    EnergyBlast = 5111002;
  end;

  Outlaw = class
  const
    Octopus = 5211001;
    Gaviota = 5211002;
  end;

  Buccaneer = class
  const
    MapleWarrior = 5121000;
  end;

  Corsair = class
  const
    WrathOfTheOctopi = 5220002;
  end;

  GM = class
  const
    Haste = 9001000;
  end;

  SuperGM = class
  const
    Haste = 9101001;
    Hide = 9101004;
  end;

  Noblesse = class
  const
    ThreeSnails = 10001000;
    Recovery = 10001001;
    NimbleFeet = 10001002;
    LegendarySpirit = 10001003;
    MonsterRider = 10001004;
    BlessingOfTheFairy = 10000012;
  end;

  DawnWarrior = class
  const
    MaxHPEnhancement = 11000000;
    HPBoost = 11000005;
    IronBody = 11001001;
    Soul = 11001004;

    SwordMastery = 11100000;
    SwordBooster = 11101001;
    Rage = 11101003;

    ComboAttack = 11111001;
    Panic = 11111002;
    Coma = 11111003;
  end;

  BlazeWizard = class
  const
    IncreasingMaxMP = 12000000;
    MPBoost = 12000005;
    MagicGuard = 12001001;
    MagicArmor = 12001002;
    Flame = 12001004;

    Meditation = 12101000;
    Slow = 12101001;
    SpellBooster = 12101004;

    Seal = 12111002;
  end;

  WindArcher = class
  const
    Focus = 13001002;
    Storm = 13001004;

		BowMastery = 13100000;
 		BowBooster = 13101001;
    SoulArrow = 13101003;

    Puppet = 13111004;
  end;

  NightWalker = class
  const
    Disorder = 14001002;
    DarkSight = 14001003;
    Darkness = 14001005;

    ClawMastery = 14100000;
    ClawBooster = 14101002;
    Haste = 14101003;

    ShadowPartner = 14111000;
  end;

  ThunderBreaker = class
  const
    Dash = 15001003;
    Lightning = 15001004;

		KnucklerMastery = 15100001;
    EnergyCharge = 15100004;
    HPBoost = 15100007;
		KnucklerBooster = 15101002;
    CorkscrewBlow = 15101003;
    EnergyBlast = 15101005;
    LightningCharge = 15101006;
  end;

  Legend = class
  const
    ThreeSnails = 20001000;
    Recovery = 20001001;
    AgileBody = 20001002;
    BlessingOfTheFairy = 20000012;
  end;

  Aran = class
  const
    ComboAbility = 21000000;
    PolearmBooster = 21001003;
  end;

  Citizen = class
  const
    PotionMastery = 30000002;
    CrystalThrow = 30001000;
    Infiltrate = 30001001;
    Capture = 30001061;
  end;

  WildHunter = class
  const
    JaguarRider = 33001001;
    CrossbowBooster = 33001003;

    SoulArrow = 33101003;

    SilverHawk = 33111005;

    CrossbowExpert = 33120000;
    ExplodingArrows = 33121001;
    SonicRoar = 33121002;
    SharpEyes = 33121004;
    StinkBombShot = 33121005;
    FelineBerserk = 33121006;
    MapleWarrior = 33121007;
    WildArrowBlast = 33121009;
  end;

procedure ApplySkillCosts(Player: TMapleCharacter; ID: Integer; Level: Byte);
procedure Add4thJobSkills(Player: TMapleCharacter);

function GetBeginnerSkill(Job: TMapleJob; BaseSkill: Integer): Integer;

implementation

uses Main, MapleClient, MaplePacketCreator;

procedure ApplySkillCosts(Player: TMapleCharacter; ID: Integer; Level: Byte);
var
  Skill: PSkillLevelInfo;
  MesosMin, MesosMax, Amount: Smallint;
begin
  Skill := SkillDataProv.GetPlayerSkill(ID).Effects[Level];

  if Skill^.MP > 0 then
    Player.AddMP(-Skill^.MP, True);

  if Skill^.HP > 0 then
    Player.AddHP(-Skill^.HP, True);

  if Skill^.Item > 0 then
    Player.RemItemByID(Skill^.Item, Skill^.ItemCount, True, True);

  if Skill^.MoneyCon > 0 then
  begin
    MesosMin := Skill^.MoneyCon - (80 + Level * 5);
    MesosMax := Skill^.MoneyCon + (80 + Level * 5);
    Amount := Rand.RandInt(MesosMax - MesosMin) + MesosMin;

    if Player.Mesos - Amount > -1 then
      Player.ModifyMesos(-Amount, False, True)
    else
      Log('[Hacking] Player %s is using skill %d without having enough money!', [Player.Name, ID]);
  end;
end;

procedure Add4thJobSkills(Player: TMapleCharacter);

  procedure _(ID: Integer);
  begin
    Player.Skills.Add(SkillDataProv.GetPlayerSkill(ID), TPlayerSkillInfo.Create(0, 10));
    TMapleClient(Player.Client).Write(MaplePacketCreator.UpdateSkill(ID, 0, 10, False));
  end;

begin
  case Player.Job of
    mjWildHunter4:
    begin
      _(WildHunter.CrossbowExpert);
      _(WildHunter.ExplodingArrows);
      _(WildHunter.SonicRoar);
      _(WildHunter.SharpEyes);
      _(WildHunter.StinkBombShot);
      _(WildHunter.FelineBerserk);
      _(WildHunter.MapleWarrior);
      _(WildHunter.WildArrowBlast);
    end;
  end;
end;

function GetBeginnerSkill(Job: TMapleJob; BaseSkill: Integer): Integer;
begin
  if (Job = mjLegendEvan) or (Word(Job) div 100 = 22) then
    Exit(20010000 + BaseSkill);

  if IsResistance(Job) then
    case BaseSkill of
      Beginner.ThreeSnails: Exit(Citizen.CrystalThrow);
      Beginner.Recovery: Exit(Citizen.PotionMastery);
      Beginner.NimbleFeet: Exit(Citizen.Infiltrate);
    end;

  Result := (Word(Job) div 1000) * 10000000 + BaseSkill;
end;

end.
