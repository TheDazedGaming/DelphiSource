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

unit GameLogic;

interface

uses Math;

const
  NO_MAP = 999999999;

type
  TMapleJob = (mjBeginner,
               mjWarrior = 100, mjFighter = 110, mjCrusader, mjHero,
                                mjPage = 120, mjWhiteKnight, mjPaladin,
                                mjSpearman = 130, mjDragonKnight, mjDarkKnight,

               mjMagician = 200, mjFPWizard = 210, mjFPMage, mjFPArchMage,
                                 mjILWizard = 220, mjILMage, mjILArchMage,
                                 mjCleric = 230, mjPriest, mjBishop,

               mjBowman = 300, mjHunter = 310, mjRanger, mjBowmaster,
                               mjCrossbowman = 320, mjSniper, mjMarksman,

               mjThief = 400, mjAssassin = 410, mjHermit, mjNightLord,
                              mjBandit = 420, mjChiefBandit, mjShadower,
               mjBladeRecruit = 430, mjBladeAcolyte, mjBladeSpecialist, mjBladeLord, mjBladeMaster,

               mjPirate = 500, mjBrawler = 510, mjMarauder, mjBuccaneer,
                               mjGunslinger = 520, mjOutlaw, mjCorsair,

               mjGM = 900, mjSuperGM = 910,

               // Cygnus
               mjNoblesse = 1000,
               mjDawnWarrior = 1100, mjDawnWarrior2 = 1110, mjDawnWarrior3, mjDawnWarrior4,
               mjBlazeWizard = 1200, mjBlazeWizard2 = 1210, mjBlazeWizard3, mjBlazeWizard4,
               mjWindArcher = 1300, mjWindArcher2 = 1310, mjWindArcher3, mjWindArcher4,
               mjNightWalker = 1400, mjNightWalker2 = 1410, mjNightWalker3, mjNightWalker4,
               mjThunderBreaker = 1500, mjThunderBreaker2 = 1510, mjThunderBreaker3, mjThunderBreaker4,

               // Aran
               mjLegend = 2000,
               mjAran = 2100, mjAran2 = 2110, mjAran3, mjAran4,

               // Evan
               mjLegendEvan = 2001,
               mjEvan1 = 2200,

               // Resistance
               mjCitizen = 3000,
               mjWildHunter = 3300, mjWildHunter2 = 3310, mjWildHunter3, mjWildHunter4);

  TMapleInventoryType = (miEquipped = -1, miEquip = 1, miUse, miSetup, miEtc, miCash);

  TItemType = (itHelm = 100, itFace, itEye, itEarring, itTop, itOverall, itBottom,
               itShoe, itGlove, itShield, itCape, itRing, itPendant,
               it1hSword = 130, it1hAxe, it1hMace, itDagger, itKatara, itWand = 137, itStaff,
               it2hSword = 140, it2hAxe, it2hMace, itSpear, itPolearm, itBow,
               itCrossbow, itClaw, itKnuckle, itGun, itMount = 190,
               itArrow = 206, itStar, itBullet = 233, itMonsterCard = 238);

  TEquipSlot = (esTop = -5, esBottom = -6, esShoes = -7, esShield = -10, esWeapon = -11,
                esMount = -18, esSaddle = -19, esMedal = -49);

  TBaseHP = (bhVariation = 4,   // Range
             // These are used for level up
             bhBeginner = 12, bhWarrior = 24, bhMagician = 10, bhBowman = 20,
             bhThief = 20, bhPirate = 22, bhGM = 24, bhAran = 44, bhWildHunter = 30,
             // These are used for AP distribution
             bhBeginnerAP = 8, bhWarriorAP = 20, bhMagicianAP = 8, bhBowmanAP = 16,
             bhThiefAP = 16, bhPirateAP = 18, bhGMAP = 20, bhAranAP = 40, bhWildHunterAP = 26);

  TBaseMP = (bmVariation = 2,  // Range
             // These are used for level up
             bmBeginner = 10, bmWarrior = 4, bmMagician = 6, bmBowman = 14,
             bmThief = 14, bmPirate = 18, bmGM = 18, bmAran = 4,
             // These are used for AP distribution
             bmBeginnerAP = 6, bmWarriorAP = 2, bmMagicianAP = 18, bmBowmanAP = 10,
             bmThiefAP = 10, bmPirateAP = 14, bmGMAP = 18, bmAranAP = 2);

  MobStatus = class
  const
    WAtk = $01;
    WDef = $02;
    Acc = $10;
    Speed = $40;

    Stun = $80;
    Freeze = $100;
    Poison = $200;
    Seal = $400;

    Doom = $10000;

    Darkness = $2000000;
  end;

function GetExpNeededAtLevel(const Level: Integer): Integer;
function GetInventory(const ItemID: Integer): TMapleInventoryType;
function GetItemType(const ItemID: Integer): TItemType;
function GetMaxDamageMultiplier(const Weapon: TItemType): Single;

function IsArrowForBow(const ItemID: Integer): Boolean;
function IsArrowForCBow(const ItemID: Integer): Boolean;
function IsKatara(ItemID: Integer): Boolean;
function IsOneHandedWeapon(ItemID: Integer): Boolean;
function IsTwoHandedWeapon(ItemID: Integer): Boolean;
function IsWeapon(ItemID: Integer): Boolean;
function IsOverall(const ItemID: Integer): Boolean;
function IsShield(const ItemID: Integer): Boolean;
function IsRechargeable(const ItemID: Integer): Boolean;

function AdvancementForLevel(Job: TMapleJob; Lv: Byte): Integer;
function AdvancementOf(Job: TMapleJob): Integer;
function GetJobClass(const Job: TMapleJob; CygnusToNormal: Boolean = False): TMapleJob;
function GetMaxLevel(const Job: TMapleJob): Byte;
function IsBeginner(Job: TMapleJob): Boolean;
function IsJobClass(Job: TMapleJob; JClass: TMapleJob; CygnusToNormal: Boolean = False): Boolean;
function IsRegularJob(const ID: Word): Boolean;
function IsCygnusJob(const Job: TMapleJob): Boolean;
function IsEvan(const Job: TMapleJob): Boolean;
function IsResistance(const Job: TMapleJob): Boolean;
function IsExtendedSPJob(Job: TMapleJob): Boolean;

implementation

uses Main;

function GetExpNeededAtLevel(const Level: Integer): Integer;
{ Thanks to airflow for this algorithm }
{$IFNDEF BIGBANG}
var
  Square, Exp, i: Integer;
begin
  Square := Level * Level;

  if Level <= 5 then
    Exp := (2 * Square) + (13 * Level)
  else
    Exp := 0;

  if Level in [6..50] then
  begin
    Exp := Square * Square;
    if Level mod 3 = 0 then
      Inc(Exp, 57 * Square)
    else
      Inc(Exp, 55 * Square - 56);
    Exp := Exp div 9;
  end;

  if Level >= 51 then
  begin
    Exp := 709716;  // base exp needed at Lv 50
    for i := Level downto 51 do
      Exp := Floor(Exp * 1.0548);
  end;

  if Level in [10..29] then
    Dec(Exp, Ceil(Exp * 0.3333))
  else if Level >= 30 then
    Exp := Floor(Exp * 0.8);

  Result := Exp;
{$ELSE}  // Big Bang Exp Curve
const
  EXP_TABLE: array[1..199] of Integer = (15, 34, 57, 92, 135, 372, 560, 840,
    1242, 1242, 1242, 1242, 1242, 1242, 1490, 1788, 2146, 2575, 3090, 3708, 4450,
    5340, 6408, 7690, 9228, 11074, 13289, 15947, 19136, 19136, 19136, 19136, 19136,
    19136, 22963, 27556, 33067, 39681, 47616, 51425, 55539, 59582, 64781, 69963,
    75560, 81605, 88133, 95184, 102799, 111023, 119905, 129497, 139857, 151046,
    163129, 176180, 190274, 205496, 221936, 239691, 258866, 279575, 301941, 326097,
    352184, 380359, 410788, 443651, 479143, 479143, 479143, 479143, 479143, 479143,
    512683, 548571, 586971, 628059, 672024, 719065, 769400, 823258, 880886, 942548,
    1008526, 1079123, 1154662, 1235488, 1321972, 1414511, 1513526, 1619473, 1732836,
    1854135, 1983924, 2122799, 2271395, 2430393, 2600520, 2782557, 2977336, 3185749,
    3408752, 3647365, 3902680, 4175868, 4468179, 4780951, 5115618, 5473711, 5856871,
    6266852, 6705531, 7176919, 7677163, 8214565, 8789584, 9404855, 10063195, 10063195,
    10063195, 10063195, 10063195, 10063195, 10767619, 11521352, 12327847, 13190796,
    14114152, 15102142, 16159292, 17290443, 18500774, 19795828, 21181536, 22664244,
    24250741, 25948292, 27764673, 29708200, 31787774, 34012918, 36393823, 38941390,
    41667288, 44583998, 47704878, 51044219, 54617315, 58440527, 62531364, 66908559,
    71592158, 76603609, 81965862, 87703472, 93842715, 100411706, 107440525, 113895024,
    120728726, 127972450, 135650797, 143789844, 152417235, 161565269, 171256005, 181531366,
    192423248, 203968643, 216206761, 229179167, 242929917, 257505712, 272956055, 289333418,
    306693423, 525095029, 344600730, 365276774, 387193381, 410424983, 435050483, 461153512,
    488822722, 518152086, 549241211, 582195683, 617127424, 654155070, 693404374, 735008637,
    779109155, 825855704, 875407047, 927931469, 983607358, 1042623799, 1105181227);
begin
  Result := EXP_TABLE[Level];
{$ENDIF}
end;

function GetInventory(const ItemID: Integer): TMapleInventoryType;
begin
  Result := TMapleInventoryType(Trunc(ItemID / 1000000));
end;

function GetItemType(const ItemID: Integer): TItemType;
begin
  Result := TItemType(ItemID div 10000);
end;

function GetMaxDamageMultiplier(const Weapon: TItemType): Single;
begin
  case Weapon of
    itBow: Result := 3.4;
    itCrossbow, itClaw, itStaff, itWand: Result := 3.6;
    itGun: Result := 3.715;
    itDagger, it1hSword: Result := 4;
    it1hAxe, it1hMace: Result := 4.4;
    it2hSword: Result := 4.6;
    it2hAxe, it2hMace, itKnuckle: Result := 4.8;
    itSpear, itPolearm: Result := 5.0;

    else Result := 0;   // No weapon / unknown O_o
  end;
end;

function GetJobClass(const Job: TMapleJob; CygnusToNormal: Boolean = False): TMapleJob;
begin
  if IsBeginner(Job) then
    Exit(mjBeginner);

  if (not CygnusToNormal) or ((CygnusToNormal) and (IsRegularJob(Word(Job)))) then
    Result := TMapleJob(Trunc(Word(Job) / 100) * 100)
  else if (CygnusToNormal) and (Word(Job) >= 1000) and (Word(Job) < 2000) then
    Result := TMapleJob(Trunc((Word(Job) - 1000) / 100) * 100)
  else if Job = mjAran then
    Result := Job
  else if Word(Job) div 100 = 33 then
    Result := mjWildHunter
  else
  begin
    Log('[WARNING] GetJobClass failed for %d', [Word(Job)]);
    Result := Job;
  end;
end;

function IsArrowForBow(const ItemID: Integer): Boolean;
begin
  Result := ItemID div 1000 = 2060;
end;

function IsArrowForCBow(const ItemID: Integer): Boolean;
begin
  Result := ItemID div 1000 = 2061;
end;

function IsKatara(ItemID: Integer): Boolean;
begin
  Result := GetItemType(ItemID) = itKatara;
end;

function IsOneHandedWeapon(ItemID: Integer): Boolean;
begin
  Result := ItemID div 100000 = 13;
end;

function IsTwoHandedWeapon(ItemID: Integer): Boolean;
begin
  Result := ItemID div 100000 = 14;
end;

function IsWeapon(ItemID: Integer): Boolean;
begin
  Result := IsOneHandedWeapon(ItemID) or IsTwoHandedWeapon(ItemID);
end;

function IsOverall(const ItemID: Integer): Boolean;
begin
  Result := GetItemType(ItemID) = itOverall;
end;

function IsShield(const ItemID: Integer): Boolean;
begin
  Result := GetItemType(ItemID) = itShield;
end;

function IsRechargeable(const ItemID: Integer): Boolean;
begin
  Result := GetItemType(ItemID) in [itStar, itBullet];
end;

function IsJobClass(Job: TMapleJob; JClass: TMapleJob; CygnusToNormal: Boolean = False): Boolean;
begin
  Result := GetJobClass(Job, CygnusToNormal) = JClass;
end;

function IsBeginner(Job: TMapleJob): Boolean;
begin
  Result := (Word(Job) mod 1000 = 0) or (Job = mjLegendEvan);
end;

function IsRegularJob(const ID: Word): Boolean;
begin
  Result := (ID >= 100) and (ID <= 910);
end;

function IsCygnusJob(const Job: TMapleJob): Boolean;
begin
  Result := (Word(Job) >= 1000) and (Word(Job) <= 1512);
end;

function IsEvan(const Job: TMapleJob): Boolean;
begin
  Result := (Job = mjLegendEvan) or ((Word(Job) >= 2200) and (Word(Job) <= 2218));
end;

function IsResistance(const Job: TMapleJob): Boolean;
begin
  Result := Job >= mjCitizen;
end;

function IsExtendedSPJob(Job: TMapleJob): Boolean;
begin
  Result := IsEvan(Job) or IsResistance(Job);
end;

function AdvancementForLevel(Job: TMapleJob; Lv: Byte): Integer;
begin
  if IsResistance(Job) then
  begin
    case Lv of
      11..30: Result := 1;
      31..70: Result := 2;
      71..120: Result := 3;
      else Result := 4;
    end;
  end
  else if IsEvan(Job) then
  begin
    case Lv of
      11..60:
      begin
        if Lv mod 10 = 0 then
          Result := Lv div 10 - 1
        else
          Result := Lv div 10;
      end;
      61..80: Result := 6;
      81..100: Result := 7;
      101..120: Result := 8;
      121..160: Result := 9;
      else Result := 10;
    end;
  end
  else
    Result := 0;
end;

function AdvancementOf(Job: TMapleJob): Integer;
begin
  if Word(Job) mod 100 >= 10 then
    Exit(Word(Job) mod 10 + 2)
  else if Word(Job) mod 1000 >= 100 then
    Exit(1);

  Result := 0;
end;

function GetMaxLevel(const Job: TMapleJob): Byte;
begin
  if IsCygnusJob(Job) then
    Result := 120
  else
    Result := 200;
end;

end.
