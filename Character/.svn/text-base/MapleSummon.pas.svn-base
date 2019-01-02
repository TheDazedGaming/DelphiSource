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

unit MapleSummon;

interface

uses Generics.Collections, Types, MapleMapObject, MapleStream, MovementParser,
     Scheduler;

type
  TRemoveMessage = (rmTime, rmDisappearing = 3, rmNone);

  TMapleSummon = class(TAnimatedMapObject)
  private
    FOwner: TObject;
    FSkill, FHP: Integer;
    FSkillLevel, FMoveType, FAttackType: Byte;
    FPuppet, FOctopus: Boolean;
    FTimer: THandle;
  public
    constructor Create(AOwner: TObject; ASkill, ASkillLevel, ATime: Integer; APosition: TPoint);
    destructor Destroy; override;

    procedure AddHP(Delta: Integer);
    procedure Cancel;

    function GetType: TObjectType; override;
    function SendSpawnDataTo(Client: TObject): Boolean; override;
    procedure SendDestroyDataTo(Client: TObject); override;

    function GetSpawnPacket(Animated: Boolean): TMapleStream;
    function GetDamagePacket(Unk, Damage, Monster: Integer): TMapleStream;
    function GetMovePacket(Moves: TList<TLifeMovement>): TMapleStream;
    function GetRemovePacket(Msg: TRemoveMessage): TMapleStream;

    function IsStatic: Boolean;
    class function IsSummon(ASkill: Integer): Boolean;

    property HP: Integer read FHP;
    property Skill: Integer read FSkill;
  end;

implementation

uses MapleCharacter, MapleClient, MapleServerHandler, MaplePacketCreator, Skills;

{ TMapleSummon }

constructor TMapleSummon.Create(AOwner: TObject; ASkill, ASkillLevel, ATime: Integer; APosition: TPoint);
begin
  FOwner := AOwner;
  FSkill := ASkill;
  FSkillLevel := ASkillLevel;
  FPosition := APosition;
  FPuppet := False;
  FOctopus := False;
  FHP := 0;

  case FSkill of
    Ranger.Puppet,
    Sniper.Puppet,
    WindArcher.Puppet:
      FPuppet := True;

    Outlaw.Octopus,
    Corsair.WrathOfTheOctopi:
      FOctopus := True;
  end;

  if IsStatic then
    FMoveType := 0
  else
    case FSkill of
      DawnWarrior.Soul,
      BlazeWizard.Flame,
      WindArcher.Storm,
      NightWalker.Darkness,
      ThunderBreaker.Lightning:
        FMoveType := 1;

      Priest.SummonDragon,
      Ranger.SilverHawk,
      Sniper.GoldenEagle,
      WildHunter.SilverHawk:
        FMoveType := 4;

      Outlaw.Gaviota:
        FMoveType := 5;
    end;

  if FPuppet then
    FAttackType := 0
  else
    FAttackType := 1;

  FTimer := Sched.AddSchedule(ATime * 1000, Cancel);
end;

destructor TMapleSummon.Destroy;
begin
  if FTimer > 0 then
    Sched.CancelSchedule(FTimer);

  inherited;
end;

procedure TMapleSummon.AddHP(Delta: Integer);
begin
  Inc(FHP, Delta);
end;

procedure TMapleSummon.Cancel;
begin
  TMapleCharacter(FOwner).Summons.Remove(Self);
  TMapleCharacter(FOwner).Map.RemoveMapObject(Self);
  TMapleCharacter(FOwner).Map.BroadcastMessage(GetRemovePacket(rmTime));
  FTimer := 0;
  Free;
end;

function TMapleSummon.GetSpawnPacket(Animated: Boolean): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['SpawnSummon']);

    WriteInt(TMapleCharacter(FOwner).ID);
    WriteInt(FObjectID);
    WriteInt(FSkill);
    WriteByte(TMapleCharacter(FOwner).Level);
    WriteByte(FSkillLevel);
    WritePos(FPosition);
    WriteByte(4);
    WriteShort(0);
    WriteByte(FMoveType);
    WriteByte(FAttackType);
    WriteBool(Animated);
    WriteByte(0);
  end;
end;

function TMapleSummon.GetDamagePacket(Unk, Damage, Monster: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['DamageSummon']);

    WriteInt(TMapleCharacter(FOwner).ID);
    WriteInt(FObjectID);
    WriteByte(Unk);
    WriteInt(Damage);
    WriteInt(Monster);
    WriteByte(0);
  end;
end;

function TMapleSummon.GetMovePacket(Moves: TList<TLifeMovement>): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['MoveSummon']);

    WriteInt(TMapleCharacter(FOwner).ID);
    WriteInt(FObjectID);
    WritePos(FPosition);
    WriteInt(0);
    SerializeMovements(Result, Moves);
  end;
end;

function TMapleSummon.GetRemovePacket(Msg: TRemoveMessage): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['RemoveSummon']);

    WriteInt(TMapleCharacter(FOwner).ID);
    WriteInt(FObjectID);
    WriteByte(Msg);
  end;
end;

function TMapleSummon.GetType: TObjectType;
begin
  Result := otSummon;
end;

function TMapleSummon.IsStatic: Boolean;
begin
  Result := FPuppet or FOctopus;
end;

class function TMapleSummon.IsSummon(ASkill: Integer): Boolean;
begin
  case ASkill of
    Priest.SummonDragon,
    Ranger.Puppet,
    Ranger.SilverHawk,
    Sniper.Puppet,
    Sniper.GoldenEagle,
    Outlaw.Octopus,
    Outlaw.Gaviota,
    Corsair.WrathOfTheOctopi,
    DawnWarrior.Soul,
    BlazeWizard.Flame,
    WindArcher.Storm,
    WindArcher.Puppet,
    NightWalker.Darkness,
    ThunderBreaker.Lightning,
    WildHunter.SilverHawk:
      Result := True;
    else
      Result := False;
  end;
end;

procedure TMapleSummon.SendDestroyDataTo(Client: TObject);
begin
  TMapleClient(Client).Write(GetRemovePacket(rmNone));
end;

function TMapleSummon.SendSpawnDataTo(Client: TObject): Boolean;
begin
  TMapleClient(Client).Write(GetSpawnPacket(False));
  Result := True;
end;

end.
