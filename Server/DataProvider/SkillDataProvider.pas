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

unit SkillDataProvider;

interface

uses SysUtils, Types, Generics.Collections, GameLogic;

type
  TSkillLevelInfo = record
    Level, MobCount, HitCount, MaxHP, MaxMP: Byte;
    Time, Item: Cardinal;
    HP, MP, Damage, ItemCount, BulletCon, MoneyCon, HPRate, MPRate, Prop: Word;
    X, Y, Speed, Jump, WAtk, WDef, MAtk, MDef, Acc, Avo, Morph, Cooltime: Smallint;
    Lt, Rb: TPoint;
  end;
  PSkillLevelInfo = ^TSkillLevelInfo;

  TSkill = class
  private
    FID: Integer;
    FEffects: TDictionary<Byte, PSkillLevelInfo>;

    function GetMaxLevel: Byte;
  public
    constructor Create(const ID: Integer);
    destructor Destroy; override;

    function CanBeLearnedBy(Job: TMapleJob): Boolean;
    function IsFourthJob: Boolean;

    property ID: Integer read FID;
    property Effects: TDictionary<Byte, PSkillLevelInfo> read FEffects;
    property MaxLevel: Byte read GetMaxLevel;
  end;

  TPlayerSkillInfo = record
    Level, MasterLevel: Byte;
    constructor Create(const ALevel, AMasterLevel: Byte);
  end;

  TSkillDataProvider = class
  private
    FPlayerSkills: TDictionary<Integer, TSkill>;
  public
    constructor Create;
    destructor Destroy; override;

    function GetPlayerSkill(const ID: Integer): TSkill;
  end;

var
  SkillDataProv: TSkillDataProvider;

implementation

uses MapleServerHandler;

{ TSkill }

constructor TSkill.Create(const ID: Integer);
begin
  FID := ID;
  FEffects := TDictionary<Byte, PSkillLevelInfo>.Create;
end;

destructor TSkill.Destroy;
var
  PSLI: PSkillLevelInfo;
begin
  for PSLI in FEffects.Values do
    Dispose(PSLI);
  FEffects.Free;

  inherited;
end;

function TSkill.GetMaxLevel: Byte;
begin
  Result := FEffects.Count;
end;

function TSkill.CanBeLearnedBy(Job: TMapleJob): Boolean;
var
  JobID, ForJob: Integer;
begin
  if IsJobClass(Job, mjGM) then
    Exit(True);

  JobID := Word(Job);
  ForJob := FID div 10000;

  if (JobID div 100 <> ForJob div 100) and (ForJob div 100 <> 0) then
    Exit(False);   // wrong job

  if (ForJob div 10) mod 10 > (JobID div 10) mod 10 then
    Exit(False);   // wrong 2nd job

  if ForJob mod 10 > JobID mod 10 then
    Exit(False);   // wrong 3rd/4th job

  Result := True;
end;

function TSkill.IsFourthJob: Boolean;
begin
  Result := (FID div 10000) mod 10 = 2;
end;

{ TSkillDataProvider }

constructor TSkillDataProvider.Create;
begin
  FPlayerSkills := TDictionary<Integer, TSkill>.Create;
end;

destructor TSkillDataProvider.Destroy;
var
  Skill: TSkill;
begin
  for Skill in FPlayerSkills.Values do
    Skill.Free;

  FPlayerSkills.Free;

  inherited;
end;

function TSkillDataProvider.GetPlayerSkill(const ID: Integer): TSkill;
var
  Level: PSkillLevelInfo;
begin
  if FPlayerSkills.ContainsKey(ID) then
    Exit(FPlayerSkills[ID]);

  Result := nil;

  with MCDB.GetQuery do
  begin
    SQL.Text := 'SELECT * FROM skill_player_level_data WHERE skillid = ' + IntToStr(ID);
    Open;

    if not EOF then
    begin
      Result := TSkill.Create(ID);
      FPlayerSkills.Add(ID, Result);
    end;

    while not EOF do
    begin
      New(Level);
      with Level^ do
      begin
        Level := FieldByName('skill_level').AsInteger;
        MobCount := FieldByName('mob_count').AsInteger;
        HitCount := FieldByName('hit_count').AsInteger;
        Time := FieldByName('buff_time').AsInteger;
        HP := FieldByName('hp_cost').AsInteger;
        MP := FieldByName('mp_cost').AsInteger;
        Damage := FieldByName('damage').AsInteger;
        Item := FieldByName('item_cost').AsInteger;
        ItemCount := FieldByName('item_count').AsInteger;
        BulletCon := FieldByName('bullet_cost').AsInteger;
        MoneyCon := FieldByName('money_cost').AsInteger;
        X := FieldByName('x_property').AsInteger;
        Y := FieldByName('y_property').AsInteger;
        Speed := FieldByName('speed').AsInteger;
        Jump := FieldByName('jump').AsInteger;
        WAtk := FieldByName('weapon_atk').AsInteger;
        WDef := FieldByName('weapon_def').AsInteger;
        MAtk := FieldByName('magic_atk').AsInteger;
        MDef := FieldByName('magic_def').AsInteger;
        Acc := FieldByName('accuracy').AsInteger;
        Avo := FieldByName('avoid').AsInteger;
        HPRate := FieldByName('hp').AsInteger;
        MPRate := FieldByName('mp').AsInteger;
        {$IFDEF VERSION97_UP}
        MaxHP := FieldByName('max_hp').AsInteger;
        MaxMP := FieldByName('max_mp').AsInteger;
        {$ENDIF}
        Prop := FieldByName('prop').AsInteger;
        Morph := FieldByName('morph').AsInteger;
        Lt := Point(FieldByName('ltx').AsInteger, FieldByName('lty').AsInteger);
        Rb := Point(FieldByName('rbx').AsInteger, FieldByName('rby').AsInteger);
        Cooltime := FieldByName('cooldown_time').AsInteger;
      end;

      Result.Effects.Add(Level^.Level, Level);

      Next;
    end;

    Free;
  end;
end;

{ TPlayerSkillInfo }

constructor TPlayerSkillInfo.Create(const ALevel, AMasterLevel: Byte);
begin
  Level := ALevel;
  MasterLevel := AMasterLevel;
end;

end.
