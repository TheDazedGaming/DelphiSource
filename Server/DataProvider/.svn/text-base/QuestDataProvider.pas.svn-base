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

unit QuestDataProvider;

interface

uses SysUtils, Generics.Collections, MapleQuest, ZDataset, GameLogic, Utils;

type
  TQuestDataProvider = class
  private
    FQuests: TDictionary<Word, TMapleQuest>;

    procedure LoadData;

    function GetQuest(ID: Word): TMapleQuest;
  public
    constructor Create;
    destructor Destroy; override;

    property Quests[ID: Word]: TMapleQuest read GetQuest;
  end;

var
  // Singleton
  QuestDataProv: TQuestDataProvider;

implementation

uses Main, Settings, MapleServerHandler;

function StringIndex(const SearchString: string; StrList: array of string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(StrList) do
    if CompareText(SearchString, StrList[I]) = 0 then
      Exit(I);
end;

{ TQuestDataProvider }

constructor TQuestDataProvider.Create;
begin
  FQuests := TDictionary<Word, TMapleQuest>.Create;

  LoadData;
end;

destructor TQuestDataProvider.Destroy;
var
  Quest: TMapleQuest;
begin
  for Quest in FQuests.Values do
    Quest.Free;

  FreeAndNil(FQuests);

  inherited;
end;

function TQuestDataProvider.GetQuest(ID: Word): TMapleQuest;
begin
  if FQuests.ContainsKey(ID) then
    Result := FQuests[ID]
  else
    Result := nil;
end;

procedure TQuestDataProvider.LoadData;
var
  Q: TZQuery;
  CurrentID, ObjID, Job, i: Integer;
  ObjCount: Word;
  CurQuest: TMapleQuest;
  Start: Boolean;
  Reward: PQuestRewardInfo;
  RT, JobTracks: string;
  EJob: TArrayofstring;
begin
  Q := MCDB.GetQuery;
  Q.SQL.Text := 'SELECT questid, next_quest FROM quest_data';
  Q.Open;

  while not Q.EOF do
  begin
    FQuests.Add(Q.FieldByName('questid').AsInteger,
                TMapleQuest.Create(Q.FieldByName('questid').AsInteger,
                                   Q.FieldByName('next_quest').AsInteger));

    Q.Next;
  end;

  Q.Close;

  CurQuest := nil;

  Q.SQL.Text := 'SELECT * FROM quest_requests';
  Q.Open;

  while not Q.EOF do
  begin
    CurrentID := Q.FieldByName('questid').AsInteger;

    if FQuests.ContainsKey(CurrentID) then
      CurQuest := FQuests[CurrentID]
    else
    begin
      Q.Next;
      Continue;
    end;

    // xxx process quest_state

    ObjID := Q.FieldByName('objectid').AsInteger;
    ObjCount := Q.FieldByName('quantity').AsInteger;

    RT := Q.FieldByName('request_type').AsString;
    case StringIndex(RT, ['mob', 'item', 'quest']) of
      0: CurQuest.AddMobRequest(ObjID, ObjCount);
      1: CurQuest.AddItemRequest(ObjID, ObjCount);
      2: CurQuest.AddQuestRequest(ObjID, ObjCount);
      else Log('[Info] Unknown Quest-Request-Type: ' + RT);
    end;

    Q.Next;
  end;
  Q.Close;

  CurQuest := nil;

  Q.SQL.Text := 'SELECT * FROM quest_rewards';
  Q.Open;

  while not Q.EOF do
  begin
    CurrentID := Q.FieldByName('questid').AsInteger;

    if FQuests.ContainsKey(CurrentID) then
      CurQuest := FQuests[CurrentID]
    else
    begin
      Q.Next;
      Continue;
    end;

    Job := Q.FieldByName('job').AsInteger;
    Start := Q.FieldByName('quest_state').AsString = 'start';

    New(Reward);

    // xxx pet_closeness RT
    RT := Q.FieldByName('reward_type').AsString;
    Reward^.IsItem := RT = 'item';
    Reward^.IsExp := RT = 'exp';
    Reward^.IsMesos := RT = 'mesos';
    Reward^.IsFame := RT = 'fame';
    Reward^.IsSkill := RT = 'skill';
    Reward^.IsBuff := RT = 'buff';
    Reward^.IsMasterLevelOnly := Pos('only_master_level', Q.FieldByName('flags').AsString) > 0;
    Reward^.ID := Q.FieldByName('rewardid').AsInteger;
    Reward^.Count := Q.FieldByName('quantity').AsInteger;
    Reward^.MasterLevel := Q.FieldByName('master_level').AsInteger;
    Reward^.Gender := StringIndex(Q.FieldByName('gender').AsString, ['male', 'female']);
    Reward^.Prop := Q.FieldByName('prop').AsInteger;
    JobTracks := Q.FieldByName('job_tracks').AsString;

    if (Job <> -1) or (Length(JobTracks) = 0) then
      CurQuest.AddReward(Start, Reward, TMapleJob(Job))
    else
    begin
      EJob := Explode(',', JobTracks);
      // xxx cygnus support
      for i := 0 to High(EJob) do
        case StringIndex(EJob[i], ['beginner', 'warrior', 'magician', 'bowman', 'thief', 'pirate']) of
          // TMapleQuest.GiveRewards runs GetJobClass on the player's job, so we don't need to add each single job here
          0: CurQuest.AddReward(Start, Reward, mjBeginner);
          1: CurQuest.AddReward(Start, Reward, mjWarrior);
          2: CurQuest.AddReward(Start, Reward, mjMagician);
          3: CurQuest.AddReward(Start, Reward, mjBowman);
          4: CurQuest.AddReward(Start, Reward, mjThief);
          5: CurQuest.AddReward(Start, Reward, mjPirate);
        end;
      EJob := nil;
    end;

    Q.Next;
  end;
  Q.Close;

  FreeAndNil(Q);
end;

end.
