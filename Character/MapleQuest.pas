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

unit MapleQuest;

interface

uses SysUtils, Generics.Collections, PlayerInventory, MapleCharacter,
     GameLogic, MTRand, MapleItem;

type
  TQuestRewardInfo = record
    IsMesos, IsItem, IsExp, IsFame, IsSkill, IsMasterLevelOnly, IsBuff: Boolean;
    Gender: ShortInt;
    Count, MasterLevel: SmallInt;
    Prop, ID: Integer;
  end;
  PQuestRewardInfo = ^TQuestRewardInfo;

  TQuestRewards = class
  private
    FRewards: TList<PQuestRewardInfo>;
    FJobRewards: TDictionary<TMapleJob, TList<PQuestRewardInfo>>;
  public
    constructor Create;
    destructor Destroy; override;

    property Rewards: TList<PQuestRewardInfo> read FRewards;
    property JobRewards: TDictionary<TMapleJob, TList<PQuestRewardInfo>> read FJobRewards;
  end;

  TMapleQuest = class
  private
    FID, FNextQuest: Word;
    FMobRequests, FItemRequests: TDictionary<Integer, Word>;
    FQuestRequests: TDictionary<Word, Byte>;
    FStartRewards, FEndRewards: TQuestRewards;
    FAllRewards: TList<PQuestRewardInfo>;
  public
    constructor Create(ID, NextQuest: Word);
    destructor Destroy; override;

    procedure AddMobRequest(ID: Integer; Count: Word);
    procedure AddItemRequest(ID: Integer; Count: Word);
    procedure AddQuestRequest(ID: Word; State: Byte);

    procedure AddReward(Start: Boolean; const RewInfo: PQuestRewardInfo; Job: TMapleJob);

    procedure GiveRewards(Target: TMapleCharacter; Start: Boolean);

    property ID: Word read FID;
    property NextQuest: Word read FNextQuest;
    property ItemRequests: TDictionary<Integer, Word> read FItemRequests;
    property MobRequests: TDictionary<Integer, Word> read FMobRequests;
  end;

  TQuestStatus = (qsNotStarted, qsStarted, qsAllRequestsDone, qsCompleted);

  TPlayerQuestStatus = class
  private
    FQuest: TMapleQuest;
    FStatus: TQuestStatus;
    FCompletionTime: Int64;
    FForfeited: Integer;
    FMobKills: TDictionary<Integer, Word>;
    FData: string;
    FID: Word;

    function GetID: Word;
  public
    constructor Create(Quest: TMapleQuest; Status: TQuestStatus; AID: Word);
    destructor Destroy; override;

    procedure CheckDone(Client: TObject);
    function GetQuestData: string;

    property CompletionTime: Int64 read FCompletionTime write FCompletionTime;
    property Forfeited: Integer read FForfeited write FForfeited;
    property MobKills: TDictionary<Integer, Word> read FMobKills;
    property Quest: TMapleQuest read FQuest;
    property Status: TQuestStatus read FStatus write FStatus;
    property Data: string read FData write FData;  // read access only for DB!! else use GetQuestData
    property ID: Word read GetID;
  end;

implementation

uses Main, MapleClient, MaplePacketCreator;

{ TQuestRewards }

constructor TQuestRewards.Create;
begin
  FRewards := TList<PQuestRewardInfo>.Create;
  FJobRewards := TDictionary<TMapleJob, TList<PQuestRewardInfo>>.Create;
end;

destructor TQuestRewards.Destroy;
var
  Rew: TList<PQuestRewardInfo>;
begin
  // The data in the lists is freed by TMapleQuest
  FreeAndNil(FRewards);

  for Rew in FJobRewards.Values do
    Rew.Free;
  FreeAndNil(FJobRewards);

  inherited;
end;

{ TMapleQuest }

constructor TMapleQuest.Create(ID, NextQuest: Word);
begin
  FID := ID;
  FNextQuest := NextQuest;

  FMobRequests := TDictionary<Integer, Word>.Create;
  FItemRequests := TDictionary<Integer, Word>.Create;
  FQuestRequests := TDictionary<Word, Byte>.Create;

  FAllRewards := TList<PQuestRewardInfo>.Create;
  FStartRewards := TQuestRewards.Create;
  FEndRewards := TQuestRewards.Create;
end;

destructor TMapleQuest.Destroy;
var
  PQRI: PQuestRewardInfo;
begin
  FreeAndNil(FMobRequests);
  FreeAndNil(FItemRequests);
  FreeAndNil(FQuestRequests);

  FreeAndNil(FStartRewards);
  FreeAndNil(FEndRewards);

  for PQRI in FAllRewards do
    Dispose(PQRI);
  FAllRewards.Free;

  inherited;
end;

procedure TMapleQuest.AddItemRequest(ID: Integer; Count: Word);
begin
  FItemRequests.AddOrSetValue(ID, Count);
end;

procedure TMapleQuest.AddMobRequest(ID: Integer; Count: Word);
begin
  FMobRequests.Add(ID, Count);
end;

procedure TMapleQuest.AddQuestRequest(ID: Word; State: Byte);
begin
  FQuestRequests.AddOrSetValue(ID, State);
end;

procedure TMapleQuest.AddReward(Start: Boolean; const RewInfo: PQuestRewardInfo;
  Job: TMapleJob);
var
  L: TList<PQuestRewardInfo>;
begin
  if not FAllRewards.Contains(RewInfo) then
    FAllRewards.Add(RewInfo);

  if Start then
  begin
    if SmallInt(Job) = -1 then
      FStartRewards.Rewards.Add(RewInfo)
    else
    begin
      if not FStartRewards.JobRewards.ContainsKey(Job) then
      begin
        L := TList<PQuestRewardInfo>.Create;
        L.Add(RewInfo);
        FStartRewards.JobRewards.Add(Job, L);
      end
      else
        FStartRewards.JobRewards[Job].Add(RewInfo);
    end;
  end
  else
    if SmallInt(Job) = -1 then
      FEndRewards.Rewards.Add(RewInfo)
    else
      if not FEndRewards.JobRewards.ContainsKey(Job) then
      begin
        L := TList<PQuestRewardInfo>.Create;
        L.Add(RewInfo);
        FEndRewards.JobRewards.Add(Job, L);
      end
      else
        FEndRewards.JobRewards[Job].Add(RewInfo);
end;

procedure TMapleQuest.GiveRewards(Target: TMapleCharacter; Start: Boolean);
var
  Rews: TQuestRewards;
  Rew: PQuestRewardInfo;
  Chance, Ran: Integer;
  Items: TList<PQuestRewardInfo>;

  procedure GiveReward;
  var
    Available: TList<TItem>;
    Item: TItem;
    Removed: Integer;
  begin
    if Rew^.IsExp then
      Target.GainExp(Rew^.ID, True, True)
    else
    if Rew^.IsItem then
    begin
      if Rew^.Count > 0 then
      begin
        Target.GainItemByID(Rew^.ID, Rew^.Count);
        TMapleClient(Target.Client).Write(ShowItemGain(Rew^.ID, Rew^.Count, True));
      end
      else
      if Rew^.Count < 0 then
      begin
        Target.RemoveItemByID(Rew^.ID, -Rew^.Count, True, False);
        TMapleClient(Target.Client).Write(ShowItemGain(Rew^.ID, Rew^.Count, True));
      end
      else   // sometimes count is 0 -- Remove all
      begin
        Removed := 0;
        Available := Target.Inventory[GetInventory(Rew^.ID)].ListByID(Rew^.ID);
        try
          for Item in Available do
          begin
            Target.RemoveItemFromSlot(GetInventory(Rew^.ID), Item.Position, Item.Quantity, True, False);
            Inc(Removed, Item.Quantity);
          end;
        finally
          Available.Free;
        end;

        if Removed > 0 then
          TMapleClient(Target.Client).Write(ShowItemGain(Rew^.ID, -Removed, True));
      end;
    end
    else
    if Rew^.IsMesos then
      Target.ModifyMesos(Rew^.ID, True, False, True)
    else
    if Rew^.IsFame then
    begin
      Target.ModifyFame(Rew^.ID);
      Target.UpdateSingleStat(msFame, Target.Fame);
      TMapleClient(Target.Client).Write(ShowFameGain(Rew^.ID));
    end
    else
      Log('[WARNING] Unimplemented operation: MapleQuest.GiveRewards');
  end;

begin
  if Start then
    Rews := FStartRewards
  else
    Rews := FEndRewards;

  if (Rews.Rewards.Count = 0) and (Rews.JobRewards.Count = 0) then
    Exit;

  Chance := 0;
  Items := nil;

  for Rew in Rews.Rewards do
    if Rew^.IsItem and (Rew^.Prop > 0) then
    begin
      Inc(Chance, Rew^.Prop);

      if not Assigned(Items) then
        Items := TList<PQuestRewardInfo>.Create;
      Items.Add(Rew);
    end
    else
      GiveReward;

  if Rews.JobRewards.ContainsKey(GetJobClass(Target.Job)) then
    for Rew in Rews.JobRewards[GetJobClass(Target.Job)] do
      if Rew^.IsItem and (Rew^.Prop > 0) then
      begin
        Inc(Chance, Rew^.Prop);

        if not Assigned(Items) then
          Items := TList<PQuestRewardInfo>.Create;
        Items.Add(Rew);
      end
      else
        GiveReward;

  if Chance > 0 then
  begin
    Ran := Rand.RandInt(Chance - 1);
    Chance := 0;
    Log('[Quest] Ran = %d', [Ran]);

    for Rew in Items do
    begin
      Log('[Quest] Rew.ID = %d; Chance = %d', [Rew^.ID, Chance]);

      if Chance >= Ran then
      begin
        TMapleClient(Target.Client).Write(ShowItemGain(Rew^.ID, Rew^.Count, True));
        if Rew^.Count > 0 then
          Target.GainItemByID(Rew^.ID, Rew^.Count)
        else
          Target.RemoveItemByID(Rew^.ID, Rew^.Count, True, False);

        Break;
      end
      else
        Inc(Chance, Rew^.Prop);
    end;
  end;
end;

{ TPlayerQuestStatus }

constructor TPlayerQuestStatus.Create(Quest: TMapleQuest; Status: TQuestStatus; AID: Word);
var
  Mob: Integer;
begin
  FQuest := Quest;
  FStatus := Status;
  FCompletionTime := -1;
  FForfeited := 0;
  FData := '';
  FID := AID;

  if (Assigned(FQuest)) and (FQuest.MobRequests.Count > 0) then
  begin
    FMobKills := TDictionary<Integer, Word>.Create;

    for Mob in FQuest.MobRequests.Keys do
      FMobKills.Add(Mob, 0);
  end;
end;

destructor TPlayerQuestStatus.Destroy;
begin
  if Assigned(FMobKills) then
    FreeAndNil(FMobKills);

  inherited;
end;

procedure TPlayerQuestStatus.CheckDone(Client: TObject);
var
  i: Integer;
  L: TList<TItem>;
begin
  // Should be qsStarted or qsAllRequestsDone...
  if (not Assigned(FQuest)) or (FStatus <> qsStarted) then
    Exit;   // no data / already checked

  for i in FQuest.MobRequests.Keys do
    if (not FMobKills.ContainsKey(i)) or (FMobKills[i] < FQuest.MobRequests[i]) then
      Exit;

  for i in FQuest.ItemRequests.Keys do
  begin
    L := TMapleClient(Client).Player.Inventory[GetInventory(i)].ListByID(i);
    try
      if (L.Count = 0) or (MapleItem.TItem(L[0]).Quantity < FQuest.ItemRequests[i]) then
        Exit;
    finally
      L.Free;
    end;
  end;

  FStatus := qsAllRequestsDone;
  TMapleClient(Client).Write(ShowQuestComplete(FQuest.ID));
end;

function TPlayerQuestStatus.GetID: Word;
begin
  if Assigned(FQuest) then
    Result := FQuest.ID
  else
    Result := FID;
end;

function TPlayerQuestStatus.GetQuestData: string;
var
  i: Integer;
  Mobs: TList<Integer>;
begin
  Result := '';

  if (FMobKills = nil) and (FData = '') then
    Exit;

  if (FData <> '') and (FMobKills <> nil) then
    Log('[WARNING] Conflict at quest %d, data and mobkills assigned!', [FQuest.ID]);

  if FData <> '' then
    Result := FData
  else
  begin
    // Put all IDs in a separate list first to sort them
    Mobs := TList<Integer>.Create;
    for i in FMobKills.Keys do
      Mobs.Add(i);
    Mobs.Sort;

    for i in Mobs do  // format it so that e.g. 4 is 004, 20 is 020 etc.
      Result := Result + Format('%1.3d', [FMobKills[i]]);

    Mobs.Free;
  end;
end;

end.
