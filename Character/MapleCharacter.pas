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

unit MapleCharacter;

interface

uses Types, SysUtils, ZDbcIntfs, MapleItem, MapleMapObject, MapleInventory,
     Generics.Collections, ZDataset, MapleMap, MaplePortal, DatabaseConnection,
     NPCConversation, GameLogic, Math, MTRand, SkillDataProvider, PlayerBuffs,
     BuddyList, MapleParty, EventScript, ScriptHelper, MapleSummon;

const
  MAX_VIEW_RANGE_SQ = 900 * 900;

type
  {$IFNDEF BIGBANG}
  TCharacterType = (ctCygnusKnight, ctAdventurer, ctAran, ctEvan);
  {$ELSE}
  TCharacterType = (ctResistance, ctAdventurer, ctCygnusKnight, ctAran, ctEvan);
  {$ENDIF}

  TMapleSkinColor = (scNormal, scDark, scBlack, scPale, scBlue);

  TMapleStat = (msSkin = 1, msFace, msHair = 4, msLevel = $10, msJob = $20,
                msStr = $40, msDex = $80, msInt = $100, msLuk = $200,
                msHP = $400, msMaxHP = $800, msMP = $1000, msMaxMP = $2000,
                msRemainingAP = $4000, msRemainingSP = $8000, msEXP = $10000,
                msFame = $20000, msMeso = $40000);

  TInventories = array[TMapleInventoryType] of TMapleInventory;

  TFameStatus = (fsNotToday, fsNotThisMonth, fsOK);

  TSavedLocation = (slFreeMarket, slWorldTour, slFlorina, slDimensionalMirror);
  TSavedLocations = array[TSavedLocation] of Integer;

  TKeyBinding = record
    KeyType: Byte;
    Action: Integer;
  end;
  PKeyBinding = ^TKeyBinding;

  TQuickSlot = array[0..7] of Byte;

  TMapleCharacter = class(TAnimatedMapObject)
  private
    FClient: TObject;
    FDB: TDatabaseConnection;
    FID, FAccID: Integer;
    FType: TCharacterType;
    FName: string;
    FMap: TMapleMap;
    FSpawnPoint: Integer;
    FHP, FMaxHP: Integer;
    FMP, FMaxMP: Integer;
    FHair, FFace: Integer;
    FLevel, FFame: Integer;
    FJob: TMapleJob;
    FStr, FDex, FInt, FLuk: Integer;
    FGender: Integer;
    FSkinColor: TMapleSkinColor;
    FExp, FMesos: Integer;
    FRemainingAP, FRemainingSP: Integer;
    FHpApUsed, FMpApUsed: Integer;
    FWorld: Byte;
    FIsGM: Boolean;
    FLastFameTime: TDateTime;
    FExtendedSP: TBytes;
    FJaguar: Byte;

    FInventory: TInventories;
    FQuests: TDictionary<Word, TObject>;  // OMG
    FSkills: TDictionary<TSkill, TPlayerSkillInfo>;
    FKeyMap: TDictionary<Integer, PKeyBinding>;
    FQuickSlot: TQuickSlot;
    FSavedLocations: TSavedLocations;
    FLastMonthFameIDs: TList<Integer>;
    FBuddyList: TBuddyList;
    FAreaData: TDictionary<Integer, string>;
    FParty: TMapleParty;
    FPartyChar: TMaplePartyCharacter;    // current party char object

    FCurConversation: TNPCConversation;
    FControlled: TList<TObject>;
    FUsedPortals: TList<Byte>;
    FVisibleMapObjects: TList<TMapleMapObject>;
    FActiveBuffs: TPlayerActiveBuffs;
    FChair: Integer;
    FShop: Integer;
    FComboCounter: Cardinal;
    FEventChar: TScriptMapleCharacter;
    FEventInstance: TEventInstance;
    FSummons: TList<TMapleSummon>;
    FTrade: TObject;

    // local stats represent current stats of the player to avoid expensive operations
    FLocalMaxHP, FLocalMaxMP: Integer;
    FLocalStr, FLocalDex, FLocalInt, FLocalLuk: Integer;
    FMagic, FWeaponAtk, FLocalMaxBaseDamage: Integer;
    FSpeedMod, FJumpMod: Double;

    procedure Init;

    function CalculateMaxBaseDamage(const WAtk: Integer): Integer;

    function GetMapID: Integer;
    {$HINTS OFF}    // The fucking IDE doesn't see it's used by a property and shows an annoying hint...
    procedure SetHP(const Value: Integer); overload;
    {$HINTS ON}
    procedure SetMP(const Value: Integer);
    procedure SetExp(const Value: Integer);
    procedure SetStr(const Value: Integer);
    procedure SetDex(const Value: Integer);
    procedure SetInt(const Value: Integer);
    procedure SetLuk(const Value: Integer);
    procedure SetMaxHP(const Value: Integer);
    procedure SetMaxMP(const Value: Integer);

    procedure LoseExp;
    procedure SilentEnforceMaxHPMP;
    procedure DefaultQuickSlot;
  public
    constructor CreateDefault(Client: TObject);
    constructor LoadFromDB(ID: Integer; Client: TObject; ChannelServer: Boolean);
    destructor Destroy; override;

    procedure SaveToDB(Update: Boolean);

    function CanUsePortal(const ID: Byte): Boolean;
    procedure ChangeMap(NewMap: TMapleMap); overload;
    procedure ChangeMap(NewMap: TMapleMap; SpawnPoint: TMaplePortal); overload;
    procedure LeaveMap;

    procedure SaveLocation(LType: TSavedLocation);
    procedure ClearSavedLocation(LType: TSavedLocation);
    function GetSavedLocation(LType: TSavedLocation): Integer;

    // This will be returned when accessing the ObjectID property
    function GetObjectID: Integer; override;
    function GetType: TObjectType; override;

    function GetStartedQuests: TList<TObject>;
    function GetCompletedQuests: TList<TObject>;
    function HasStartedQuest(const ID: Integer): Boolean;

    procedure AddHP(Delta: Integer; Reaction: Boolean = False);
    procedure AddMP(Delta: Integer; Reaction: Boolean = False);
    procedure GainEXP(Exp: Integer; Show, InChat: Boolean; White: Boolean = True);
    function ModifyMesos(Delta: Integer; Show: Boolean; EnableActions: Boolean = False; InChat: Boolean = False): Boolean;
    procedure ModifyFame(Change: Integer);
    function IsAlive: Boolean;
    function IsOccupied: Boolean;
    procedure SetHP(const Value: Integer; Silent: Boolean); overload;

    // You can't use 2 class helpers for a class in the same unit, this is for UseSkillHandler
    procedure RemItemByID(ID, Quantity: Integer; Reaction, Consume: Boolean);

    procedure ChangeJob(const Value: TMapleJob);

    procedure LevelUp;

    procedure AddSummon(Value: TMapleSummon);
    procedure ChangeSkillLevel(Skill: TSkill; NewLevel: Byte; SendPacket: Boolean = True);
    function GetSkillLevel(Skill: TSkill): Byte;
    function GetSkillInfo(SkillID: Integer): PSkillLevelInfo;
    function GetHPIncrease: TSkill;
    function GetMPIncrease: TSkill;
    function GetMagicGuard: TSkill;
    function GetPowerGuard: TSkill;
    function HasHPIncrease: Boolean;
    function HasMPIncrease: Boolean;
    function HasMagicGuard: Boolean;

    procedure ChangeKeyBinding(Key: Integer; KeyType: Byte; Action: Integer);

    procedure EnforceMaxHPMP;
    procedure EquipsChanged;

    procedure StartQuest(QuestID, NPC: Integer);
    procedure FinishQuest(QuestID, NPC: Integer);
    procedure ForfeitQuest(QuestID: Integer);
    procedure MobKilled(ID: Integer);

    procedure ControlMonster(Mob: TObject; Aggro, NewSpawn: Boolean);
    procedure StopControllingMonster(Mob: TObject);

    function SendSpawnDataTo(Client: TObject): Boolean; override;
    procedure SendDestroyDataTo(Client: TObject); override;

    procedure RecalcLocalStats;
    procedure SendUpdateStats;
    procedure UpdateSingleStat(Stat: TMapleStat; NewValue: Integer; ItemReaction: Boolean = False);

    function CanGiveFame(Target: TMapleCharacter): TFameStatus;
    procedure HasGivenFame(Target: TMapleCharacter);

    procedure ReceivePartyMemberHP;
    procedure UpdatePartyMemberHP;
    procedure SilentPartyUpdate;

    // for debugging only, puts some info about this in a string
    function ToString: string; override;

    property Client: TObject read FClient;

    property ID: Integer read FID write FID;
    property AccID: Integer read FAccID;
    property CharType: TCharacterType read FType write FType;
    property Name: string read FName write FName;
    property Str: Integer read FStr write SetStr;
    property Dex: Integer read FDex write SetDex;
    property Int: Integer read FInt write SetInt;
    property Luk: Integer read FLuk write SetLuk;
    property Face: Integer read FFace write FFace;
    property Hair: Integer read FHair write FHair;
    property Gender: Integer read FGender write FGender;
    property SkinColor: TMapleSkinColor read FSkinColor write FSkinColor;

    property Level: Integer read FLevel write FLevel;
    property Job: TMapleJob read FJob write FJob;
    property Exp: Integer read FExp write SetExp;
    property Fame: Integer read FFame write FFame;
    property Mesos: Integer read FMesos write FMesos;
    property MapID: Integer read GetMapID;
    property SpawnPoint: Integer read FSpawnPoint;
    property IsGM: Boolean read FIsGM;
    property Jaguar: Byte read FJaguar write FJaguar;

    property HP: Integer read FHP write SetHP;
    property MaxHP: Integer read FMaxHP write SetMaxHP;
    property MP: Integer read FMP write SetMP;
    property MaxMP: Integer read FMaxMP write SetMaxMP;
    property CurrentMaxHP: Integer read FLocalMaxHP;

    property RemainingAP: Integer read FRemainingAP write FRemainingAP;
    property RemainingSP: Integer read FRemainingSP write FRemainingSP;
    property ExtendedSP: TBytes read FExtendedSP;
    property HpApUsed: Integer read FHpApUsed write FHpApUsed;
    property MpApUsed: Integer read FMpApUsed write FMpApUsed;

    property World: Byte read FWorld;
    property Map: TMapleMap read FMap;

    property Inventory: TInventories read FInventory;
    property Quests: TDictionary<Word, TObject> read FQuests;
    property Skills: TDictionary<TSkill, TPlayerSkillInfo> read FSkills;
    property Keymap: TDictionary<Integer, PKeyBinding> read FKeyMap;
    property QuickSlot: TQuickSlot read FQuickSlot write FQuickSlot;
    property BuddyList: TBuddyList read FBuddyList;
    property AreaData: TDictionary<Integer, string> read FAreaData;
    property Party: TMapleParty read FParty write FParty;
    property PartyChar: TMaplePartyCharacter read FPartyChar write FPartyChar;

    property CurConversation: TNPCConversation read FCurConversation write FCurConversation;
    property ControlledMonsters: TList<TObject> read FControlled;
    property UsedPortals: TList<Byte> read FUsedPortals;
    property VisibleMapObjects: TList<TMapleMapObject> read FVisibleMapObjects;
    property ActiveBuffs: TPlayerActiveBuffs read FActiveBuffs;
    property Chair: Integer read FChair write FChair;
    property Shop: Integer read FShop write FShop;
    property ComboCounter: Cardinal read FComboCounter write FComboCounter;
    property EventChar: TScriptMapleCharacter read FEventChar write FEventChar;
    property EventInstance: TEventInstance read FEventInstance write FEventInstance;
    property Summons: TList<TMapleSummon> read FSummons;
    property Trade: TObject read FTrade write FTrade;

    property LocalMaxBaseDamage: Integer read FLocalMaxBaseDamage;
  end;

function CharIDByName(CharName: string; Con: TDatabaseConnection): Integer;

implementation

uses PlayerInventory, MapleClient, Main, MapleServerHandler, MaplePacketCreator,
     Settings, QuestDataProvider, MapleQuest, ItemDataProvider, MapleMonster,
     Skills, Trade;

function CharIDByName(CharName: string; Con: TDatabaseConnection): Integer;
const
  Query = 'SELECT id FROM characters WHERE name = ''%s'';';
var
  Q: TZQuery;
begin
  Q := Con.GetQuery;
  Q.SQL.Text := Format(Query, [CharName]);
  Q.Open;
  try
    if Q.EOF then
      Exit(-1);

    Result := Q.FieldByName('id').AsInteger;
  finally
    Q.Close;
    Q.Free;
  end;
end;

{ TMapleCharacter }

constructor TMapleCharacter.CreateDefault(Client: TObject);

  function CreateKey(const AType, AAction: Integer): PKeyBinding;
  begin
    New(Result);
    Result^.KeyType := AType;
    Result^.Action := AAction;
  end;

begin
  FClient := Client;

  FType := ctAdventurer;
  FHP := 50;
  FMaxHP := 50;
  FMP := 5;
  FMaxMP := 5;

  FSkinColor := scNormal;
  FJob := mjBeginner;

  FLevel := 1;
  FExp := 0;
  FMesos := 0;
  FMap := nil;

  FAccID := TMapleClient(Client).AccID;
  FWorld := TMapleClient(Client).World.Index;
  FIsGM := TMapleClient(Client).IsGM;

  Init;

  FBuddyList := TBuddyList.Create(20, FClient);

  // Default keymap, v75
	FKeyMap.Add(2,  CreateKey(4, 10));
  FKeyMap.Add(3,  CreateKey(4, 12));
	FKeyMap.Add(4,  CreateKey(4, 13));
	FKeyMap.Add(5,  CreateKey(4, 18));
	FKeyMap.Add(6,  CreateKey(4, {$IFNDEF EMS} 24 {$ELSE} 23 {$ENDIF}));
  FKeyMap.Add(7,  CreateKey(4, 21));
  FKeyMap.Add(16, CreateKey(4, 8));
	FKeyMap.Add(17, CreateKey(4, 5));
  FKeyMap.Add(18, CreateKey(4, 0));
	FKeyMap.Add(19, CreateKey(4, 4));
  FKeyMap.Add(23, CreateKey(4, 1));
  FKeyMap.Add(24, CreateKey(4, {$IFNDEF EMS} 25 {$ELSE} 24 {$ENDIF}));
	FKeyMap.Add(25, CreateKey(4, 19));
	FKeyMap.Add(26, CreateKey(4, 14));
	FKeyMap.Add(27, CreateKey(4, 15));
	FKeyMap.Add(29, CreateKey(5, 52));
	FKeyMap.Add(31, CreateKey(4, 2));
  FKeyMap.Add(33, CreateKey(4, {$IFNDEF EMS} 26 {$ELSE} 25 {$ENDIF}));
	FKeyMap.Add(34, CreateKey(4, 17));
	FKeyMap.Add(35, CreateKey(4, 11));
	FKeyMap.Add(37, CreateKey(4, 3));
	FKeyMap.Add(38, CreateKey(4, 20));
	FKeyMap.Add(40, CreateKey(4, 16));
  FKeyMap.Add(41, CreateKey(4, {$IFNDEF EMS} 23 {$ELSE} 22 {$ENDIF}));
	FKeyMap.Add(43, CreateKey(4, 9));
	FKeyMap.Add(44, CreateKey(5, 50));
	FKeyMap.Add(45, CreateKey(5, 51));
	FKeyMap.Add(46, CreateKey(4, 6));
  {$IFDEF EMS}
  FKeyMap.Add(48, CreateKey(4, 21));
  {$ENDIF}
	FKeyMap.Add(50, CreateKey(4, 7));
	FKeyMap.Add(56, CreateKey(5, 53));
  FKeyMap.Add(57, CreateKey(5, 54));
	FKeyMap.Add(59, CreateKey(6, 100));
	FKeyMap.Add(60, CreateKey(6, 101));
	FKeyMap.Add(61, CreateKey(6, 102));
	FKeyMap.Add(62, CreateKey(6, 103));
	FKeyMap.Add(63, CreateKey(6, 104));
	FKeyMap.Add(64, CreateKey(6, 105));
	FKeyMap.Add(65, CreateKey(6, 106));

  DefaultQuickSlot;

  RecalcLocalStats;
end;

destructor TMapleCharacter.Destroy;
var
  i: TMapleInventoryType;
  b: PKeyBinding;
  o: TObject;
  S: TMapleSummon;
begin
  if Assigned(FCurConversation) then
    FreeAndNil(FCurConversation);

  if Assigned(FEventInstance) then
  begin
    FEventInstance.PlayerDisconnected(FEventChar);
    FEventChar.Free;
  end;

  if Assigned(FTrade) then
    TTrade(FTrade).Cancel;

  for S in FSummons do
    S.Free;
  FSummons.Free;

  for i := miEquipped to miCash do
    FreeAndNil(FInventory[i]);

  for o in FQuests.Values do
    TPlayerQuestStatus(o).Free;
  FreeAndNil(FQuests);
  FreeAndNil(FSkills);

  if Assigned(FLastMonthFameIDs) then
    FreeAndNil(FLastMonthFameIDs);

  if Assigned(FBuddyList) then
    FreeAndNil(FBuddyList);

  FreeAndNil(FAreaData);

  if Assigned(FUsedPortals) then
    FreeAndNil(FUsedPortals);

  FreeAndNil(FControlled);
  FreeAndNil(FVisibleMapObjects);
  FreeAndNil(FActiveBuffs);

  for b in FKeyMap.Values do
    Dispose(b);
  FreeAndNil(FKeyMap);

  inherited;
end;

procedure TMapleCharacter.DefaultQuickSlot;
const
  Default: TQuickSlot = (42, 82, 71, 73, 29, 83, 79, 81);
begin
  FQuickSlot := Default;
end;

procedure TMapleCharacter.Init;
var
  i: TMapleInventoryType;
  j: TSavedLocation;
begin
  for i := Low(FInventory) to High(FInventory) do
    FInventory[i] := TMapleInventory.Create(i, frmSettings.seSlotLimit.Value);

  for j := Low(FSavedLocations) to High(FSavedLocations) do
    FSavedLocations[j] := -1;

  FQuests := TDictionary<Word, TObject>.Create;
  FSkills := TDictionary<TSkill, TPlayerSkillInfo>.Create;
  FKeyMap := TDictionary<Integer, PKeyBinding>.Create;
  FEventInstance := nil;
  FEventChar := nil;
  FExtendedSP := nil;
  FLastMonthFameIDs := nil;
  FBuddyList := nil;
  FAreaData := TDictionary<Integer, string>.Create;

  FControlled := TList<TObject>.Create;
  FVisibleMapObjects := TList<TMapleMapObject>.Create;
  FActiveBuffs := TPlayerActiveBuffs.Create(Self);
  FSummons := TList<TMapleSummon>.Create;

  FDB := TMapleClient(FClient).DB;
  FStance := 0;
  FChair := -1;
  FShop := -1;
end;

constructor TMapleCharacter.LoadFromDB(ID: Integer; Client: TObject; ChannelServer: Boolean);
var
  InvType: TMapleInventoryType;
  ItemID: Integer;
  Equip: TEquip;
  Item: TItem;
  Q, MobQ: TZQuery;
  Skill: TSkill;
  Portal: TMaplePortal;
  Quest: TMapleQuest;
  QuestStatus: TPlayerQuestStatus;
  Key: PKeyBinding;
  MapID: Integer;
begin
  FClient := Client;
  FID := ID;

  Init;

  Q := FDB.GetQuery;

  with Q do
  begin
    SQL.Text := 'SELECT * FROM characters WHERE id = :id';
    ParamByName('id').AsInteger := FID;
    Open;

    if Q.EOF then
      raise Exception.CreateFmt('Character %d not in database!', [FID]);

    FAccID := FieldByName('accountid').AsInteger;
    FWorld := FieldByName('world').AsInteger;

    FName := FieldByName('name').AsString;
    FLevel := FieldByName('level').AsInteger;
    FExp := FieldByName('exp').AsInteger;
    FStr := FieldByName('str').AsInteger;
    FDex := FieldByName('dex').AsInteger;
    FInt := FieldByName('int').AsInteger;
    FLuk := FieldByName('luk').AsInteger;

    FHP := FieldByName('hp').AsInteger;
    FMaxHP := FieldByName('maxhp').AsInteger;
    FMP := FieldByName('mp').AsInteger;
    FMaxMP := FieldByName('maxmp').AsInteger;
    FMesos := FieldByName('meso').AsInteger;
    FHpApUsed := FieldByName('hpApUsed').AsInteger;
    FMpApUsed := FieldByName('mpApUsed').AsInteger;

    FJob := TMapleJob(FieldByName('job').AsInteger);
    FSkinColor := TMapleSkinColor(FieldByName('skincolor').AsInteger);
    FGender := FieldByName('gender').AsInteger;
    FFame := FieldByName('fame').AsInteger;
    FHair := FieldByName('hair').AsInteger;
    FFace := FieldByName('face').AsInteger;
    FIsGM := TMapleClient(FClient).IsGM;

    FRemainingSP := FieldByName('sp').AsInteger;
    FRemainingAP := FieldByName('ap').AsInteger;

    MapID := FieldByName('map').AsInteger;
    FSpawnPoint := FieldByName('spawnpoint').AsInteger;

    FInventory[miEquip].SlotLimit := FieldByName('equipSlots').AsInteger;
    FInventory[miUse].SlotLimit := FieldByName('useSlots').AsInteger;
    FInventory[miSetup].SlotLimit := FieldByName('setupSlots').AsInteger;
    FInventory[miEtc].SlotLimit := FieldByName('etcSlots').AsInteger;

    FBuddyList := TBuddyList.Create(FieldByName('buddyCapacity').AsInteger, FClient);

    if ChannelServer then
    begin
      FJaguar := FieldByName('jaguar').AsInteger;

      FUsedPortals := TList<Byte>.Create;

      FMap := TMapleClient(FClient).Channel.MapProvider.LoadMap(MapID);

      // Go to forced return map if it exists
      if (FMap.ForcedReturn < NO_MAP) and (FMap.ID > 0) then  // Don't do this for the adventurer start map (0)
      begin
        FMap := TMapleClient(FClient).Channel.MapProvider.LoadMap(FMap.ForcedReturn);
        FSpawnPoint := 0;
      end;

      if FMap = nil then
      begin
        Log('Map %d not found, warping char to Henesys...', [MapID]);
        FMap := TMapleClient(FClient).Channel.MapProvider.LoadMap(100000000);
      end;

      Portal := FMap.GetPortal(FSpawnPoint);
      if Portal = nil then
      begin
        // char is on a spawnpoint that doesn't exist - select the first spawnpoint instead
        Portal := FMap.GetPortal(0);
        FSpawnPoint := 0;
      end;
      FPosition := Portal.Position;

      FParty := TMapleClient(FClient).World.GetParty(FieldByName('party').AsInteger);
      if FParty <> nil then
      begin
        FPartyChar := FParty.Member[FID];
        // Not in party anymore?
        if FPartyChar = nil then
          FParty := nil;
      end;
    end;

    Close;

    SQL.Text := 'SELECT * FROM items LEFT JOIN equipment USING (inventoryitemid) WHERE characterid = :id';
    ParamByName('id').Value := FID;
    // Do only load equipped items for LoginServer
    if not ChannelServer then
       SQL.Text := SQL.Text + ' AND inventorytype = -1';
    Open;

    while not EOF do
    begin
      InvType := TMapleInventoryType(FieldByName('inventorytype').AsInteger);

      if (InvType = miEquipped) or (InvType = miEquip) then
      begin
        ItemID := FieldByName('itemid').AsInteger;
        Equip := TEquip.Create(ItemID, FieldByName('position').AsInteger);

        with Equip do
        begin
          Owner := FieldByName('owner').AsString;
          Quantity := FieldByName('quantity').AsInteger;
          Acc := FieldByName('acc').AsInteger;
          Avoid := FieldByName('avoid').AsInteger;
          Hands := FieldByName('hands').AsInteger;
          Str := FieldByName('str').AsInteger;
          Dex := FieldByName('dex').AsInteger;
          Int := FieldByName('int').AsInteger;
          Luk := FieldByName('luk').AsInteger;
          Level := FieldByName('level').AsInteger;
          HP := FieldByName('hp').AsInteger;
          MP := FieldByName('mp').AsInteger;
          MAtk := FieldByName('matk').AsInteger;
          MDef := FieldByName('mdef').AsInteger;
          WAtk := FieldByName('watk').AsInteger;
          WDef := FieldByName('wdef').AsInteger;
          Jump := FieldByName('jump').AsInteger;
          Speed := FieldByName('speed').AsInteger;
          UpgradeSlots := FieldByName('upgradeslots').AsInteger;
        end;

        FInventory[InvType].AddFromDB(Equip);
      end
      else
      begin
        Item := MapleItem.TItem.Create(FieldByName('itemid').AsInteger,
                                       FieldByName('position').AsInteger,
                                       FieldByName('quantity').AsInteger);
        Item.Owner := FieldByName('owner').AsString;
        // xxx unigue id

        FInventory[InvType].AddFromDB(Item);
      end;

      Next;
    end;
    Close;

    if IsExtendedSPJob(FJob) then
    begin
      SQL.Text := 'SELECT advancement, points FROM extendedsp WHERE characterid = :id';
      ParamByName('id').Value := FID;
      Open;

      if EOF then
        SetLength(FExtendedSP, 1);

      while not EOF do
      begin
        if FieldByName('advancement').AsInteger > High(FExtendedSP) then
          SetLength(FExtendedSP, FieldByName('advancement').AsInteger + 1);
        FExtendedSP[FieldByName('advancement').AsInteger] := FieldByName('points').AsInteger;
        Next;
      end;
      Close;
    end;

    if ChannelServer then
    begin
      // ======== Quests =========
      SQL.Text := 'SELECT * FROM queststatus WHERE characterid = ' + IntToStr(FID);
      MobQ := FDB.GetQuery;
      MobQ.SQL.Text := 'SELECT * FROM queststatusmobs WHERE queststatusid = :id';
      Open;

      while not EOF do
      begin
     (*   if QuestDataProv.Quests[FieldByName('quest').AsInteger] = nil then
        begin
          Next;
          Continue;
        end;        *)

        Quest := QuestDataProv.Quests[FieldByName('quest').AsInteger];
        QuestStatus := TPlayerQuestStatus.Create(Quest, TQuestStatus(FieldByName('status').AsInteger), FieldByName('quest').AsInteger);
        if FieldByName('time').AsLargeInt > -1 then
          QuestStatus.CompletionTime := FieldByName('time').AsLargeInt;
        QuestStatus.Forfeited := FieldByName('forfeited').AsInteger;
        QuestStatus.Data := FieldByName('specialstatus').AsString;
        FQuests.Add(QuestStatus.ID, QuestStatus);

        MobQ.ParamByName('id').Value := FieldByName('queststatusid').AsInteger;
        MobQ.Open;
        while not MobQ.EOF do
        begin
          if QuestStatus.MobKills.ContainsKey(MobQ.FieldByName('mob').AsInteger) then
            QuestStatus.MobKills[MobQ.FieldByName('mob').AsInteger] :=
                                             MobQ.FieldByName('count').AsInteger;

          MobQ.Next;
        end;
        MobQ.Close;

        Next;
      end;
      Close;
      MobQ.Free;

      // ========= Skills ========
      SQL.Text := 'SELECT * FROM skills WHERE characterid = :id';
      ParamByName('id').Value := FID;
      Open;

      while not EOF do
      begin
        Skill := SkillDataProv.GetPlayerSkill(FieldByName('skillid').AsInteger);
        if Skill <> nil then
          FSkills.Add(Skill, TPlayerSkillInfo.Create(FieldByName('skilllevel').AsInteger, FieldByName('masterlevel').AsInteger))
        else
          Log('No data for skill %d - removed.', [FieldByName('skillid').AsInteger]);

        Next;
      end;
      Close;

      // ========= KeyMap =========
      SQL.Text := 'SELECT `key`, `type`, `action` FROM keymap WHERE characterid = :id';
      ParamByName('id').Value := FID;
      Open;

      while not EOF do
      begin
        New(Key);
        Key^.KeyType := FieldByName('type').AsInteger;
        Key^.Action := FieldByName('action').AsInteger;
        FKeyMap.Add(FieldByName('key').AsInteger, Key);

        Next;
      end;
      Close;

      // ======= Locations =======
      SQL.Text := 'SELECT locationtype, map FROM savedlocations WHERE characterid = :id';
      ParamByName('id').Value := FID;
      Open;

      while not EOF do
      begin
        FSavedLocations[TSavedLocation(FieldByName('locationtype').AsInteger)] :=
          FieldByName('map').AsInteger;

        Next;
      end;
      Close;

      // ====== Famelog ======
      SQL.Text := 'SELECT characterid_to, `when` FROM famelog WHERE characterid = :id AND DATEDIFF(NOW(), `when`) < 30';
      ParamByName('id').Value := FID;
      Open;

      FLastFameTime := 0;
      FLastMonthFameIDs := TList<Integer>.Create;
      while not EOF do
      begin
        FLastFameTime := Max(FLastFameTime, FieldByName('when').AsDateTime);
        FLastMonthFameIDs.Add(FieldByName('characterid_to').AsInteger);

        Next;
      end;
      Close;

      // ====== Area-Data ======
      SQL.Text := 'SELECT infoid, area_data FROM areainfo WHERE charid = :id';
      ParamByName('id').Value := FID;
      Open;

      while not EOF do
      begin
        FAreaData.Add(FieldByName('infoid').AsInteger, FieldByName('area_data').AsString);

        Next;
      end;

      // ======= Buddylist =======
      FBuddyList.LoadFromDatabase(FDB, FID);

      Close;

      // ======= Quick Slot =======
      SQL.Text := 'SELECT quickslot, `key` FROM quickslots WHERE characterid = :id';
      ParamByName('id').Value := FID;
      Open;

      if EOF then
        DefaultQuickSlot
      else
        while not EOF do
        begin
          FQuickSlot[FieldByName('quickslot').AsInteger] := FieldByName('key').AsInteger;
          Next;
        end;
    end;

    Close;
    Free;
  end;

  RecalcLocalStats;
  SilentEnforceMaxHPMP;
end;

procedure TMapleCharacter.SaveToDB(Update: Boolean);
var
  k, LastIns, Mob: Integer;
  i: TMapleInventoryType;
  l: TSavedLocation;
  Item: TItem;
  Equip: TEquip;
  Spawn: TMaplePortal;
  Q, EquipQ, MobQ: TZQuery;
  Obj: TObject;
  Quest: TPlayerQuestStatus;
  Skill: TSkill;
  BLE: PBuddyListEntry;
begin
  try
    try
      // clients should not be able to log back before their old state is saved (see MapleClient#getLoginState) so we are save to switch to a very low isolation level here
      FDB.Con.TransactIsolationLevel := tiReadUncommitted;
      FDB.Con.StartTransaction;
      Q := FDB.GetQuery;

      if Update then
      begin
        Q.SQL.Text := 'UPDATE characters ' +
        'SET level = :lv, fame = :fame, str = :str, dex = :dex, luk = :luk, `int` = :int, exp = :exp, ' +
        'hp = :hp, mp = :mp, maxhp = :mhp, maxmp = :mmp, sp = :sp, ap = :ap, ' +
        'skincolor = :sc, gender = :sex, job = :job, hair = :hair, face = :face, map = :map, meso = :meso, ' +
        'hpApUsed = :hpap, mpApUsed = :mpap, spawnpoint = :spwnp, party = :pty, buddyCapacity = :bc, ' +
        'equipSlots = :es, useSlots = :us, setupSlots = :ss, etcSlots = :etcs, storageSlots = :strgs, jaguar = :jaguar WHERE id = :id';
      end
      else
        Q.SQL.Text :=
        'INSERT INTO characters (level, fame, str, dex, luk, `int`, exp, hp, mp, ' +
        'maxhp, maxmp, sp, ap, skincolor, gender, job, hair, face, map, meso, ' +
        'hpApUsed, mpApUsed, spawnpoint, party, buddyCapacity, equipSlots, useSlots, ' +
        'setupSlots, etcSlots, storageSlots, accountid, name, world) ' +
        'VALUES (:lv, :fame, :str, :dex, :luk, :int, :exp, :hp, :mp, :mhp, :mmp, ' +
        ':sp, :ap, :sc, :sex, :job, :hair, :face, :map, :meso, :hpap, :mpap, :spwnp, ' +
        ':pty, :bc, :es, :us, :ss, :etcs, :strgs, :aid, :name, :world)';

      with Q do
      begin
        ParamByName('lv').Value := FLevel;
        ParamByName('fame').Value := FFame;
        ParamByName('str').Value := FStr;
        ParamByName('dex').Value := FDex;
        ParamByName('int').Value := FInt;
        ParamByName('luk').Value := FLuk;
        ParamByName('exp').Value := FExp;
        ParamByName('hp').Value := FHP;
        ParamByName('mp').Value := FMP;
        ParamByName('mhp').Value := FMaxHP;
        ParamByName('mmp').Value := FMaxMP;
        ParamByName('sp').Value := FRemainingSP;
        ParamByName('ap').Value := FRemainingAP;
        ParamByName('sc').Value := Integer(FSkinColor);
        ParamByName('sex').Value := FGender;
        ParamByName('job').Value := Integer(FJob);
        ParamByName('hair').Value := FHair;
        ParamByName('face').Value := FFace;
        if FMap = nil then
        begin
          case FType of
            ctAdventurer: ParamByName('map').AsInteger := 0;
            ctCygnusKnight: ParamByName('map').Value := 130030000;
            ctAran: ParamByName('map').Value := 914000000;
            ctEvan: ParamByName('map').Value := 900010000;
            {$IFDEF BIGBANG}
            ctResistance: ParamByName('map').Value := 931000000;
            {$ENDIF}
          end;
        end
        else
          ParamByName('map').Value := FMap.ID;
        ParamByName('meso').Value := FMesos;
        ParamByName('hpap').Value := FHpApUsed;
        ParamByName('mpap').Value := FMpApUsed;
        if FMap = nil then
          ParamByName('spwnp').AsInteger := 0
        else
        begin
          Spawn := FMap.FindNearestSpawnpoint(FPosition);
          if Spawn <> nil then
            ParamByName('spwnp').Value := Spawn.ID
          else
            ParamByName('spwnp').AsInteger := 0;
        end;

        if Assigned(FParty) then
          ParamByName('pty').Value := FParty.ID
        else
          ParamByName('pty').AsInteger := -1;
        ParamByName('bc').Value := FBuddyList.Capacity;

        ParamByName('es').AsInteger := FInventory[miEquip].SlotLimit;
        ParamByName('us').AsInteger := FInventory[miUse].SlotLimit;
        ParamByName('ss').AsInteger := FInventory[miSetup].SlotLimit;
        ParamByName('etcs').AsInteger := FInventory[miEtc].SlotLimit;
        ParamByName('strgs').AsInteger := 24;   // xxx Storage slots

        if Update then
        begin
          ParamByName('jaguar').AsInteger := FJaguar;
          ParamByName('id').Value := FID;
        end
        else
        begin
          ParamByName('aid').Value := FAccID;
          ParamByName('name').AsString := FName;
          ParamByName('world').AsInteger := FWorld;
        end;

        ExecSQL;
      end;

      if not Update then
      begin
        Q.SQL.Text := 'SELECT id FROM characters WHERE name = :name;';
        Q.ParamByName('name').AsString := FName;
        Q.Open;
        try
          if not Q.EOF then
            FID := Q.FieldByName('id').AsInteger
          else
          begin
            Log('Creating character failed (not in database)');
            Exit;
          end;
        finally
          Q.Close;
        end;
      end;

      Q.SQL.Text := 'DELETE FROM items WHERE characterid = ' + IntToStr(FID);
      Q.ExecSQL;

      Q.SQL.Text := 'INSERT INTO items VALUES (DEFAULT, :cid, :iid, :type, :pos, :qty, :owner, :pet, null)';

      EquipQ := FDB.GetQuery;   // create a new query
      EquipQ.SQL.Text := 'INSERT INTO equipment VALUES ' +
                         '(DEFAULT, :iid, :slots, :lv, :str, :dex, :int, :luk, ' +
                         ':hp, :mp, :atk, :matk, :def, :mdef, :acc, :avo, :hands, ' +
                         ':spd, :jmp, :ring, 0, 0, 0, 0)';

      with Q do
        for i := miEquipped to miCash do
        begin
          ParamByName('cid').Value := FID;
          ParamByName('type').AsInteger := Integer(FInventory[i].InvType);
          for Item in FInventory[i] do
          begin
            ParamByName('iid').Value := Item.ID;
            ParamByName('pos').AsInteger := Item.Position;
            ParamByName('qty').AsInteger := Item.Quantity;
            ParamByName('owner').AsString := Item.Owner;
            ParamByName('pet').AsInteger := -1;  // xxx

            ExecSQL;

            LastIns := FDB.GetLastInsertedID;
            if LastIns = -1 then
              raise Exception.Create('Inserting char failed!');

            if (FInventory[i].InvType = miEquipped) or (FInventory[i].InvType = miEquip) then
            begin
              Equip := TEquip(Item);
              with Equip, EquipQ do
              begin
                ParamByName('iid').Value := LastIns;
                ParamByName('slots').AsInteger := UpgradeSlots;
                ParamByName('lv').AsInteger := Level;
                ParamByName('str').AsInteger := Str;
                ParamByName('dex').AsInteger := Dex;
                ParamByName('int').AsInteger := Int;
                ParamByName('luk').AsInteger := Luk;
                ParamByName('hp').AsInteger := HP;
                ParamByName('mp').AsInteger := MP;
                ParamByName('atk').AsInteger := WAtk;
                ParamByName('matk').AsInteger := MAtk;
                ParamByName('def').AsInteger := WDef;
                ParamByName('mdef').AsInteger := MDef;
                ParamByName('acc').AsInteger := Acc;
                ParamByName('avo').AsInteger := Avoid;
                ParamByName('hands').AsInteger := Hands;
                ParamByName('spd').AsInteger := Speed;
                ParamByName('jmp').AsInteger := Jump;
                ParamByName('ring').AsInteger := -1;

                ExecSQL;
              end;
            end;
          end;
        end;
      EquipQ.Free;

      // =========== Quests ===========
      Q.SQL.Text := 'DELETE FROM queststatus WHERE characterid = ' + IntToStr(FID);
      Q.ExecSQL;

      Q.SQL.Text := 'INSERT INTO queststatus VALUES (DEFAULT, :cid, :qid, :status, :time, :f, :data)';
      MobQ := FDB.GetQuery;
      MobQ.SQL.Text := 'INSERT INTO queststatusmobs VALUES (DEFAULT, :qsid, :mob, :qty)';
      with Q do
      begin
        ParamByName('cid').Value := FID;
        for Obj in FQuests.Values do
        begin
          Quest := TPlayerQuestStatus(Obj);
          ParamByName('qid').AsInteger := Quest.ID;
          ParamByName('status').Value := Integer(Quest.Status);
          ParamByName('time').AsLargeInt := Quest.CompletionTime;
          ParamByName('f').Value := Quest.Forfeited;
          ParamByName('data').AsString := Quest.Data;
          ExecSQL;

          if not Assigned(Quest.MobKills) then
            Continue;  // No MobKills required

          LastIns := FDB.GetLastInsertedID;
          if LastIns = -1 then
            raise Exception.Create('Updating char failed (quests)!');

          for Mob in Quest.MobKills.Keys do
            with MobQ do
            begin
              ParamByName('qsid').Value := LastIns;
              ParamByName('mob').Value := Mob;
              ParamByName('qty').AsInteger := Quest.MobKills[Mob];
              ExecSQL;
            end;
        end;
      end;
      MobQ.Free;

      // =========== Skills ===========
      Q.SQL.Text := 'DELETE FROM skills WHERE characterid = ' + IntToStr(FID);
      Q.ExecSQL;

      Q.SQL.Text := 'INSERT INTO skills VALUES (DEFAULT, :sid, :cid, :lv, :mlv)';
      with Q do
      begin
        ParamByName('cid').Value := FID;
        for Skill in FSkills.Keys do
        begin
          ParamByName('sid').Value := Skill.ID;
          ParamByName('lv').AsInteger := TPlayerSkillInfo(FSkills[Skill]).Level;
          ParamByName('mlv').AsInteger := TPlayerSkillInfo(FSkills[Skill]).MasterLevel;
          ExecSQL;
        end;
      end;

      // =========== KeyMap ===========
      Q.SQL.Text := 'DELETE FROM keymap WHERE characterid = ' + IntToStr(FID);
      Q.ExecSQL;

      Q.SQL.Text := 'INSERT INTO keymap VALUES (DEFAULT, :cid, :key, :type, :act)';
      with Q do
      begin
        ParamByName('cid').Value := FID;
        for k in FKeyMap.Keys do
        begin
          ParamByName('key').Value := k;
          ParamByName('type').AsInteger := PKeyBinding(FKeyMap[k])^.KeyType;
          ParamByName('act').AsInteger := PKeyBinding(FKeyMap[k])^.Action;
          ExecSQL;
        end;
      end;

      // =========== Locations ===========
      Q.SQL.Text := 'DELETE FROM savedlocations WHERE characterid = ' + IntToStr(FID);
      Q.ExecSQL;

      Q.SQL.Text := 'INSERT INTO savedlocations VALUES (DEFAULT, :cid, :type, :map)';
      with Q do
      begin
        ParamByName('cid').Value := FID;
        for l := Low(FSavedLocations) to High(FSavedLocations) do
        begin
          ParamByName('type').AsInteger := Byte(l);
          ParamByName('map').AsInteger := FSavedLocations[l];
          ExecSQL;
        end;
      end;

      // ========== BuddyList ===========
      Q.SQL.Text := 'DELETE FROM buddies WHERE pending = 0 AND characterid = ' + IntToStr(FID);
      Q.ExecSQL;

      Q.SQL.Text := 'INSERT INTO buddies VALUES (DEFAULT, :cid, :bid, :group, 0)';
      with Q do
      begin
        ParamByName('cid').Value := FID;
        for BLE in FBuddyList do
          if BLE^.Visible then
          begin
            ParamByName('bid').Value := BLE^.CID;
            ParamByName('group').AsString := BLE^.Group;
            ExecSQL;
          end;
      end;

      // ========= Area-Data ==========
      Q.SQL.Text := 'DELETE FROM areainfo WHERE charid = ' + IntToStr(FID);
      Q.ExecSQL;

      Q.SQL.Text := 'INSERT INTO areainfo VALUES (DEFAULT, :id, :iid, :data)';
      Q.ParamByName('id').Value := FID;
      for k in FAreaData.Keys do
      begin
        Q.ParamByName('iid').Value := k;
        Q.ParamByName('data').Value := AnsiString(FAreaData[k]);
        Q.ExecSQL;
      end;
      // ==============================

      Q.SQL.Text := 'DELETE FROM quickslots WHERE characterid = ' + IntToStr(FID);
      Q.ExecSQL;

      Q.SQL.Text := 'INSERT INTO quickslots VALUES (DEFAULT, :id, :qslot, :key)';
      Q.ParamByName('id').AsInteger := FID;
      for k := 0 to 7 do
      begin
        Q.ParamByName('qslot').AsInteger := k;
        Q.ParamByName('key').AsInteger := FQuickSlot[k];
        Q.ExecSQL;
      end;
      // ==============================

      if IsExtendedSPJob(FJob) then
      begin
        Q.SQL.Text := 'DELETE FROM extendedsp WHERE characterid = ' + IntToStr(FID);
        Q.ExecSQL;

        Q.SQL.Text := 'INSERT INTO extendedsp VALUES (DEFAULT, :id, :adv, :sp)';
        Q.ParamByName('id').AsInteger := FID;
        for k := 1 to High(FExtendedSP) do
        begin
          Q.ParamByName('adv').AsInteger := k;
          Q.ParamByName('sp').AsInteger := FExtendedSP[k];
          Q.ExecSQL;
        end;
      end;
      // ==============================

      Q.Free;

      FDB.Con.Commit;
      if not Update then
        Log('Created character %s in database; ID: %d', [FName, FID]);
    except
      Log(Exception(ExceptObject).Message);
    end;
  finally
    FDB.Con.AutoCommit := True;
    FDB.Con.TransactIsolationLevel := tiRepeatableRead;
  end;
end;

function TMapleCharacter.GetMapID: Integer;
begin
  if FMap <> nil then
    Exit(FMap.ID);

  Result := -1;
end;

function TMapleCharacter.GetObjectID: Integer;
begin
  Result := FID;
end;

function TMapleCharacter.GetType: TObjectType;
begin
  Result := otPlayer;
end;

function TMapleCharacter.GetSkillInfo(SkillID: Integer): PSkillLevelInfo;
var
  S: TSkill;
begin
  S := SkillDataProv.GetPlayerSkill(SkillID);
  if not FSkills.ContainsKey(S) then
    Result := nil
  else
    Result := S.Effects[TPlayerSkillInfo(FSkills[S]).Level];
end;

function TMapleCharacter.GetSkillLevel(Skill: TSkill): Byte;
begin
  if not FSkills.ContainsKey(Skill) then
    Result := 0
  else
    Result := TPlayerSkillInfo(FSkills[Skill]).Level;
end;

procedure TMapleCharacter.AddSummon(Value: TMapleSummon);
var
  S, Rem: TMapleSummon;
begin
  Rem := nil;
  for S in FSummons do
    if S.Skill = Value.Skill then
    begin
      FMap.RemoveMapObject(S);
      FMap.BroadcastMessage(S.GetRemovePacket(rmNone));
      Rem := S;
    end;

  if Rem <> nil then
  begin
    FSummons.Remove(Rem);
    Rem.Free;
  end;

  FSummons.Add(Value);
end;

procedure TMapleCharacter.ChangeSkillLevel(Skill: TSkill; NewLevel: Byte; SendPacket: Boolean = True);
var
  Info: TPlayerSkillInfo;
begin
  if FSkills.ContainsKey(Skill) then
  begin
    Info := FSkills[Skill];
    Info.Level := NewLevel;
    FSkills[Skill] := Info;
  end
  else
    FSkills.Add(Skill, TPlayerSkillInfo.Create(NewLevel, 0));

  if SendPacket then
    TMapleClient(FClient).Write(UpdateSkill(Skill.ID, NewLevel, 0));
end;

function TMapleCharacter.GetHPIncrease: TSkill;
begin
  Result := nil;

  case GetJobClass(FJob) of
    mjWarrior: Result := SkillDataProv.GetPlayerSkill(Warrior.ImprovedMaxHPIncrease);
    mjDawnWarrior: Result := SkillDataProv.GetPlayerSkill(DawnWarrior.MaxHPEnhancement);
    // xxx add others
  end;
end;

function TMapleCharacter.GetMPIncrease: TSkill;
begin
  Result := nil;

  case GetJobClass(FJob) of
    mjMagician: Result := SkillDataProv.GetPlayerSkill(Magician.ImprovedMaxMPIncrease);
    mjBlazeWizard: Result := SkillDataProv.GetPlayerSkill(BlazeWizard.IncreasingMaxMP);
  end;
end;

function TMapleCharacter.GetMagicGuard: TSkill;
begin
  Result := nil;

  case GetJobClass(FJob) of
    mjMagician: Result := SkillDataProv.GetPlayerSkill(Magician.MagicGuard);
    mjBlazeWizard: Result := SkillDataProv.GetPlayerSkill(BlazeWizard.MagicGuard)
  end;
end;

function TMapleCharacter.GetPowerGuard: TSkill;
begin
  Result := nil;

  case FJob of
    mjFighter: Result := SkillDataProv.GetPlayerSkill(Fighter.PowerGuard);
    mjPage: Result := SkillDataProv.GetPlayerSkill(Page.PowerGuard)
  end;
end;

function TMapleCharacter.HasHPIncrease: Boolean;
var
  S: TSkill;
begin
  S := GetHPIncrease;
  Result := (S <> nil) and (GetSkillLevel(S) > 0);
end;

function TMapleCharacter.HasMPIncrease: Boolean;
var
  S: TSkill;
begin
  S := GetMPIncrease;
  Result := (S <> nil) and (GetSkillLevel(S) > 0);
end;

function TMapleCharacter.HasMagicGuard: Boolean;
var
  S: TSkill;
begin
  S := GetMagicGuard;
  Result := (S <> nil) and (GetSkillLevel(S) > 0);
end;

procedure TMapleCharacter.ClearSavedLocation(LType: TSavedLocation);
begin
  FSavedLocations[LType] := -1;
end;

function TMapleCharacter.GetSavedLocation(LType: TSavedLocation): Integer;
begin
  Result := FSavedLocations[LType];
end;

procedure TMapleCharacter.SaveLocation(LType: TSavedLocation);
begin
  FSavedLocations[LType] := FMap.ID;
end;

function TMapleCharacter.HasStartedQuest(const ID: Integer): Boolean;
begin
  Result := (FQuests.ContainsKey(ID)) and (TPlayerQuestStatus(FQuests[ID]).Status in [qsStarted, qsAllRequestsDone]);
end;

function TMapleCharacter.GetStartedQuests: TList<TObject>;
var
  Quest: TObject;
begin
  Result := TList<TObject>.Create;
  for Quest in FQuests.Values do
    if TPlayerQuestStatus(Quest).Status in [qsStarted, qsAllRequestsDone] then
      Result.Add(Quest);
end;

function TMapleCharacter.GetCompletedQuests: TList<TObject>;
var
  Quest: TObject;
begin
  Result := TList<TObject>.Create;
  for Quest in FQuests.Values do
    if TPlayerQuestStatus(Quest).Status = qsCompleted then
      Result.Add(Quest);
end;

procedure TMapleCharacter.RecalcLocalStats;
var
  OldMaxHP, Speed, Jump: Integer;
  Item: TItem;
  Equip: TEquip;
  Info: PSkillLevelInfo;
  {$IFDEF BIGBANG}
  Skill, C: Integer;
  S: TSkill;
  {$ENDIF}
begin
  OldMaxHP := FLocalMaxHP;
  FLocalMaxHP := FMaxHP;
  FLocalMaxMP := FMaxMP;
  FLocalStr := FStr;
  FLocalDex := FDex;
  FLocalInt := FInt;
  FLocalLuk := FLuk;

  Speed := 100;
  Jump := 100;

  FMagic := FLocalInt;
  FWeaponAtk := 0;

  {$IFDEF BIGBANG}
  Skill := 0;
  C := Word(FJob) div 100;
  case C of
    1: Skill := Warrior.HPBoost;
    2: Skill := Magician.MPBoost;
    5: Skill := Brawler.HPBoost;
    11: Skill := DawnWarrior.HPBoost;
    12: Skill := BlazeWizard.MPBoost;
    15: Skill := ThunderBreaker.HPBoost;
  end;

  if Skill > 0 then
  begin
    S := SkillDataProv.GetPlayerSkill(Skill);
    if GetSkillLevel(S) > 0 then
    begin
      Info := S.Effects[GetSkillLevel(S)];
      if Info.MaxHP > 0 then
        Inc(FLocalMaxHP, Trunc((FLocalMaxHP / 100) * Info.MaxHP));
      if Info.MaxMP > 0 then
        Inc(FLocalMaxMP, Trunc((FLocalMaxMP / 100) * Info.MaxMP));
    end;
  end;
  {$ENDIF}

  for Item in FInventory[miEquipped] do
  begin
    Equip := TEquip(Item);
    Inc(FLocalMaxHP, Equip.HP);
    Inc(FLocalMaxMP, Equip.MP);
    Inc(FLocalStr, Equip.STR);
    Inc(FLocalDex, Equip.DEX);
    Inc(FLocalInt, Equip.INT);
    Inc(FLocalLuk, Equip.LUK);
    Inc(FMagic, Equip.MAtk + Equip.INT);
    Inc(FWeaponAtk, Equip.WAtk);
    Inc(Speed, Equip.Speed);
    Inc(Jump, Equip.Jump);
  end;

  FMagic := Min(FMagic, 2000);

  Info := ActiveBuffs.GetActiveSkillInfo(Spearman.HyperBody);
  if Info <> nil then
  begin
    Inc(FLocalMaxHP, Trunc(FLocalMaxHP / 100 * Info.X));
    Inc(FLocalMaxMP, Trunc(FLocalMaxMP / 100 * Info.Y));
  end;

  FLocalMaxHP := Min({$IFNDEF BIGBANG}30000{$ELSE}99999{$ENDIF}, FLocalMaxHP);
  FLocalMaxMP := Min({$IFNDEF BIGBANG}30000{$ELSE}99999{$ENDIF}, FLocalMaxMP);

  // xxx all kinds of buffs & skills

  if Speed > 140 then
    Speed := 140;
  if Jump > 123 then
    Jump := 123;
  FSpeedMod := Speed / 100;
  FJumpMod := Jump / 100;

  // xxx Mounts

  FLocalMaxBaseDamage := CalculateMaxBaseDamage(FWeaponAtk);

  if (OldMaxHP <> 0) and (OldMaxHP <> FLocalMaxHP) then
    UpdatePartyMemberHP;
end;

procedure TMapleCharacter.ReceivePartyMemberHP;
var
  MPC: TMaplePartyCharacter;
  Other: TMapleCharacter;
begin
  if FParty <> nil then
    for MPC in FParty do
      if (MPC.MapID = FMap.ID) and (MPC.Channel = TMapleClient(FClient).Channel.Index) then
      begin
        Other := TMapleClient(FClient).Channel.Player[MPC.Name];
        if Other <> nil then
          TMapleClient(FClient).Write(
            MaplePacketCreator.UpdatePartyMemberHP(Other.ID, Other.HP, Other.CurrentMaxHP));
      end;
end;

procedure TMapleCharacter.RemItemByID(ID, Quantity: Integer; Reaction,
  Consume: Boolean);
begin
  RemoveItemByID(ID, Quantity, Reaction, Consume);
end;

procedure TMapleCharacter.GainEXP(Exp: Integer; Show, InChat: Boolean; White: Boolean = True);
begin
  if Exp = 0 then
    Exit;

  if Exp > 0 then
    Exp := Exp * frmSettings.seEXPRate.Value;

  if FLevel < GetMaxLevel(FJob) then
  begin
    Self.EXP := Self.EXP + Exp;
    UpdateSingleStat(msEXP, FExp);
  end;

  if Show then
    TMapleClient(FClient).Write(ShowExpGain(Exp, InChat, White));

  if (FLevel < GetMaxLevel(FJob)) and (FExp >= GetExpNeededAtLevel(FLevel)) then
    LevelUp;
end;

procedure TMapleCharacter.LoseExp;
var
  ExpLoss: Byte;
begin
  if (not IsBeginner(FJob)) and (FLevel < GetMaxLevel(FJob)) then
  begin
    case GetJobClass(FJob) of
      mjThief: ExpLoss := 5;
      mjMagician: ExpLoss := 7;
      else ExpLoss := 10;
    end;

    Exp := Exp - GetExpNeededAtLevel(FLevel) * ExpLoss div 100;
    UpdateSingleStat(msExp, Exp);
  end;
end;

procedure TMapleCharacter.LeaveMap;
begin
  FControlled.Clear;
  FVisibleMapObjects.Clear;

  if FChair <> -1 then
    FChair := -1;
end;

procedure TMapleCharacter.LevelUp;
var
  StatUp: TList<TPair<TMapleStat, Integer>>;
  MaxIncrease: TSkill;
  Used: Integer;
begin
  Inc(FRemainingAP, 5);
  if (IsCygnusJob(FJob)) and (FLevel < 70) then
    Inc(FRemainingAP);   // Cygnus Knights get 6 AP

  if IsBeginner(FJob) then
  begin
    Inc(FMaxHP, Byte(bhBeginner));
    Inc(FMaxMP, Byte(bmBeginner));
  end
  else if IsJobClass(FJob, mjWarrior, True) then
  begin
    Inc(FMaxHP, Byte(bhWarrior));
    Inc(FMaxMP, Byte(bmWarrior));

    if HasHPIncrease then
    begin
      MaxIncrease := GetHPIncrease;
      Inc(FMaxHP, PSkillLevelInfo(MaxIncrease.Effects[GetSkillLevel(MaxIncrease)])^.X);
    end;
  end
  else if IsJobClass(FJob, mjMagician, True) then
  begin
    Inc(FMaxHP, Byte(bhMagician));
    Inc(FMaxMP, Byte(bmMagician));

    if HasMPIncrease then
    begin
      MaxIncrease := GetMPIncrease;
      Inc(FMaxMP, 2 * PSkillLevelInfo(MaxIncrease.Effects[GetSkillLevel(MaxIncrease)])^.X);
    end;
  end
  else if IsJobClass(FJob, mjBowman, True) then
  begin
    Inc(FMaxHP, Byte(bhBowman));
    Inc(FMaxMP, Byte(bmBowman));
  end
  else if IsJobClass(FJob, mjThief, True) then
  begin
    Inc(FMaxHP, Byte(bhThief));
    Inc(FMaxMP, Byte(bmThief));
  end
  else if IsJobClass(FJob, mjPirate, True) then
  begin
    Inc(FMaxHP, Byte(bhPirate));
    Inc(FMaxMP, Byte(bmPirate));
  end
  else if IsJobClass(FJob, mjGM) then
  begin
    Inc(FMaxHP, Byte(bhGM));
    Inc(FMaxMP, Byte(bmGM));
  end
  else if IsJobClass(FJob, mjAran) then
  begin
    Inc(FMaxHP, Byte(bhAran));
    Inc(FMaxMP, Byte(bmAran));
  end
  else if IsJobClass(FJob, mjWildHunter) then
  begin
    Inc(FMaxHP, Byte(bhWildHunter));
    Inc(FMaxMP, Byte(bmBowman));
  end;

  Inc(FMaxHP, Rand.RandInt(Byte(bhVariation)));
  Inc(FMaxMP, Rand.RandInt(Byte(bmVariation)));

  Inc(FMaxMP, FLocalInt div 10);

  Exp := Exp - GetExpNeededAtLevel(FLevel);

  Inc(FLevel, 1);
  if FLevel = GetMaxLevel(FJob) then
    EXP := 0;

  FMaxHP := Min(30000, FMaxHP);
  FMaxMP := Min(30000, FMaxMP);

  StatUp := TList<TPair<TMapleStat, Integer>>.Create;

  // Auto Auto-Assign :D
  if (IsBeginner(FJob)) and (FLevel < 11) then
  begin
    Used := 0;
    if FDex < FLevel then
    begin
      Used := FLevel - FDex;
      FDex := FLevel;
    end;

    // I won't check if Used is negative, that can only happen when someone fucked it up manually :p
    FStr := FStr + (FRemainingAP - Used);
    FRemainingAP := 0;

    StatUp.Add(TPair<TMapleStat, Integer>.Create(msStr, FStr));
    if Used > 0 then
      StatUp.Add(TPair<TMapleStat, Integer>.Create(msDex, FDex));
  end;

  if FRemainingAP > 0 then
    StatUp.Add(TPair<TMapleStat, Integer>.Create(msRemainingAP, FRemainingAP));
  StatUp.Add(TPair<TMapleStat, Integer>.Create(msMaxHP, FMaxHP));
  StatUp.Add(TPair<TMapleStat, Integer>.Create(msMaxMP, FMaxMP));
  StatUp.Add(TPair<TMapleStat, Integer>.Create(msHP, FMaxHP));
  StatUp.Add(TPair<TMapleStat, Integer>.Create(msMP, FMaxMP));
  StatUp.Add(TPair<TMapleStat, Integer>.Create(msEXP, FExp));
  StatUp.Add(TPair<TMapleStat, Integer>.Create(msLevel, FLevel));

  if not IsBeginner(FJob) then
  begin
    if not IsExtendedSPJob(FJob) then
      Inc(FRemainingSP, 3)
    else
    begin
      Used := AdvancementForLevel(FJob, FLevel);  // Make sure the SP goes to the right advancement even when overleveling
      if High(FExtendedSP) < Used then
      begin
        SetLength(FExtendedSP, Used + 1);
        FExtendedSP[Used] := 0;
      end;
      Inc(FExtendedSP[Used], 3);
    end;
    StatUp.Add(TPair<TMapleStat, Integer>.Create(msRemainingSP, FRemainingSP));
  end;

  RecalcLocalStats;

  HP := FMaxHP;
  MP := FMaxMP;
  TMapleClient(FClient).Write(UpdatePlayerStats(StatUp, False, FExtendedSP));
  StatUp.Free;
  FMap.BroadcastMessage(Self, ShowSpecialEffect(FID, seLevelUp), False);
  SilentPartyUpdate;
end;

procedure TMapleCharacter.AddHP(Delta: Integer; Reaction: Boolean = False);
begin
  HP := HP + Delta;
  UpdateSingleStat(msHP, FHP, Reaction);
end;

procedure TMapleCharacter.AddMP(Delta: Integer; Reaction: Boolean = False);
begin
  MP := MP + Delta;
  UpdateSingleStat(msMP, FMP, Reaction);
end;

function TMapleCharacter.IsAlive: Boolean;
begin
  Result := FHP > 0;
end;

function TMapleCharacter.IsOccupied: Boolean;
begin
  Result := (not IsAlive) or (Assigned(FTrade) and TTrade(FTrade).Opened) or
            Assigned(CurConversation) or (FShop > 0);
end;

function TMapleCharacter.CalculateMaxBaseDamage(const WAtk: Integer): Integer;
var
  MaxBaseDamage, MainStat, SecondaryStat: Integer;
  Weapon: TItem;
begin
  if WAtk = 0 then
    Exit(1);

  Weapon := FInventory[miEquipped][esWeapon];
  if (not Assigned(Weapon)) and (not IsJobClass(FJob, mjBrawler)) then
    Exit(0);

  if GetItemType(Weapon.ID) in [itBow, itCrossbow] then
  begin
    MainStat := FLocalDex;
    SecondaryStat := FLocalStr;
  end
  else
  if (IsJobClass(FJob, mjThief)) and (GetItemType(Weapon.ID) in [itClaw, itDagger]) then
  begin
    MainStat := FLocalLuk;
    SecondaryStat := FLocalDex + FLocalStr;
  end
  else
  if (IsJobClass(FJob, mjPirate)) and (GetItemType(Weapon.ID) = itGun) then
  begin
    MainStat := FLocalDex;
    SecondaryStat := FLocalStr;
  end
  else
  begin
    MainStat := FLocalStr;
    SecondaryStat := FLocalDex;
  end;

  MaxBaseDamage := Round(((GetMaxDamageMultiplier(GetItemType(Weapon.ID)) * MainStat + SecondaryStat) / 100) * WAtk);
  Inc(MaxBaseDamage, 10);   // just some saveguard against rounding errors, we want to a/b for this

  Result := MaxBaseDamage;
end;

function TMapleCharacter.CanUsePortal(const ID: Byte): Boolean;
begin
  Result := not FUsedPortals.Contains(ID);
end;

procedure TMapleCharacter.ChangeMap(NewMap: TMapleMap);
begin
  ChangeMap(NewMap, NewMap.GetPortal(0));
end;

procedure TMapleCharacter.ChangeJob(const Value: TMapleJob);
var
  Increase: Byte;
begin
  FJob := Value;

  if FJob <> mjBladeRecruit then
  begin
    Increase := 1;
    if ((Word(FJob) >= 2200) and (Word(FJob) <= 2218)) or ((Word(FJob) mod 10 = 2) and (FJob <> mjBladeSpecialist)) or (FJob = mjBladeMaster) or (FJob > mjCitizen) then
      Inc(Increase, 2);
    if FJob = mjWildHunter then
      Inc(Increase, 2);

    {$IFDEF VERSION89_UP}
    TMapleClient(FClient).Write(ShowSPGain(Increase, FJob));
    {$ENDIF}

    if IsExtendedSPJob(FJob) then
    begin
      if High(FExtendedSP) < AdvancementOf(FJob) then
      begin
        SetLength(FExtendedSP, AdvancementOf(FJob) + 1);
        FExtendedSP[AdvancementOf(FJob)] := 0;
      end;
      Inc(FExtendedSP[AdvancementOf(FJob)], Increase);
    end
    else
      Inc(FRemainingSP, Increase);

    if (Word(FJob) mod 10 >= 1) and (FJob <> mjBladeAcolyte) and (not ((Word(FJob) >= 2211) and (Word(FJob) <= 2218))) then
    begin
      Inc(FRemainingAP, 5);
      UpdateSingleStat(msRemainingAP, FRemainingAP);
    end;

    UpdateSingleStat(msRemainingSP, FRemainingSP);
  end;

  if Word(FJob) mod 10 = 2 then
    Add4thJobSkills(Self);

  UpdateSingleStat(msJob, Word(FJob));
  Map.BroadcastMessage(Self, ShowSpecialEffect(FID, seJobChange), False);
  SilentPartyUpdate;
end;

procedure TMapleCharacter.ChangeKeyBinding(Key: Integer; KeyType: Byte;
  Action: Integer);
var
  b: PKeyBinding;
begin
  //Log('%d %d %d', [Key, KeyType, Action]);
  if KeyType = 0 then
  begin
    if FKeyMap.ContainsKey(Key) then
      FKeyMap.Remove(Key);

    Exit;
  end;

  if FKeyMap.ContainsKey(Key) then
  begin
    PKeyBinding(FKeyMap[Key])^.KeyType := KeyType;
    PKeyBinding(FKeyMap[Key])^.Action := Action;
  end
  else
  begin
    New(b);
    b^.KeyType := KeyType;
    b^.Action := Action;
    FKeyMap.Add(Key, b);
  end;
end;

procedure TMapleCharacter.ChangeMap(NewMap: TMapleMap; SpawnPoint: TMaplePortal);
begin
  if NewMap = nil then
  begin
    Log('[WARNING] ChangeMap - NewMap = nil');
    Exit;
  end;

  FMap.RemovePlayer(Self);
  FUsedPortals.Clear;

  FMap := NewMap;

  if SpawnPoint = nil then
    SpawnPoint := FMap.GetPortal(0);

  TMapleClient(Client).Write(WarpToMap(FMap.ID, SpawnPoint.ID, Self));

  FPosition := SpawnPoint.Position;
  FMap.AddPlayer(Self);

  if FParty <> nil then
  begin
    SilentPartyUpdate;
    UpdatePartyMemberHP;
  end;
end;

function TMapleCharacter.SendSpawnDataTo(Client: TObject): Boolean;
begin
  TMapleClient(Client).Write(SpawnPlayer(Self));

  Result := True;
end;

procedure TMapleCharacter.SendDestroyDataTo(Client: TObject);
begin
  TMapleClient(Client).Write(MaplePacketCreator.RemovePlayer(Self.ID));
end;

procedure TMapleCharacter.SendUpdateStats;
{ Used by job advance scripts }
var
  StatUpdate: TPlayerStatUpdate;
begin
  StatUpdate := TPlayerStatUpdate.Create;
  try
    StatUpdate.Add(TPair<TMapleStat, Integer>.Create(msRemainingAP, FRemainingAP));
    StatUpdate.Add(TPair<TMapleStat, Integer>.Create(msStr, FStr));
    StatUpdate.Add(TPair<TMapleStat, Integer>.Create(msDex, FDex));
    StatUpdate.Add(TPair<TMapleStat, Integer>.Create(msInt, FInt));
    StatUpdate.Add(TPair<TMapleStat, Integer>.Create(msLuk, FLuk));
    StatUpdate.Add(TPair<TMapleStat, Integer>.Create(msHP, FHP));

    TMapleClient(FClient).Write(UpdatePlayerStats(StatUpdate));
  finally
    StatUpdate.Free;
  end;
end;

procedure TMapleCharacter.SetExp(const Value: Integer);
begin
  FExp := Value;

  if FExp < 0 then
    FExp := 0;
end;

procedure TMapleCharacter.SetHP(const Value: Integer);
begin
  SetHP(Value, False);
end;

procedure TMapleCharacter.SetHP(const Value: Integer; Silent: Boolean);
var
  OldHP: Integer;
begin
  OldHP := FHP;

  if Value < 0 then
    FHP := 0
  else if Value > FLocalMaxHP then
    FHP := FLocalMaxHP
  else
    FHP := Value;

  if not Silent then
    UpdatePartyMemberHP;

  if (OldHP > FHP) and (FHP = 0) then
  begin
    LoseExp;
    TMapleClient(FClient).Write(EnableActions);
  end;
end;

procedure TMapleCharacter.SetMP(const Value: Integer);
begin
  if Value < 0 then
    FMP := 0
  else if Value > FLocalMaxMP then
    FMP := FLocalMaxMP
  else
    FMP := Value;
end;

procedure TMapleCharacter.SetMaxHP(const Value: Integer);
begin
  FMaxHP := Value;
  RecalcLocalStats;
end;

procedure TMapleCharacter.SetMaxMP(const Value: Integer);
begin
  FMaxMP := Value;
  RecalcLocalStats;
end;

procedure TMapleCharacter.SetStr(const Value: Integer);
begin
  FStr := Value;
  RecalcLocalStats;
end;

procedure TMapleCharacter.SetDex(const Value: Integer);
begin
  FDex := Value;
  RecalcLocalStats;
end;

procedure TMapleCharacter.SetInt(const Value: Integer);
begin
  FInt := Value;
  RecalcLocalStats;
end;

procedure TMapleCharacter.SetLuk(const Value: Integer);
begin
  FLuk := Value;
  RecalcLocalStats;
end;

procedure TMapleCharacter.SilentEnforceMaxHPMP;
begin
  MP := MP;
  HP := HP;
end;

procedure TMapleCharacter.SilentPartyUpdate;
begin
  if FParty <> nil then
  begin
    FPartyChar.Init(Self);
    TMapleClient(FClient).World.UpdateParty(FParty.ID, poUpdate, FPartyChar);
  end;
end;

function TMapleCharacter.ToString: string;
begin
  Result := sLineBreak + FName + sLineBreak;
  Result := Result + 'Face: ' + IntToStr(FFace) + sLineBreak;
  Result := Result + 'Hair: ' + IntToStr(FHair) + sLineBreak;
  Result := Result + 'Gender: ' + IntToStr(FGender) + sLineBreak;
  Result := Result + 'STR: ' + IntToStr(FSTR) + sLineBreak;
  Result := Result + 'DEX: ' + IntToStr(FDEX) + sLineBreak;
  Result := Result + 'INT: ' + IntToStr(FINT) + sLineBreak;
  Result := Result + 'LUK: ' + IntToStr(FLUK) + sLineBreak;
end;

procedure TMapleCharacter.EnforceMaxHPMP;
var
  Stats: TPlayerStatUpdate;
begin
  Stats := TPlayerStatUpdate.Create;
  try
    if FHP > FLocalMaxHP then
    begin
      SetHP(FHP);
      Stats.Add(TPair<TMapleStat, Integer>.Create(msHP, FHP));
    end;

    if FMP > FLocalMaxMP then
    begin
      SetMP(FMP);
      Stats.Add(TPair<TMapleStat, Integer>.Create(msMP, FMP));
    end;

    if Stats.Count > 0 then
      TMapleClient(FClient).Write(UpdatePlayerStats(Stats));
  finally
    Stats.Free;
  end;
end;

procedure TMapleCharacter.EquipsChanged;
begin
  FMap.BroadcastMessage(Self, UpdateCharLook(Self));
  RecalcLocalStats;
  EnforceMaxHPMP;
end;

procedure TMapleCharacter.StartQuest(QuestID, NPC: Integer);
var
  Status: TPlayerQuestStatus;
begin
  if QuestDataProv.Quests[QuestID] = nil then
  begin
    Log('[Quest] Starting failed, unknown ID %d', [QuestID]);
    Exit;
  end;

  MaplePacketCreator.StartQuest(TMapleClient(FClient), QuestID, NPC);

  if not FQuests.ContainsKey(QuestID) then
  begin
    Status := TPlayerQuestStatus.Create(QuestDataProv.Quests[QuestID], qsStarted, QuestID);
    FQuests.Add(QuestID, Status);
  end
  else
    TPlayerQuestStatus(FQuests[QuestID]).Status := qsStarted;

  // Give Start Rewards
  TMapleQuest(QuestDataProv.Quests[QuestID]).GiveRewards(Self, True);
end;

procedure TMapleCharacter.FinishQuest(QuestID, NPC: Integer);
begin
  if QuestDataProv.Quests[QuestID] = nil then
  begin
    Log('[Quest] Finishing failed, unknown ID %d', [QuestID]);
    Exit;
  end;

  TMapleQuest(QuestDataProv.Quests[QuestID]).GiveRewards(Self, False);

  TPlayerQuestStatus(FQuests[QuestID]).Status := qsCompleted;
  TPlayerQuestStatus(FQuests[QuestID]).CompletionTime := GetServerTime;

  MaplePacketCreator.CompleteQuest(TMapleClient(FClient), QuestID,
                     TMapleQuest(QuestDataProv.Quests[QuestID]).NextQuest, NPC);
end;

procedure TMapleCharacter.ForfeitQuest(QuestID: Integer);
begin
  if (not FQuests.ContainsKey(QuestID)) or (TPlayerQuestStatus(FQuests[QuestID]).Status <> qsStarted) then
    Exit;

  with TPlayerQuestStatus(FQuests[QuestID]) do
  begin
    Status := qsNotStarted;
    Forfeited := Forfeited + 1;
  end;
  TMapleClient(FClient).Write(MaplePacketCreator.ForfeitQuest(QuestID));
end;

procedure TMapleCharacter.MobKilled(ID: Integer);
var
  O: TObject;
  Q: TPlayerQuestStatus;
begin
  for O in GetStartedQuests do
  begin
    Q := TPlayerQuestStatus(O);

    if not Assigned(Q.Quest) then
      Continue;

    // Quest requires Green Mushrooms to be killed - count both, Green Mushrooms & Dejected Green Mushrooms
    if (Q.Quest.MobRequests.ContainsKey(9101000)) and ((ID = 1110100) or (ID = 1110130)) then
      ID := 9101000;    // Regardless of Personality Green Mushroom

    // Zobmie Mushrooms
    if (Q.Quest.MobRequests.ContainsKey(9101001)) and ((ID = 2230101) or (ID = 2230131)) then
      ID := 9101001;    // Regardless of Personality Zombie Mushroom

    // Ghost Stumps
    if (Q.Quest.MobRequests.ContainsKey(9101002)) and ((ID = 1140100) or (ID = 1140130)) then
      ID := 9101002;    // Regardless of Personality Ghost Stump

    if Q.Quest.MobRequests.ContainsKey(ID) then
      if not Q.MobKills.ContainsKey(ID) then
      begin
        Q.MobKills.Add(ID, 1);
        TMapleClient(FClient).Write(UpdateQuest(Q.ID, Q.GetQuestData));
      end
      else  // add one, if not already killed the amount that was required
        if Q.MobKills[ID] < Q.Quest.MobRequests[ID] then
        begin
          Q.MobKills[ID] := Q.MobKills[ID] + 1;
          TMapleClient(FClient).Write(UpdateQuest(Q.ID, Q.GetQuestData));

          if Q.MobKills[ID] >= Q.Quest.MobRequests[ID] then
            Q.CheckDone(FClient);   // display red quest complete message
        end;
  end;
end;

function TMapleCharacter.ModifyMesos(Delta: Integer; Show: Boolean; EnableActions: Boolean = False; InChat: Boolean = False): Boolean;
begin
  if FMesos + Delta < 0 then
  begin
    TMapleClient(FClient).Write(MaplePacketCreator.EnableActions);
    Exit(False);
  end;

  Inc(FMesos, Delta);
  UpdateSingleStat(msMeso, FMesos, EnableActions);

  if Show then
    TMapleClient(FClient).Write(ShowMesoGain(Delta, InChat));

  Result := True;
end;

procedure TMapleCharacter.ControlMonster(Mob: TObject; Aggro, NewSpawn: Boolean);
begin
  TMapleMonster(Mob).Controller := Self;
  FControlled.Add(Mob);
  TMapleClient(FClient).Write(
    MaplePacketCreator.ControlMonster(TMapleMonster(Mob), NewSpawn, Aggro));
end;

procedure TMapleCharacter.StopControllingMonster(Mob: TObject);
begin
  FControlled.Remove(Mob);
  TMapleClient(FClient).Write(MaplePacketCreator.StopControllingMonster(TMapleMonster(Mob).ObjectID));
end;

procedure TMapleCharacter.UpdatePartyMemberHP;
var
  MPC: TMaplePartyCharacter;
  Other: TMapleCharacter;
begin
  if FParty <> nil then
    for MPC in FParty do
      if (MPC.MapID = FMap.ID) and (MPC.Channel = TMapleClient(FClient).Channel.Index) then
      begin
        Other := TMapleClient(FClient).Channel.Player[MPC.Name];
        if Other <> nil then
          TMapleClient(Other.Client).Write(
            MaplePacketCreator.UpdatePartyMemberHP(FID, FHP, FLocalMaxHP));
      end;
end;

procedure TMapleCharacter.UpdateSingleStat(Stat: TMapleStat; NewValue: Integer;
  ItemReaction: Boolean = False);
var
  StatUpdate: TList<TPair<TMapleStat, Integer>>;
begin
  StatUpdate := TList<TPair<TMapleStat, Integer>>.Create;
  try
    StatUpdate.Add(TPair<TMapleStat, Integer>.Create(Stat, NewValue));
    TMapleClient(FClient).Write(UpdatePlayerStats(StatUpdate, ItemReaction, FExtendedSP));
  finally
    StatUpdate.Free;
  end;
end;

function TMapleCharacter.CanGiveFame(Target: TMapleCharacter): TFameStatus;
begin
  if Now < FLastFameTime + 1 then
    Result := fsNotToday
  else if FLastMonthFameIDs.Contains(Target.ID) then
    Result := fsNotThisMonth
  else
    Result := fsOK;
end;

procedure TMapleCharacter.HasGivenFame(Target: TMapleCharacter);
begin
  FLastFameTime := Now;
  FLastMonthFameIDs.Add(Target.ID);

  with FDB.GetQuery do
    try
      SQL.Text := 'INSERT INTO famelog (characterid, characterid_to) VALUES (:id, :toid)';
      ParamByName('id').Value := FID;
      ParamByName('toid').Value := Target.ID;
      ExecSQL;
    finally
      Free;
    end;
end;

procedure TMapleCharacter.ModifyFame(Change: Integer);
begin
  Inc(FFame, Change);
end;

end.
