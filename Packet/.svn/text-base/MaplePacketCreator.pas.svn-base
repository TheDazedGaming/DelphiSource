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

unit MaplePacketCreator;

interface

uses Windows, Classes, SysUtils, Generics.Collections, WinSock, MapleStream,
     PlayerInventory, MapleCharacter, MapleInventory, MapleItem, MapleServerHandler,
     Utils, Generics.Defaults, MapleClient, MovementParser, AttackParser, MapleNPC,
     GameLogic, MapleMonster, DropHandler, MTRand, SkillDataProvider, Buffs, PlayerBuffs,
     BuffDataProvider, WorldServer, MapleReactor, ShopDataProvider, ItemDataProvider,
     BuddyList, MapleParty, Math, Skills, BuffStat;

const
  {$IFNDEF EMS}
  WorldNames: array[0..4] of string = ('Scania', 'Bera', 'Broa', 'Windia', 'Khaini');
  {$ELSE}
  WorldNames: array[0..4] of string = ('Kradia', 'Demethos', 'Broa', 'Windia', 'Khaini');
  {$ENDIF}

type
  TPinOperation = (poAccepted, poRegister, poInvalid, poConError, poEnter);

  TServerStatus = (ssNormal, ssHighlyPopulated, ssFull);

  TDeleteCharState = (dsOK, dsDBError, dsDateWrong = $12);

  TServerMessageType = (smNotice, smPopup, smMegaphone, smSuperMegaphone,
                        smScrollingTicker, smPinkText, smLightBlueText);

  TSpecialEffect = (seLevelUp, seSkill, sePartySkill, {$IFNDEF BIGBANG}seItemGain{$ELSE}seItemGain = 5{$ENDIF},
                    sePortal = seItemGain + 4, seJobChange, seQuestComplete,
                    seWZEffect = seQuestComplete + 9, seWZEffect2 = seWZEffect + 5);

  TDropStatus = (dsShowNewSecond, dsShowNewFirst, dsShowExisting, dsUntradeable);

  TPartyStatusMessage = (pmAlreadyJoined = $10, pmFull, pmCharNotFound = $13,
                         pmTakingCareOfInvitation = $16, pmRequestDenied = $17);

  TStatusInfoMode = (siInventory, siQuest, siExp = 3, {$IFDEF VERSION89_UP}siSP,{$ENDIF} siFame,
                     siMesos, siGuildPoints, siText = siFame + {$IFNDEF AFTERSHOCK}5{$ELSE}6{$ENDIF},
                     siAreaInfo);

  TPlayerStatUpdate = TList<TPair<TMapleStat, Integer>>;

function GetServerTime: Int64;

// ======= Login-Server =======
function GetHello(RecvIV, SendIV: TIVArray): TMapleStream;
{$IFDEF EMS}
function EMSCrypto: TMapleStream;
function EMSCryptoEnd: TMapleStream;
{$ENDIF}
function GetLoginFailed(Reason: TLoginStatus): TMapleStream;
function GetAuthSuccessRequestPin(Client: TMapleClient): TMapleStream;
function PinOperation(Mode: TPinOperation): TMapleStream;
function PinAssigned: TMapleStream;
function GetServerList(World: TWorldServer): TMapleStream;
function GetEndOfServerList: TMapleStream;
function GetServerStatus(Status: TServerStatus): TMapleStream;
function GetCharList(Client: TMapleClient; WorldID: Byte): TMapleStream;
function NameCheckResponse(Name: string; NameUsed: Boolean): TMapleStream;
procedure AddCharEntry(var Packet: TMapleStream; Char: TMapleCharacter; IsViewAll: Boolean = False);
procedure AddCharStats(var Packet: TMapleStream; Char: TMapleCharacter);
procedure AddCharLook(var Packet: TMapleStream; Char: TMapleCharacter; Mega: Boolean);
function AddNewCharEntry(Char: TMapleCharacter; Error: Boolean): TMapleStream;
function DeleteCharResponse(CharID: Integer; State: TDeleteCharState): TMapleStream;
function ShowAllCharacters(CharCount: Integer; Unk: Integer): TMapleStream;
function ShowAllCharactersInfo(WorldID: Integer; Chars: TList<TMapleCharacter>): TMapleStream;
function GetServerIP(InetAddr: In_Addr; Port: Word; CharID: Integer): TMapleStream;
function PICWrong: TMapleStream;

// =======  Channel-Server =======

// General character info
function GetCharInfo(Char: TMapleCharacter): TMapleStream;
procedure AddInventoryInfo(var Packet: TMapleStream; Char: TMapleCharacter);
procedure AddItemInfo(var Packet: TMapleStream; Item: TItem; AddPosition: Boolean = True);
function KeymapPacket(Map: TDictionary<Integer, PKeyBinding>): TMapleStream;
function QuickSlotPacket(Slot: TQuickSlot): TMapleStream;
function ChangeChannel(InetAddr: In_Addr; Port: Word): TMapleStream;
function CharInfoResponse(Char: TMapleCharacter; IsSelf: Boolean): TMapleStream;

// General server methods
procedure SerializeMovements(var Packet: TMapleStream; Moves: TList<TLifeMovement>);
function ServerMessage(MsgType: TServerMessageType; Channel: Byte; Msg: string): TMapleStream;

// Direct player actions
function SpawnPlayer(Char: TMapleCharacter): TMapleStream;
function RemovePlayer(CharID: Integer): TMapleStream;
function MovePlayer(CharID: Integer; Moves: TList<TLifeMovement>): TMapleStream;
function CloseRangeAttack(Attack: TAttackInfo): TMapleStream;
function FacialExpression(CharID, Face: Integer): TMapleStream;
function ChatText(CharIDFrom: Integer; Text: string; GM, BubbleOnly: Boolean): TMapleStream;
function WarpToMap(Map, SpawnPoint: Integer; Char: TMapleCharacter): TMapleStream;
function DamagePlayer(From, Direction, Stance: ShortInt; MonsterIDFrom, CharID, Damage, NoDamageSkill: Integer): TMapleStream;
function RangedAttack(Attack: TAttackInfo; Projectile: Integer): TMapleStream;
function MagicAttack(Attack: TAttackInfo): TMapleStream;
function EnergyChargeAttack(Attack: TAttackInfo): TMapleStream;
function GroupChat(From, Msg: string; Mode: Byte): TMapleStream;

// Infos & Effects
function InstructionBubble(Msg: string; Width, Height: Integer): TMapleStream;
function TrembleEffect(Mode, Delay: Integer): TMapleStream;
function PlayWZSound(Name: string): TMapleStream;
function ShowMapEffect(Name: string): TMapleStream;
function ShowItemGain(ID, Quantity: Integer; InChat: Boolean): TMapleStream;
function ShowEXPGain(Gain: Integer; InChat, White: Boolean; Party: Byte = 1): TMapleStream;
function ShowFameGain(Gain: Integer): TMapleStream;
function ShowMesoGain(Gain: Integer; InChat: Boolean): TMapleStream;
{$IFDEF VERSION89_UP}
function ShowSPGain(Gain: Byte; Job: TMapleJob): TMapleStream;
{$ENDIF}
function ShowEquipEffect: TMapleStream;
function ShowSpecialEffect(Effect: TSpecialEffect): TMapleStream; overload;
function ShowSpecialEffect(CharID: Integer; Effect: TSpecialEffect): TMapleStream; overload;
function ShowOrangeText(Msg: string): TMapleStream;
function ShowWZEffect(Path: string; AdditionalInfo: Integer = -1): TMapleStream;
function UpdateAreaInfo(InfoID: Integer; NewList: string): TMapleStream;
function ShowAranComboCounter(Count: Cardinal): TMapleStream;
function ShowGuideEffectWithText(Text: string; Width, Duration: Integer): TMapleStream;
function ShowGuideEffect(ID, Duration: Integer): TMapleStream;
function ShowInfoOnScreen(Msg: string): TMapleStream;
function ShowClock(Seconds: Integer): TMapleStream;

// Player stats & looks
function UpdatePlayerStats(Stats: TPlayerStatUpdate; Reaction: Boolean = False): TMapleStream; overload;  // Workaround for XE
function UpdatePlayerStats(Stats: TPlayerStatUpdate; Reaction: Boolean; ExtendedSP: TBytes): TMapleStream; overload;
function UpdateCharLook(Char: TMapleCharacter): TMapleStream;
function EnableActions(Reaction: Boolean = False): TMapleStream;
function PortalBlocked: TMapleStream;
function StatUpdateOK: TMapleStream;
function AranTutorialMaxStats: TMapleStream;
function RemoveTemporaryStats: TMapleStream;
procedure AddJaguarInfo(var Packet: TMapleStream; NewID: Byte);
function UpdateJaguar(NewID: Byte): TMapleStream;

// Quests
procedure StartQuest(Client: TMapleClient; Quest: Word; NPC: Integer);
function UpdateQuest(Quest: Word; Data: string): TMapleStream;
procedure CompleteQuest(Client: TMapleClient; QuestID, NextQuest: Word; NPC: Integer; Silent: Boolean = False);
function ForfeitQuest(Quest: Word): TMapleStream;
function ShowQuestComplete(Quest: Word): TMapleStream;

// Inventory
function ShowInventoryStatus(Mode: Integer): TMapleStream;
function InventoryFull: TMapleStream;
function ShowInventoryFull: TMapleStream;
function MoveInventoryItem(InvType: TMapleInventoryType; Src, Dst: ShortInt): TMapleStream; overload;
function MoveInventoryItem(InvType: TMapleInventoryType; Src, Dst, Equip: ShortInt): TMapleStream; overload;
function MoveAndMergeInventoryItem(InvType: TMapleInventoryType; Src, Dst: ShortInt; Total: SmallInt): TMapleStream;
function MoveAndMergeWithRestInventoryItem(InvType: TMapleInventoryType; Src, Dst: ShortInt; SrcQ, DstQ: SmallInt): TMapleStream;
function UpdateInventorySlot(InvType: TMapleInventoryType; Item: TItem; Reaction: Boolean = False): TMapleStream;
function AddInventorySlot(InvType: TMapleInventoryType; Item: TItem; Reaction: Boolean = False): TMapleStream;
function ClearInventorySlot(InvType: TMapleInventoryType; Slot: Integer; Reaction: Boolean = False): TMapleStream;
function DropInventoryItemUpdate(InvType: TMapleInventoryType; Item: TItem): TMapleStream;
function DropInventoryItem(InvType: TMapleInventoryType; Src: ShortInt): TMapleStream;
function UpdateInventorySlotLimit(Inventory: TMapleInventory): TMapleStream;

// Monsters
function SpawnMonster(Life: TMapleMonster; NewSpawn: Boolean; SummonEffect: Integer = 0): TMapleStream;
function ControlMonster(Life: TMapleMonster; NewSpawn: Boolean; Aggro: Boolean = False): TMapleStream;
function MoveMonsterResponse(ObjectID, MoveID, CurrentMP: Integer; UseSkills: Boolean; SkillID: Integer = 0; SkillLevel: Integer = 0): TMapleStream;
function MoveMonster(UseSkills: Boolean; Action, SkillID, SkillLv, Delay, OID: Integer; StartPos: TPoint; Moves: TList<TLifeMovement>): TMapleStream;
function ShowMonsterHP(OID, RemHPPercentage: Integer): TMapleStream;
function KillMonster(OID: Integer; Animation: Boolean): TMapleStream;
function StopControllingMonster(OID: Integer): TMapleStream;
function ApplyMobStatus(OID, StatusMask: Integer; Info: TList<TMobStatus>; Delay: Word): TMapleStream;
function RemoveMobStatus(OID, Status: Integer): TMapleStream;
function ShowMagnet(OID: Integer; Success: Boolean): TMapleStream;
function DamageMonster(OID, Damage: Integer): TMapleStream;

// Drops
function ShowDrop(Drop: TDrop; Status: TDropStatus; Origin: TPoint): TMapleStream;
function RemoveDrop(Drop: TDrop): TMapleStream;
function DontTake: TMapleStream;
function DropNotAvailableForPickup: TMapleStream;
function CantGetAnymoreItems: TMapleStream;
function TakeDrop(DropID, PlayerID: Integer; PetIndex: ShortInt = -1): TMapleStream;

// Skills
function UpdateSkill(SkillID, NewLevel, MasterLevel: Integer; Reaction: Boolean = True): TMapleStream;
function ShowSkill(Player: TMapleCharacter; SkillID: Integer; Level, EffectID: Byte): TMapleStream;
procedure UseBuff(Client: TMapleClient; SkillID, Time: Integer; PSkill: PActiveBuff; MSkill: PActiveMapBuff; AddedInfo: SmallInt);
procedure UseMount(Client: TMapleClient; SkillID: Integer; PSkill: PActiveBuff; MSkill: PActiveMapBuff; MountID: Integer);
procedure UsePirateBuff(Client: TMapleClient; SkillID, Time: Integer; PSkill: PActiveBuff; MSkill: PActiveMapBuff);
procedure EndBuff(Client: TMapleClient; ToEnd: TBuffStat);
function ShowHPHealBySkill(Delta: Word): TMapleStream;
function ShowOwnBuffEffect(Player: TMapleCharacter; SkillID, SkillLv, EffectID: Integer; AdditionalInfo: Byte = 0): TMapleStream;

// Reactors
function SpawnReactor(Reactor: TMapleReactor): TMapleStream;
function TriggerReactor(Reactor: TMapleReactor): TMapleStream;
function DestroyReactor(Reactor: TMapleReactor): TMapleStream;

// Chairs
function ShowChair(CharID, ItemID: Integer): TMapleStream;
function ChairAction(ID: SmallInt = -1): TMapleStream;

// Shops
function ShowShop(ID: Integer; Shop: PShopInfo): TMapleStream;
function ItemBought(Msg: Byte): TMapleStream;

// Fame
function GiveFameResponse(Mode: Byte; CharName: string; NewFame: SmallInt): TMapleStream;
function GiveFameErrorResponse(Status: Byte): TMapleStream;
function ReceiveFame(Mode: Byte; CharNameFrom: string): TMapleStream;

// Summons
function SpawnTutorialSummon(Spawn: Boolean): TMapleStream;

// UI stuff
function LockUI(Disabled: Boolean): TMapleStream;
function DisableUI(Disabled: Boolean): TMapleStream;

// Whisper/Find
function Whisper(Sender, Text: string; Channel: Byte): TMapleStream;
function WhisperResponse(Target: string; Success: Boolean): TMapleStream;
function FindReplyWithMap(Target: string; MapID: Integer): TMapleStream;
function FindReply(Target: string; Channel: Byte): TMapleStream;

// Party
function PartyCreated(ID: Integer): TMapleStream;
function UpdateParty(Channel: Integer; Party: TMapleParty; Op: TPartyOperation; Target: TMaplePartyCharacter): TMapleStream;
function PartyStatusMessage(Msg: TPartyStatusMessage; CharName: string = ''): TMapleStream;
function PartyStringMessage(Msg: string): TMapleStream;
function PartyInvite(From: TMapleCharacter): TMapleStream;
function UpdatePartyMemberHP(CID, HP, MaxHP: Integer): TMapleStream;

implementation

uses Main, Settings, MapleQuest;

function GetServerTime: Int64;
var
  SystemTime: _SYSTEMTIME;
  FileTime: _FILETIME;
  uli: ULARGE_INTEGER;
begin
  GetSystemTime(SystemTime);
  SystemTimeToFileTime(SystemTime, FileTime);
  uli.LowPart := FileTime.dwLowDateTime;
  uli.HighPart := FileTime.dwHighDateTime;
  Result := uli.QuadPart;
end;

function GetHello(RecvIV, SendIV: TIVArray): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(0);   // Size, will be set at the end
    WriteShort(MAPLE_VERSION);

    WriteMapleAnsiString(PATCH_REVISION);

    Write(RecvIV, 4);
    Write(SendIV, 4);

    WriteByte(MAPLE_LOCALE);

    // go back to beginning to write the size
    Seek(0, soBeginning);
    WriteShort(Size - 2);
  end;
end;

{$IFDEF EMS}
function EMSCrypto: TMapleStream;
const
  RSA_KEY = '30819D300D06092A864886F70D010101050003818B0030818702818100C3958E342' +
            '480A7D749C7ED6254621DF8DE2063DDE716D6EA2AE6F1B22A2819A45E45B2859771' +
            '6A26A9BD478A06A3749934FFE1E91F7A274EE1A7758E53D11AE633738A878130577' +
            'F8E649B9F007B083F3CBBBE89751E21472DD51F8ACB421EA23E81489A2DA77E7124' +
            '605B060F528FCAC38EE6B58FD43C5BEA5F2CCE746A2253020111';
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['EMSCryptoResp']);
    WriteMapleAnsiString(RSA_KEY);
  end;
end;

function EMSCryptoEnd: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['EMSCryptoEnd']);

    WriteShort(0);
    WriteByte(0);
  end;
end;
{$ENDIF}

function GetLoginFailed(Reason: TLoginStatus): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['LoginStatus']);

    WriteInt(Integer(Reason));
    WriteShort(0);
  end;
end;

function GetAuthSuccessRequestPin(Client: TMapleClient): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['LoginStatus']);

    WriteInt(0);
    WriteShort(0);
    WriteInt(Client.AccID);
    WriteByte(Client.Gender);    // 10 = Gender Select; 11 = Set Pin for Account
    WriteBool(Client.IsGM);   // Admin byte, enables commands like /c /ch /m /h but disables trading.
    WriteShort(0);
    {$IFDEF BIGBANG}
    WriteByte(0);
    {$ENDIF}

    WriteMapleAnsiString(Client.AccountName);
    WriteByte(0);
    WriteByte(0);    // IsQuietBanned
    WriteInt64(0);   // QuietBanTime
    WriteInt64(TimeToTick(Client.CreationTime));
    {$IFNDEF EMS}
    WriteInt(1);  // 0 and it will display a crappy "Select the world you want to play in" which doesn't fit into the new loginscreen
    // 0 = Pin-System Enabled, 1 = Disabled
    WriteBool((not frmSettings.cbEnablePins.Checked) or (frmSettings.cbEnablePIC.Checked and (Client.PIC <> '')));
    if not frmSettings.cbEnablePIC.Checked then
      WriteByte(2)    // 0 = Register PIC, 1 = Ask for PIC, 2 = Disabled
    else
      WriteBool(Client.PIC <> '');   // 0 if it's empty (register)
    {$ELSE}
    WriteByte(0);   // Doesn't seem to matter at all
    {$ENDIF}
    {$IFDEF VERSION88_UP}
    WriteInt64(0);
    {$ENDIF}
  end;
end;

function PinOperation(Mode: TPinOperation): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PinOperation']);

    WriteByte(Mode);
  end;
end;

function PinAssigned: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PinAssigned']);

    WriteByte(0);
  end;
end;

function GetServerList(World: TWorldServer): TMapleStream;
var
  i: Integer;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ServerList']);

    WriteByte(World.Index);   // Server ID: 0 = Scania; 1 = Bera; 2 = Broa and so on
    WriteMapleAnsiString(World.Name);
    WriteByte(0);    // Ribbon: 1 = E; 2 = N; 3 = H
    WriteMapleAnsiString(frmSettings.edtEventMsg.Text);

		WriteShort(100);  // rate modifier, don't ask O.O!
		WriteShort(100);  // rate modifier, don't ask O.O!
    {$IFNDEF EMS}
		WriteByte(0);
    {$ENDIF}

    WriteByte(World.Channels.Count);
    for i := 0 to World.Channels.Count - 1 do
    begin
      WriteMapleAnsiString(World.Name + '-' + IntToStr(i + 1));

      // Here we can see the last 7 bytes of each of the 14 channels.  [EMS]
    // L  O  A  D |Sv|ID|LANGUAGE
   (* '00 03 00 00 00 00 05 ' +
      '56 01 00 00 00 01 05 ' +
      '5C 01 00 00 00 02 05 ' +
      '1E 01 00 00 00 03 00 ' +
      'FB 00 00 00 00 04 00 ' +
      'EE 00 00 00 00 05 00 ' +
      'F9 00 00 00 00 06 02 ' +
      'B4 00 00 00 00 07 02 ' +
      '46 FF FF FF 00 08 03 ' +
      'FA FF FF FF 00 09 03 ' +
      '49 01 00 00 00 0A 04 ' +
      '12 01 00 00 00 0B 04 ' +
      '88 00 00 00 00 0C 01 ' +
      '73 00 00 00 00 0D 01 ' +
      '00 00');   *)

      WriteInt(0);         // xxx Channel-Population (Load)
      WriteByte(World.Index);
      WriteByte(i);
      // Language: 0 = English; 1 = French; 2 = German; 3 = Spanish; 4 = NL; 5 = Europe (All)
      WriteByte({$IFNDEF EMS} 0 {$ELSE} 5 {$ENDIF});
      // ===================
    end;

    WriteShort(0);       // End
    {$IFDEF CHAOS}
    WriteInt(0);
    {$ENDIF}
  end;
end;

function GetEndOfServerList: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ServerList']);

    WriteByte($FF);
  end;
end;

function GetServerStatus(Status: TServerStatus): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ServerStatus']);

    WriteShort(Integer(Status));
  end;
end;

function GetCharList(Client: TMapleClient; WorldID: Byte): TMapleStream;
var
  Chars: TList<TMapleCharacter>;
  i: Integer;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['CharList']);

    WriteByte(0);

    Chars := Client.LoadCharacters(WorldID);
    WriteByte(Chars.Count);
    for i := 0 to Chars.Count - 1 do
      AddCharEntry(Result, Chars[i]);

    {$IFNDEF EMS}
    // Same byte as in GetAuthSuccess o.o
    if not frmSettings.cbEnablePIC.Checked then
      WriteByte(2)    // 0 = Register PIC, 1 = Ask for PIC, 2 = Disabled
    else
      WriteBool(Client.PIC <> '');   // 0 if it's empty (register)
    {$ENDIF}
    WriteInt(8);   // max chars

    WriteInt64(0);   // Int in 88 but oh well...

    for i := 0 to Chars.Count - 1 do
      Chars[i].Free;
    Chars.Free;
  end;
end;

function NameCheckResponse(Name: string; NameUsed: Boolean): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['NameCheckResponse']);

    WriteMapleAnsiString(Name);
    WriteBool(NameUsed);
  end;
end;

procedure AddCharEntry(var Packet: TMapleStream; Char: TMapleCharacter; IsViewAll: Boolean = False);
begin
  AddCharStats(Packet, Char);
  AddCharLook(Packet, Char, False);

  if not IsViewAll then
    Packet.WriteByte(0);
  Packet.WriteByte(0);   // world rank disabled (next 4 ints are not sent if disabled)
end;


procedure AddSPArray(var Packet: TMapleStream; SP: TBytes);
var
  i: Integer;
begin
  Packet.WriteByte(Max(Length(SP) - 1, 0));
  for i := 1 to High(SP) do
  begin
    Packet.WriteByte(i);
    Packet.WriteByte(SP[i]);
  end;
end;

procedure AddCharStats(var Packet: TMapleStream; Char: TMapleCharacter);
var
  i: Integer;
begin
  with Packet do
  begin
    WriteInt(Char.ID);

    WriteAnsiString(Char.Name);
    // fill to maximum name length
    for i := Length(Char.Name) to 12 do
      WriteByte(0);

    WriteByte(Char.Gender);    // gender (0 = male, 1 = female)
    WriteByte(Char.SkinColor);
    WriteInt(Char.Face);
    WriteInt(Char.Hair);
    WriteInt64(0);
    WriteInt64(0);
    WriteInt64(0);
    WriteByte(Char.Level);
    WriteShort(Word(Char.Job));
    WriteShort(Char.STR);
    WriteShort(Char.DEX);
    WriteShort(Char.INT);
    WriteShort(Char.LUK);
    {$IFNDEF BIGBANG}
    WriteShort(Char.HP);
    WriteShort(Char.MaxHP);
    WriteShort(Char.MP);
    WriteShort(Char.MaxMP);
    {$ELSE}
    WriteInt(Char.HP);
    WriteInt(Char.MaxHP);
    WriteInt(Char.MP);
    WriteInt(Char.MaxMP);
    {$ENDIF}
    WriteShort(Char.RemainingAP);
    if not IsExtendedSPJob(Char.Job) then
      WriteShort(Char.RemainingSP)
    else
      AddSPArray(Packet, Char.ExtendedSP);
    WriteInt(Char.EXP);
    {$IFNDEF CHAOS}
    WriteShort(Char.Fame);
    {$ELSE}
    WriteInt(Char.Fame);
    {$ENDIF}
    {$IFNDEF EMS}
    WriteInt(0);
    {$ENDIF}
    WriteInt(Char.MapID);
    WriteByte(Char.SpawnPoint);
    WriteInt(0);
    {$IFDEF VERSION88_UP}
    WriteShort(0);   // Char.IsDualBlade
    {$ENDIF}

    {$IFDEF CHAOS}  // uh... yeah... right.
    WriteInt64(0);  // 1, 4, 4, 4, 4, 4, 4, 4, buf(0xc), 4, 1, 4, 1
    WriteInt64(0);
    WriteInt64(0);
    WriteInt64(0);
    WriteInt64(0);
    WriteInt64(0);
    WriteShort(0);
    WriteByte(0);
    {$ENDIF}
    {$IFDEF ASCENSION}
    WriteInt(0);
    {$ENDIF}
  end;
end;

procedure AddCharLook(var Packet: TMapleStream; Char: TMapleCharacter; Mega: Boolean);
var
  Item: TItem;
  Equips, MaskedEquips: TDictionary<Byte, Integer>;
  Pos: Byte;
begin
  with Packet do
  begin
    WriteByte(Char.Gender);
    WriteByte(Char.SkinColor);
    WriteInt(Char.Face);
    {$IFDEF CHAOS}
    WriteInt(0);
    {$ENDIF}
    WriteBool(not Mega);
    WriteInt(Char.Hair);

    Equips := TDictionary<Byte, Integer>.Create;
    MaskedEquips := TDictionary<Byte, Integer>.Create;
    try
      for Item in Char.Inventory[miEquipped] do
      begin
        Pos := Item.Position * -1;
        if (Pos < 100) and (not Equips.ContainsKey(Pos)) then
          Equips.Add(Pos, Item.ID)
        else
        if (Pos > 100) and (Pos <> 111) then
        begin
          Dec(Pos, 100);
          if Equips.ContainsKey(Pos) then
            MaskedEquips.AddOrSetValue(Pos, Equips[Pos]);
          Equips.AddOrSetValue(Pos, Item.ID);
        end
        else
        if Equips.ContainsKey(Pos) then
          MaskedEquips.Add(Pos, Item.ID);
      end;

      // Write visible items
      for Pos in Equips.Keys do
      begin
        WriteByte(Pos);
        WriteInt(Equips[Pos]);
      end;
      WriteByte($FF);   // End of visible items

      // Write invisible items
      for Pos in MaskedEquips.Keys do
      begin
        WriteByte(Pos);
        WriteInt(Equips[Pos]);
      end;
      WriteByte($FF);
    finally
      Equips.Free;
      MaskedEquips.Free;
    end;

    // Cash weapon
    if Char.Inventory[miEquipped][-111] <> nil then
      WriteInt(Char.Inventory[miEquipped][-111].ID)
    else
      WriteInt(0);

    WriteInt(0);
    WriteInt64(0);
  end;
end;

function AddNewCharEntry(Char: TMapleCharacter; Error: Boolean): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['AddNewCharEntry']);

    WriteBool(Error);
    AddCharEntry(Result, Char);
  end;
end;

function DeleteCharResponse(CharID: Integer; State: TDeleteCharState): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['DeleteCharResponse']);

    WriteInt(CharID);
    WriteByte(State);
  end;
end;

function ShowAllCharacters(CharCount: Integer; Unk: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ViewAllChars']);

    WriteByte(1);
    WriteInt(CharCount);
    WriteInt(Unk);
  end;
end;

function ShowAllCharactersInfo(WorldID: Integer; Chars: TList<TMapleCharacter>): TMapleStream;
var
  i: Integer;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ViewAllChars']);

    WriteByte(0);
    WriteByte(WorldID);
    WriteByte(Chars.Count);
    for i := 0 to Chars.Count - 1 do
      AddCharEntry(Result, Chars[i], True);
  end;
end;

function GetServerIP(InetAddr: In_Addr; Port: Word; CharID: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ServerIP']);

    WriteShort(0);
    WriteInt(InetAddr.S_addr);
    WriteShort(Port);
    WriteInt(CharID);   // this gets repeated to the channel server

    WriteByte(0);
    WriteInt(0);
  end;
end;

function ChangeChannel(InetAddr: In_Addr; Port: Word): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ChangeChannel']);

    WriteByte(1);
    WriteInt(InetAddr.S_addr);
    WriteShort(Port);
  end;
end;

procedure AddChannelInfo(Packet: TMapleStream; Channel: Integer);
begin
  with Packet do
  begin
    {$IFDEF VERSION88_UP}
    WriteShort(2);
    WriteInt64(1);
    WriteInt64(2);
    {$ENDIF}
    WriteInt(Channel);
    {$IFDEF ASCENSION}
    WriteByte(0);
    {$ENDIF}
    {$IFDEF VERSION88_UP}
    WriteInt(0);
    {$ENDIF}
  end;
end;

function PICWrong: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PICWrong']);

    WriteByte(20);   // can be any value o.o - It always displays PIC incorrect.
  end;
end;

procedure AddExpirationTime(Packet: TMapleStream; Expire: TDateTime; AddZero: Boolean = True);
const                            // False, True
  ShowValues: array[Boolean] of Byte = (2, 1);
var
  ShowExpiration: Boolean;
begin
  if AddZero then
    Packet.WriteByte(0);

  ShowExpiration := False;
  Packet.WriteShort($0580);
  if Expire <> 0 then
  begin
    Packet.WriteInt(0);   // xxx do Korean shit
    ShowExpiration := True;
  end
  else
    Packet.WriteInt(400967355);

  Packet.WriteByte(ShowValues[ShowExpiration]);
end;

procedure AddSkillInfo(var Packet: TMapleStream; Char: TMapleCharacter);
var
  Skill: TSkill;
begin
  // Skills
  Packet.WriteShort(Char.Skills.Count);
  for Skill in Char.Skills.Keys do
  begin
    Packet.WriteInt(Skill.ID);
    Packet.WriteInt(TPlayerSkillInfo(Char.Skills[Skill]).Level);

    AddExpirationTime(Packet, 0);

    if Skill.IsFourthJob then
      Packet.WriteInt(TPlayerSkillInfo(Char.Skills[Skill]).MasterLevel);
  end;

  // xxx Cooldowns
  Packet.WriteShort(0);
end;

procedure AddQuestInfo(var Packet: TMapleStream; Char: TMapleCharacter);
var
  Started, Completed: TList<TObject>;
  i: Integer;
begin
  Started := Char.GetStartedQuests;
  try
    Packet.WriteShort(Started.Count);
    for i := 0 to Started.Count - 1 do
    begin
      Packet.WriteShort(TPlayerQuestStatus(Started[i]).ID);
      Packet.WriteMapleAnsiString(TPlayerQuestStatus(Started[i]).GetQuestData);
    end;
  finally
    Started.Free;
  end;

  Completed := Char.GetCompletedQuests;
  try
    Packet.WriteShort(Completed.Count);
    for i := 0 to Completed.Count - 1 do
    begin
      Packet.WriteShort(TPlayerQuestStatus(Completed[i]).ID);
      {$IF DEFINED(CHAOS) AND NOT DEFINED(ASCENSION)}
      Packet.WriteShort(0);
      {$IFEND}
      Packet.WriteInt64(TPlayerQuestStatus(Completed[i]).CompletionTime);
    end;
  finally
    Completed.Free;
  end;
end;

function GetCharInfo(Char: TMapleCharacter): TMapleStream;
var
  i: Integer;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['WarpToMap']);

    AddChannelInfo(Result, TMapleClient(Char.Client).Channel.Index);
    WriteByte(1);
    {$IFDEF AFTERSHOCK}
    WriteInt(0);
    {$ENDIF}
    WriteByte(1);
    WriteShort(0);
    WriteInt(Rand.RandInt(MAXINT));
    WriteInt(Rand.RandInt(MAXINT));
    WriteInt(Rand.RandInt(MAXINT));
    WriteInt64({$IFNDEF FELLOWSHIP} -1 {$ELSE} Int64($FFFFFFFDFFFFFFFF) {$ENDIF});
    WriteByte(0);
    {$IFDEF BIGBANG}
    WriteByte(0);
    {$IFDEF CHAOS}
    WriteInt(0);
    WriteByte(0);
    {$ENDIF}
    {$ENDIF}

    AddCharStats(Result, Char);

    WriteByte(Char.BuddyList.Capacity);
    WriteByte(0);  // 1 = Send Link-Name
    // xxx Link
    //WriteMapleAnsiString(LinkName);
    {$IFDEF AFTERSHOCK}
    WriteShort(0);  // Probably has to do with that new Cygnus extension
    {$ENDIF}

    AddInventoryInfo(Result, Char);
    {$IFDEF CHAOS}
    WriteInt(0);
    WriteByte(0);
    {$ENDIF}
    AddSkillInfo(Result, Char);
    AddQuestInfo(Result, Char);

    WriteInt64(0);

    {$IFDEF FELLOWSHIP}
    WriteShort(0);
    {$ENDIF}

    // Teleport Rock maps
    for i := 0 to {$IFNDEF AFTERSHOCK} 14 {$ELSE}{$IFNDEF CHAOS} 27 {$ELSE} 40 {$ENDIF}{$ENDIF} do
      WriteInt(NO_MAP);

    {$IFNDEF BIGBANG}
    // Monsterbook
    WriteInt(0);
    WriteShort(0);
    WriteShort(0);
    WriteByte(0);
    {$ELSE}
    WriteByte(0);  // 4
    {$IFDEF VERSION100_UP}
    WriteInt(0);   // 1
    WriteShort(0);  // Card Count
    WriteInt(-1);
    WriteInt(0);
    {$ELSE}
    WriteByte(0);
    {$ENDIF}
    {$ENDIF}

    {$IFDEF FELLOWSHIP}
    WriteByte(0);
    WriteInt(0);
    {$ENDIF}

    // AreaData e.g. can contain information about the Aran Intro, which hints were already displayed etc.
    WriteShort(Char.AreaData.Count);
    for i in Char.AreaData.Keys do
    begin
      WriteShort(i);
      WriteMapleAnsiString(Char.AreaData[i]);
    end;

    if (Char.Job >= mjWildHunter) and (Char.Job <= mjWildHunter4) then
      AddJaguarInfo(Result, Char.Jaguar);

    WriteShort(0);
    {$IFDEF VERSION88_UP}
    WriteInt64(0);
    WriteInt64(0);
    {$IFDEF VERSION89_UP}
    WriteShort(0);
    {$ENDIF}
    {$ENDIF}
    WriteInt64(GetServerTime);
    {$IFDEF AFTERSHOCK}
    WriteInt(100);  // "Oooh, let's add random crap to the very end of the packet"
    {$IFDEF CHAOS}
    WriteShort(0);
    {$ENDIF}
    {$ENDIF}
  end;
end;

procedure AddInventoryInfo(var Packet: TMapleStream; Char: TMapleCharacter);
var
  Equipped, EquippedCash: TList<TItem>;
  Item: TItem;
  i: TMapleInventoryType;
begin
  with Packet do
  begin
    WriteInt(Char.Mesos);

    for i := miEquip to miCash do
      WriteByte(Char.Inventory[i].SlotLimit);

    WriteHex('00 40 E0 FD 3B 37 4F 01');  // V80 now also needs that crap here

    Equipped := TList<TItem>.Create(TComparer<TItem>.Construct(CompareItem));
    EquippedCash := TList<TItem>.Create;
    try
      for Item in Char.Inventory[miEquipped] do
        if Item.Position > -100 then
          Equipped.Add(Item)
        else
          EquippedCash.Add(Item);

      Equipped.Sort;

      for Item in Equipped do
        AddItemInfo(Packet, Item);

      WriteShort(0);  // End of equipped

      for Item in EquippedCash do
        AddItemInfo(Packet, Item);

      WriteShort(0);  // End of equipped cash
    finally
      FreeAndNil(Equipped);
      FreeAndNil(EquippedCash);
    end;

    for Item in Char.Inventory[miEquip] do
      AddItemInfo(Packet, Item);
    WriteInt(0);     // End of Equip
    {$IFDEF BIGBANG}
    WriteShort(0);
    {$IFDEF CHAOS}
    WriteShort(0);
    {$ENDIF}
    {$ENDIF}

    for Item in Char.Inventory[miUse] do
      AddItemInfo(Packet, Item);
    WriteByte(0);    // End of Use

    for Item in Char.Inventory[miSetup] do
      AddItemInfo(Packet, Item);
    WriteByte(0);    // End of Setup

    for Item in Char.Inventory[miEtc] do
      AddItemInfo(Packet, Item);
    WriteByte(0);    // End of Etc

    for Item in Char.Inventory[miCash] do
      AddItemInfo(Packet, Item);
    WriteByte(0);    // End of Cash

    {$IFDEF AFTERSHOCK}
    WriteInt(-1);
    {$ENDIF}
  end;
end;

procedure AddItemInfo(var Packet: TMapleStream; Item: TItem; AddPosition: Boolean = True);
var
  Equip: TEquip;
  Pos{$IFNDEF VERSION88_UP}, i{$ENDIF}: ShortInt;
  IsCash: Boolean;
begin
  IsCash := ItemDataProv.IsCashItem(Item.ID);
  Pos := Item.Position;

  if AddPosition then
  begin
    if Pos < 0 then
      Pos := Pos * -1;

    if Pos > 100 then
      Dec(Pos, 100);

    if Item.ItemType = itEquip then
      Packet.WriteShort(Pos)
    else
      Packet.WriteByte(Pos);
  end;

  Packet.WriteByte(Item.ItemType);
  Packet.WriteInt(Item.ID);

  Packet.WriteBool(IsCash);
  if IsCash then
    Packet.WriteInt64(1);
  AddExpirationTime(Packet, 0);

  {$IFDEF AFTERSHOCK}
  Packet.WriteInt(-1);
  {$ENDIF}

  if Item.ItemType <> itEquip then
  begin
    Packet.WriteShort(Item.Quantity);
    Packet.WriteMapleAnsiString(Item.Owner);
    Packet.WriteShort(0);   // xxx Flags

    if IsRechargeable(Item.ID) then
      Packet.WriteInt64(0);

    Exit;
  end;

  Equip := TEquip(Item);
  with Equip, Packet do
  begin
    WriteByte(UpgradeSlots);
    WriteByte(Level);    // xxx Scrolls?
    WriteShort(STR);
    WriteShort(DEX);
    WriteShort(INT);
    WriteShort(LUK);
    WriteShort(HP);
    WriteShort(MP);
    WriteShort(WAtk);
    WriteShort(MAtk);
    WriteShort(WDef);
    WriteShort(MDef);
    WriteShort(Acc);
    WriteShort(Avoid);
    WriteShort(Hands);
    WriteShort(Speed);
    WriteShort(Jump);
    WriteMapleAnsiString(Owner);
    WriteShort(0);    // xxx Flags: 0 = normal; 1 = locked

    {$IFNDEF VERSION88_UP}
    if IsCash then
      for i := 0 to 9 do
        WriteByte($40)
    else
    begin
      WriteByte(0);
      WriteByte(0);    // Item level?
      WriteShort(0);
      WriteShort(0);
      WriteInt(0);    // Hammers
      WriteInt64(-1);
    end;
    {$ELSE}
    WriteByte(0);
    WriteByte(0);    // Item level?
    WriteInt(0);
    WriteInt(-1);
    WriteInt(0);   // Hammers
    {$IFDEF CHAOS}
    WriteShort(0);
    {$ENDIF}
    WriteShort(0);  // Potential Status: 0 = None, 1..4 = Hidden, 5 = Rare, 6 = Epic, 7 = Unique
    WriteShort(0);  // Potential Stat 1
    WriteShort(0);  //                2
    WriteShort(0);  //                3
    WriteInt(0);
    if not IsCash then
      WriteInt64(-1);
    {$ENDIF}
  end;

  Packet.WriteHex('00 40 E0 FD 3B 37 4F 01');  // Some kind of magic?
  Packet.WriteInt(-1);
end;

function CharInfoResponse(Char: TMapleCharacter; IsSelf: Boolean): TMapleStream;
var
  Quests: TList<Word>;
  i: Integer;
{$IFDEF VERSION88_UP}
  Chairs: TList<Integer>;
  Item: TItem;
{$ENDIF}
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['CharInfoResponse']);

    WriteInt(Char.ID);
    WriteByte(Char.Level);
    WriteShort(Word(Char.Job));
    WriteShort(Char.Fame);
    WriteBool(False);   // xxx Married

    WriteMapleAnsiString('-');   // xxx Guild
    WriteMapleAnsiString('');    // xxx Alliance

    WriteBool(IsSelf);

    // xxx add pet data
    WriteByte(0);   // end of pet data / start of taming mob

    // xxx add mount data
    WriteByte(0);   // end

    WriteByte(0);   // Wishlist size

    // xxx monster book
    WriteInt(1);    // Level
    WriteInt(0);    // Normals
    WriteInt(0);    // Specials
    WriteInt(0);    // Size
    WriteInt(0);    // Cover

    // Medal List
    if Char.Inventory[miEquipped][esMedal] <> nil then
      WriteInt(Char.Inventory[miEquipped][esMedal].ID)
    else
      WriteInt(0);
    Quests := TList<Word>.Create;
    try
      // 29900 ~ 29923 are Medal quests
      for i := 0 to 23 do
        if (Char.Quests.ContainsKey(29900 + i)) and (TPlayerQuestStatus(Char.Quests[29900 + i]).Status = qsCompleted) then
          Quests.Add(29900 + i);
      Quests.Sort;

      WriteShort(Quests.Count);
      for i := 0 to Quests.Count - 1 do
        WriteShort(Quests[i]);
    finally
      Quests.Free;
    end;

    // Chair List
    {$IFDEF VERSION88_UP}
    Chairs := TList<Integer>.Create;
    for Item in Char.Inventory[miSetup] do
      if Item.ID div 10000 = 301 then
        Chairs.Add(Item.ID);
    WriteInt(Chairs.Count);
    for i in Chairs do
      WriteInt(Chairs[i]);
    {$ENDIF}
  end;
end;

function SpawnPlayer(Char: TMapleCharacter): TMapleStream;
var
  Enter: TMapEntryBuffs;
  Stat: TBuffStat;
  Value: Smallint;
  SortedStats: TList<TBuffStat>;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['SpawnPlayer']);

    WriteInt(Char.ID);
    WriteByte(Char.Level);
    WriteMapleAnsiString(Char.Name);

    WriteMapleAnsiString('');  // guild name
    WriteShort(0);  // LogoBG
    WriteByte(0);   // LogoBGColor
    WriteShort(0);  // Logo
    WriteByte(0);   // LogoColor

    // -- Buff Info --
    Enter := Char.ActiveBuffs.MapEntryBuffs^;

    Enter.Stat.Serialize(Result);

    SortedStats := TList<TBuffStat>.Create(Enter.Values.Keys);
    SortedStats.Sort;
    for Stat in SortedStats do
    begin
      if TMapEntryVals(Enter.Values[Stat]).Debuff then
      begin
        WriteShort(TMapEntryVals(Enter.Values[Stat]).Skill);
        WriteShort(TMapEntryVals(Enter.Values[Stat]).Val);
      end
      else if TMapEntryVals(Enter.Values[Stat]).Use then
      begin
        Value := TMapEntryVals(Enter.Values[Stat]).Val;
        if Stat = COMBO then
          WriteByte(Char.ActiveBuffs.Combo + 1)
        else if Stat = WK_CHARGE then
          WriteInt(Char.ActiveBuffs.Charge)
        else if Stat = MORPH then
          WriteShort(Value)
        else
          WriteByte(Value);
      end;
    end;
    SortedStats.Free;

    WriteByte(0);
    WriteByte(0);
    {$IFDEF CHAOS}
    WriteByte(0);
    {$ENDIF}
    // ---------------------

    WriteShort(Word(Char.Job));

    AddCharLook(Result, Char, False);
    WriteInt(0);

    {$IFDEF VERSION88_UP}
    WriteInt(0);
    WriteInt(0);
    WriteInt(0);
    {$ENDIF}

    WriteInt(0);   // Item Effect
    WriteInt(Char.Chair);
    WritePos(Char.Position);
    WriteByte(Char.Stance);
    WriteShort(0);   // Char.Fh
    WriteByte(0);

    // xxx Write pet data here

    WriteByte(0);
    WriteInt(1);
    WriteInt(0);
    WriteInt(0);
    WriteShort(0);

    // xxx ring thingy

    WriteInt(0);
    WriteByte(0);
    WriteInt(0);
  end;
end;

function RemovePlayer(CharID: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['RemovePlayer']);

    WriteInt(CharID);
  end;
end;

function ServerMessage(MsgType: TServerMessageType; Channel: Byte; Msg: string): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ServerMessage']);

    WriteByte(MsgType);

    if MsgType = smScrollingTicker then
      WriteByte(1);    // GMS sends the scrolling ticker packet everytime, this is 0 when there currently is no message

    WriteMapleAnsiString(Msg);

    if MsgType = smSuperMegaphone then
    begin
      WriteByte(Channel - 1);
      WriteByte(0);   // xxx MegaEar?
    end;

    if MsgType = smLightblueText then
      WriteInt(0);
  end;
end;

procedure SerializeMovements(var Packet: TMapleStream; Moves: TList<TLifeMovement>);
var
  Move: TLifeMovement;
begin
  Packet.WriteByte(Moves.Count);
  for Move in Moves do
    Move.Serialize(Packet);
end;

function MovePlayer(CharID: Integer; Moves: TList<TLifeMovement>): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['MovePlayer']);

    WriteInt(CharID);
    WriteInt(0);
    {$IFDEF VERSION88_UP}
    WriteInt(0);
    {$ENDIF}
    SerializeMovements(Result, Moves);
  end;
end;

procedure AddAttackBody(var Packet: TMapleStream; Projectile: Integer; Attack: TAttackInfo);
var
  Dmg: TDamagePair;
  EachDmg: Integer;
begin
  with Packet do
  begin
    WriteInt(Attack.Player.ID);
    WriteByte(Attack.NumAttackedAndDamage);

    WriteByte(Attack.Player.Level);

    WriteByte(Attack.SkillLevel);
    if Attack.Skill > 0 then
      WriteInt(Attack.Skill);

    WriteByte(Attack.Display);
    WriteByte(Attack.UnkV80);
    WriteByte(Attack.Stance);
    WriteByte(Attack.WeaponSpeed);
    WriteByte(0);
    WriteInt(Projectile);

    for Dmg in Attack.AllDamage do
      if Assigned(Dmg.Value) then
      begin
        WriteInt(Dmg.Key);   // ObjectID
        WriteByte(6);
        {$IFDEF BIGBANG}
        WriteByte(0);
        {$ENDIF}

        for EachDmg in Dmg.Value do
          if Attack.Skill = 3221007 then
            WriteInt(Cardinal(EachDmg) + $80000000)    // highest bit set = crit
          else
            WriteInt(EachDmg);
      end;
  end;
end;

function CloseRangeAttack(Attack: TAttackInfo): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['CloseRangeAttack']);

    AddAttackBody(Result, 0, Attack);
  end;
end;

function RangedAttack(Attack: TAttackInfo; Projectile: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['RangedAttack']);

    AddAttackBody(Result, Projectile, Attack);

    WriteInt64(0);  // Position or some crap
  end;
end;

function MagicAttack(Attack: TAttackInfo): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['MagicAttack']);

    AddAttackBody(Result, 0, Attack);

    if Attack.Charge > 0 then
      WriteInt(Attack.Charge);
  end;
end;

function EnergyChargeAttack(Attack: TAttackInfo): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['EnergyChargeAttack']);

    AddAttackBody(Result, 0, Attack);
  end;
end;

function FacialExpression(CharID, Face: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['FaceExpression']);

    WriteInt(CharID);
    WriteInt(Face);
    {$IFDEF VERSION88_UP}
    WriteInt(-1);
    WriteByte(0);
    {$ENDIF}
  end;
end;

function ChatText(CharIDFrom: Integer; Text: string; GM, BubbleOnly: Boolean): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PublicChat']);

    WriteInt(CharIDFrom);
    WriteBool(GM);  // White background
    WriteMapleAnsiString(Text);
    WriteBool(BubbleOnly);
  end;
end;

function WarpToMap(Map, SpawnPoint: Integer; Char: TMapleCharacter): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['WarpToMap']);

    AddChannelInfo(Result, TMapleClient(Char.Client).Channel.Index);
    WriteInt(0);
    {$IFDEF AFTERSHOCK}
    WriteInt(0);
    {$ENDIF}
    WriteByte(0);
    WriteInt(Map);
    WriteByte(SpawnPoint);
    {$IFNDEF BIGBANG}
    WriteShort(Char.HP);
    {$ELSE}
    WriteInt(Char.HP);
    {$ENDIF}
    WriteByte(0);
    WriteInt64(GetServerTime);
    {$IFDEF AFTERSHOCK}
    WriteInt(100);
    {$IFDEF CHAOS}
    WriteShort(0);
    {$ENDIF}
    {$ENDIF}
  end;
end;

function DamagePlayer(From, Direction, Stance: ShortInt; MonsterIDFrom, CharID, Damage,
  NoDamageSkill: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['DamagePlayer']);

    WriteInt(CharID);
    WriteByte(From);
    WriteInt(Damage);
    WriteInt(MonsterIDFrom);
    WriteByte(Direction);

    WriteByte(0);   // Power Guard shit

    WriteByte(Stance);
    WriteInt(Damage);

    if NoDamageSkill > 0 then
      WriteInt(NoDamageSkill);
  end;
end;

function ShowEquipEffect: TMapleStream;
begin
  Result := TMapleStream.Create;      // I like this kind of packet :D
  Result.WriteShort(OpHandler.SendOps['ShowEquipEffect']);
end;

function EnableActions(Reaction: Boolean = False): TMapleStream;
var
  L: TPlayerStatUpdate;
begin
  L := TPlayerStatUpdate.Create;
  try
    Result := UpdatePlayerStats(L, Reaction);
  finally
    L.Free;
  end;
end;

function UpdatePlayerStats(Stats: TPlayerStatUpdate; Reaction: Boolean = False): TMapleStream;
begin
  Result := UpdatePlayerStats(Stats, Reaction, nil);
end;

function UpdatePlayerStats(Stats: TPlayerStatUpdate; Reaction: Boolean; ExtendedSP: TBytes): TMapleStream;
var
  UpdateMask: Integer;
  MyStats: TPlayerStatUpdate;
  Stat: TPair<TMapleStat, Integer>;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['UpdateStats']);

    WriteBool(Reaction);

    {$IFDEF EMS}
    WriteByte(0);
    {$ENDIF}

    UpdateMask := 0;

    MyStats := TPlayerStatUpdate.Create(TComparer<TPair<TMapleStat, Integer>>.Construct(
       function(const Left, Right: TPair<TMapleStat, Integer>): Integer
       begin
         if Integer(Left.Key) < Integer(Right.Key) then
           Result := -1
         else
         if Integer(Left.Key) > Integer(Right.Key) then
           Result := 1
         else
           Result := 0;
       end ));

    for Stat in Stats do
    begin
      UpdateMask := UpdateMask or Integer(Stat.Key);
      MyStats.Add(Stat);
    end;

    if MyStats.Count > 1 then
      MyStats.Sort;

    WriteInt(UpdateMask);
    {$IFDEF CHAOS}
    WriteInt(0);
    {$ENDIF}

    for Stat in MyStats do
    begin
      if Integer(Stat.Key) <= 0 then
        Continue;

      if Stat.Key = msSkin then
        WriteShort(Stat.Value)
      else
      if Stat.Key <= msHair then
        WriteInt(Stat.Value)
      else
      if Stat.Key < msJob then
        WriteByte(Stat.Value)
      else
      {$IFDEF BIGBANG}
      if (Stat.Key >= msHP) and (Stat.Key <= msMaxMP) then
        WriteInt(Stat.Value)
      else
      {$ENDIF}
      if (Stat.Key = msRemainingSP) and (ExtendedSP <> nil) then
        AddSPArray(Result, ExtendedSP)
      else
      if Stat.Key < msEXP then
        WriteShort(Stat.Value)
      else
        WriteInt(Stat.Value);
    end;

    MyStats.Free;

    {$IFDEF VERSION88_UP}
    WriteByte(0);
    {$ENDIF}
    {$IFDEF BIGBANG}
    WriteByte(0);
    {$ENDIF}
  end;
end;

function ShowInventoryStatus(Mode: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowStatusInfo']);
    WriteByte(siInventory);

    WriteByte(Mode);
    WriteInt64(0);
  end;
end;

function InventoryFull: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ModifyInventoryItem']);

    WriteShort(1);
  end;
end;

function ShowInventoryFull: TMapleStream;
begin
  Result := ShowInventoryStatus($FF);
end;

function MoveInventoryItem(InvType: TMapleInventoryType; Src, Dst: ShortInt): TMapleStream;
begin
  Result := MoveInventoryItem(InvType, Src, Dst, -1);
end;

function MoveInventoryItem(InvType: TMapleInventoryType; Src, Dst, Equip: ShortInt): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ModifyInventoryItem']);

    WriteByte(1);
    WriteByte(1);
    WriteByte(2);
    WriteByte(InvType);
    WriteShort(Src);
    WriteShort(Dst);
    if Equip > -1 then
      WriteByte(Equip);
  end;
end;

function MoveAndMergeInventoryItem(InvType: TMapleInventoryType; Src, Dst: ShortInt; Total: SmallInt): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ModifyInventoryItem']);

    WriteByte(1);
    WriteByte(2);
    WriteByte(3);
    WriteByte(InvType);
    WriteShort(Src);

    WriteByte(1);

    WriteByte(InvType);
    WriteShort(Dst);
    WriteShort(Total);
  end;
end;

function MoveAndMergeWithRestInventoryItem(InvType: TMapleInventoryType; Src, Dst: ShortInt; SrcQ, DstQ: SmallInt): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ModifyInventoryItem']);

    WriteByte(1);
    WriteByte(2);
    WriteByte(1);
    WriteByte(InvType);
    WriteShort(Src);
    WriteShort(SrcQ);

    WriteByte(1);

    WriteByte(InvType);
    WriteShort(Dst);
    WriteShort(DstQ);
  end;
end;

function UpdateCharLook(Char: TMapleCharacter): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['UpdateCharLook']);

    WriteInt(Char.ID);
    WriteByte(1);
    AddCharLook(Result, Char, False);

    WriteInt(0);   // xxx Rings

    WriteShort(0);
    {$IFDEF VERSION88_UP}
    WriteShort(0);
    WriteByte(0);
    {$ENDIF}
  end;
end;

procedure StartQuest(Client: TMapleClient; Quest: Word; NPC: Integer);
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(OpHandler.SendOps['ShowStatusInfo']);
    WriteByte(siQuest);

    WriteShort(Quest);
    WriteByte(1);
    WriteShort(0);
  end;
  Client.Write(Packet);

  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(OpHandler.SendOps['UpdateQuest']);
    WriteByte({$IFNDEF BIGBANG} 8 {$ELSE} 10 {$ENDIF});

    WriteShort(Quest);
    WriteInt(NPC);
    WriteInt(0);
  end;
  Client.Write(Packet);
end;

function UpdateQuest(Quest: Word; Data: string): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowStatusInfo']);
    WriteByte(siQuest);

    WriteShort(Quest);
    WriteByte(1);
    WriteMapleAnsiString(Data);
  end;
end;

procedure CompleteQuest(Client: TMapleClient; QuestID, NextQuest: Word; NPC: Integer;
  Silent: Boolean = False);
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(OpHandler.SendOps['ShowStatusInfo']);
    WriteByte(siQuest);

    WriteShort(QuestID);
    WriteByte(2);
    WriteInt64(GetServerTime);
  end;
  Client.Write(Packet);

  if Silent then
    Exit;

  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(OpHandler.SendOps['UpdateQuest']);
    WriteByte({$IFNDEF BIGBANG} 8 {$ELSE} 10 {$ENDIF});

    WriteShort(QuestID);
    WriteInt(NPC);
    WriteShort(NextQuest);
  end;
  Client.Write(Packet);

  Client.Write(ShowSpecialEffect(seQuestComplete));

  Packet := ShowSpecialEffect(Client.Player.ID, seQuestComplete);
  Client.Player.Map.BroadcastMessage(Client.Player, Packet, False);
end;

function ForfeitQuest(Quest: Word): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowStatusInfo']);
    WriteByte(siQuest);

    WriteShort(Quest);
    WriteShort(0);
  end;
end;

function InstructionBubble(Msg: string; Width, Height: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['InstructionBubble']);

    if Width = -1 then
    begin
      Width := Length(Msg) * 5;
      if Width < 40 then
        Width := 40;    // Anything lower crashes client / doesn't look good
    end;

    WriteMapleAnsiString(Msg);
    WriteShort(Width);
    WriteShort(Height);
    WriteByte(1);
  end;
end;

function PortalBlocked: TMapleStream;
begin
  Result := EnableActions(True);
end;

function StatUpdateOK: TMapleStream;
begin
  Result := EnableActions(True);
end;

function UpdateInventorySlot(InvType: TMapleInventoryType; Item: TItem; Reaction: Boolean = False): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ModifyInventoryItem']);

    WriteBool(Reaction);
    WriteByte(1);   // update
    WriteByte(1);   // mode
    WriteByte(InvType);
    WriteByte(Item.Position);
    WriteByte(0);  // ?
    WriteShort(Item.Quantity);
  end;
end;

function AddInventorySlot(InvType: TMapleInventoryType; Item: TItem; Reaction: Boolean = False): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ModifyInventoryItem']);

    WriteBool(Reaction);
    WriteByte(1);   // update
    WriteByte(0);   // add
    WriteByte(InvType);
    WriteShort(Item.Position);
    AddItemInfo(Result, Item, False);
  end;
end;

function ClearInventorySlot(InvType: TMapleInventoryType; Slot: Integer; Reaction: Boolean = False): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ModifyInventoryItem']);

    WriteBool(Reaction);
    WriteByte(1);   // update
    WriteByte(3);   // clear
    WriteByte(InvType);
    WriteShort(Slot);
    if Slot < 0 then
      WriteByte(1);
  end;
end;

function DropInventoryItemUpdate(InvType: TMapleInventoryType; Item: TItem): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ModifyInventoryItem']);

    WriteByte(1);
    WriteByte(1);
    WriteByte(1);
    WriteByte(InvType);
    WriteShort(Item.Position);
    WriteShort(Item.Quantity);
  end;
end;

function DropInventoryItem(InvType: TMapleInventoryType; Src: ShortInt): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ModifyInventoryItem']);

    WriteByte(1);
    WriteByte(1);
    WriteByte(3);
    WriteByte(InvType);
    WriteShort(Src);
    if Src < 0 then
      WriteByte(1);
  end;
end;

function UpdateInventorySlotLimit(Inventory: TMapleInventory): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['InventorySlotUpdate']);

    WriteByte(Inventory.InvType);
    WriteByte(Inventory.SlotLimit);
  end;
end;

function ShowItemGain(ID, Quantity: Integer; InChat: Boolean): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    if InChat then
    begin
      WriteShort(OpHandler.SendOps['ShowItemGainInChat']);
      WriteByte(seItemGain);
      WriteByte(1);
    end
    else
    begin
      WriteShort(OpHandler.SendOps['ShowStatusInfo']);
      WriteShort(Byte(siInventory));
    end;

    WriteInt(ID);
    WriteInt(Quantity);

    if not InChat then
      WriteInt64(0);
  end;
end;

function ShowEXPGain(Gain: Integer; InChat, White: Boolean; Party: Byte = 1): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowStatusInfo']);
    WriteByte(siExp);

    WriteBool(White);
    WriteInt(Gain);
    WriteBool(InChat);
    WriteInt(0);   // Monster book bonus
    WriteByte(0);
    WriteByte(0);//Party * 10);
    WriteShort(0);
    WriteInt(0);
    WriteInt64(0);
    if InChat then
      WriteInt(0)
    else
    begin
      WriteShort(0);
      WriteByte(0);
    end;
    WriteInt(0);
    {$IFDEF VERSION88_UP}
    WriteInt64(0);
    {$ENDIF}
  end;
end;

function ShowFameGain(Gain: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowStatusInfo']);
    WriteByte(siFame);

    WriteInt(Gain);
  end;
end;

function ShowMesoGain(Gain: Integer; InChat: Boolean): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowStatusInfo']);

    if not InChat then
    begin
      WriteByte(0);     // Opcode mode
      WriteBool(True);  // IsMesos
      WriteByte(0);
    end
    else
      WriteByte(siMesos);

    WriteInt(Gain);
    WriteShort(0)   // Internet Cafe Bonus
  end;
end;

{$IFDEF VERSION89_UP}
function ShowSPGain(Gain: Byte; Job: TMapleJob): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowStatusInfo']);
    WriteByte(siSP);

    if Word(Job) div 100 = 22 then
      WriteShort(Word(Job))
    else
      WriteShort(0);
    WriteByte(Gain);
  end;
end;
{$ENDIF}

function ShowSpecialEffect(Effect: TSpecialEffect): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowItemGainInChat']);

    WriteByte(Effect);
  end;
end;

function ShowSpecialEffect(CharID: Integer; Effect: TSpecialEffect): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowSkill']);

    WriteInt(CharID);
    WriteByte(Effect);
  end;
end;

function ShowOrangeText(Msg: string): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowStatusInfo']);
    WriteByte(siText);

    WriteMapleAnsiString(Msg);
  end;
end;

function ShowInfoOnScreen(Msg: string): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowInfo']);

    WriteMapleAnsiString(Msg);
  end;
end;

function ShowWZEffect(Path: string; AdditionalInfo: Integer = -1): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowItemGainInChat']);

    // Opcode mode
    if AdditionalInfo = -1 then
      WriteByte(seWZEffect)     // only name
    else
      WriteByte(seWZEffect2);   // with additional int

    WriteMapleAnsiString(Path);

    if AdditionalInfo <> -1 then
      WriteInt(AdditionalInfo);   // sometimes 1
  end;
end;

function TrembleEffect(Mode, Delay: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['MapEffect']);

    WriteByte(1);  // Opcode mode - Tremble
    WriteByte(Mode);
    WriteInt(Delay);
  end;
end;

function ShowMapEffect(Name: string): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['MapEffect']);

    WriteByte(3);  // Opcode mode - Visual
    WriteMapleAnsiString(Name);
  end;
end;

function PlayWZSound(Name: string): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['MapEffect']);

    WriteByte(4);  // Opcode mode - Sound
    WriteMapleAnsiString(Name);
  end;
end;

function UpdateAreaInfo(InfoID: Integer; NewList: string): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowStatusInfo']);
    WriteByte(siAreaInfo);

    { This is an internal quest id only the client itself knows,
      it is not in the WZ-data. 21002 is the id for the first part of the Aran Intro -
      21000 and 21001 are existing quests though, so that would
      be a logical explanation. }
    WriteShort(InfoID);   // e.g. 21002 [$0A $52] or 21019 [$1B $52]
    // probably which instructions and hints etc were already shown
    WriteMapleAnsiString(NewList);   // a list like: arr0=o;mo1=o;mo2=o
  end;
end;

function ShowAranComboCounter(Count: Cardinal): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['AranComboCounter']);

    WriteInt(Count);
  end;
end;

function ShowGuideEffectWithText(Text: string; Width, Duration: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowGuideEffect']);

    WriteByte(0);     // Mode -- 0 = Text, 1 = Only ID
    WriteMapleAnsiString(Text);
    WriteInt(Width);
    WriteInt(Duration);
  end;
end;

function ShowGuideEffect(ID, Duration: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowGuideEffect']);

    WriteByte(1);    // Mode -- 0 = Text, 1 = Only ID
    WriteInt(ID);
    WriteInt(Duration);
  end;
end;

procedure AddMonsterData(var Packet: TMapleStream; Life: TMapleMonster; NewSpawn: Boolean; SummonEffect: Integer = 0);
begin
  with Packet do
  begin
    WriteInt(Life.ObjectID);
    WriteByte(5);  // Has to do with controlling?
    WriteInt(Life.ID);

    {$IFDEF ASCENSION}
    WriteByte(0);
    {$ENDIF}
    Life.StatusPacket(Packet);

    WritePos(Life.Position);
    WriteByte(2 or Ord(Life.FacesRight));
    WriteShort(0);     // OriginFh
    WriteShort(Life.Foothold);

    if SummonEffect > 0 then
      WriteInt(SummonEffect);

    if NewSpawn then
      WriteShort(-2)
    else
      WriteShort(-1);

    WriteInt(0);
    {$IFDEF VERSION89_UP}
    WriteInt(0);
    {$ENDIF}
    {$IFDEF VERSION97_UP}
    WriteByte(-1);
    {$ENDIF}
  end;
end;

function SpawnMonster(Life: TMapleMonster; NewSpawn: Boolean; SummonEffect: Integer = 0): TMapleStream;
begin
  Result := TMapleStream.Create;
  Result.WriteShort(OpHandler.SendOps['SpawnMonster']);

  AddMonsterData(Result, Life, NewSpawn, SummonEffect);
end;

function ControlMonster(Life: TMapleMonster; NewSpawn: Boolean; Aggro: Boolean = False): TMapleStream;
const                   // Normal (False), Aggressive (True)
  AggrStatus: array[Boolean] of Byte = (1, 2);
begin
  Result := TMapleStream.Create;
  Result.WriteShort(OpHandler.SendOps['ControlMonster']);
  Result.WriteByte(AggrStatus[Aggro]);

  AddMonsterData(Result, Life, NewSpawn);
end;

function MoveMonsterResponse(ObjectID, MoveID, CurrentMP: Integer; UseSkills: Boolean; SkillID: Integer = 0; SkillLevel: Integer = 0): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['MoveMonsterResponse']);

    WriteInt(ObjectID);
    WriteShort(MoveID);
    WriteBool(UseSkills);
    WriteShort(CurrentMP);
    WriteByte(SkillID);
    WriteByte(SkillLevel);
    {$IFDEF AFTERSHOCK}
    WriteInt(0);
    {$ENDIF}
  end;
end;

function MoveMonster(UseSkills: Boolean; Action, SkillID, SkillLv, Delay, OID: Integer; StartPos: TPoint; Moves: TList<TLifeMovement>): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['MoveMonster']);

    WriteInt(OID);
    WriteByte(0);
    {$IFDEF VERSION88_UP}
    WriteByte(0);
    {$ENDIF}
    WriteBool(UseSkills);
    WriteByte(Action);
    WriteByte(SkillID);
    WriteByte(SkillLv);
    WriteShort(Delay);
    {$IFDEF VERSION88_UP}
    WriteInt64(0);
    {$ENDIF}
    WritePos(StartPos);
    {$IFDEF VERSION88_UP}
    WriteInt(0);
    {$ENDIF}
    SerializeMovements(Result, Moves);
  end;
end;

function ShowMonsterHP(OID, RemHPPercentage: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowMonsterHP']);

    WriteInt(OID);
    WriteByte(RemHPPercentage);
  end;
end;

function KillMonster(OID: Integer; Animation: Boolean): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['KillMonster']);

    WriteInt(OID);
    WriteBool(Animation);
    {$IFDEF VERSION88_UP}
    WriteByte(1);
    {$ENDIF}
  end;
end;

function StopControllingMonster(OID: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ControlMonster']);

    WriteByte(0);
    WriteInt(OID);
  end;
end;

function ApplyMobStatus(OID, StatusMask: Integer; Info: TList<TMobStatus>; Delay: Word): TMapleStream;
var
  i: TMobStatus;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['AddMobStatus']);

    WriteInt(OID);

    {$IF DEFINED(CHAOS)}  // UINT256
    WriteInt64(0);
    WriteInt64(0);
    {$ELSEIF DEFINED(AFTERSHOCK)}  // UINT160
    WriteInt(0);
    {$IFEND}  // UINT128
    WriteInt64(0);
    WriteInt64(Int64(StatusMask) shl 32);  // Equals WriteInt(0); WriteInt(Status);

    Info.Sort(TComparer<TMobStatus>.Construct(
      function (const Left, Right: TMobStatus): Integer
      begin
        if Left.Status < Right.Status then
          Result := -1
        else if Left.Status > Right.Status then
          Result := 1
        else
          Result := 0;
      end ));

    for i in Info do
    begin
      WriteShort(i.Val);
      if i.SkillID >= 0 then
        WriteInt(i.SkillID)
      else
      begin
        // xxx Mobskills
        // WriteShort(i.MobSkill);
        // WriteShort(i.Level);
      end;
      WriteShort(-1);
    end;

    WriteShort(Delay);
    WriteByte(Info.Count);
    WriteByte(1);
  end;
end;

function RemoveMobStatus(OID, Status: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['RemoveMobStatus']);

    WriteInt(OID);
    {$IFDEF BIGBANG}
    WriteInt64(0);
    WriteInt(0);
    {$ENDIF}
    WriteInt(Status);
    WriteShort(1);
  end;
end;

function ShowMagnet(OID: Integer; Success: Boolean): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowMagnet']);

    WriteInt(OID);
    WriteBool(Success);
    WriteByte(1);
  end;
end;

function DamageMonster(OID, Damage: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['DamageMonster']);

    WriteInt(OID);
    WriteByte(0);
    WriteInt(Damage);
    WriteByte(0);
    WriteByte(0);
    WriteByte(0);
  end;
end;

function ShowQuestComplete(Quest: Word): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowQuestComplete']);

    WriteShort(Quest);
  end;
end;

function ShowDrop(Drop: TDrop; Status: TDropStatus; Origin: TPoint): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['DropItem']);

    WriteByte(Status);  // 3 = disappear during drop animation; 2 = show existing; 1 then 0 = show new
    WriteInt(Drop.ObjectID);
    WriteBool(Drop.IsMesos);
    WriteInt(Drop.Data);
    WriteInt(Drop.Owner);
    WriteByte(Drop.DType);  // 0 = timeout for non-owner; 1 = timeout for non-owner's party; 2 = FFA; 3 = explosive/FFA
    WritePos(Drop.Position);
    WriteInt(Drop.DropperOID);
    if Status in [dsShowNewSecond, dsShowNewFirst, dsUntradeable] then
      WritePos(Origin);

    WriteByte(0);
    if Status <> dsShowExisting then
    begin
      WriteByte(1);
      WriteBool(Drop.IsMesos);   // Pet Meso pickup
    end;

    if not Drop.IsMesos then
    begin
      AddExpirationTime(Result, 0, False);

      WriteBool(not Drop.PlayerDrop);   // Pet item pickup
    end;
    {$IFDEF VERSION88_UP}
    WriteByte(0);
    {$ENDIF}
  end;
end;

function RemoveDrop(Drop: TDrop): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['TakeDrop']);

    WriteByte(0);
    WriteInt(Drop.ObjectID);
  end;
end;

function DontTake: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ModifyInventoryItem']);

    WriteShort(1);
  end;
end;

function DropNotAvailableForPickup: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowStatusInfo']);
    WriteByte(siInventory);

    WriteByte(-2);
  end;
end;

function CantGetAnymoreItems: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowStatusInfo']);
    WriteByte(siInventory);

    WriteByte(-1);
  end;
end;

function TakeDrop(DropID, PlayerID: Integer; PetIndex: ShortInt = -1): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['TakeDrop']);

    if PetIndex <> -1 then
      WriteByte(5)
    else
      WriteByte(2);

    WriteInt(DropID);
    WriteInt(PlayerID);

    if PetIndex <> -1 then
      WriteByte(PetIndex);
  end;
end;

function UpdateSkill(SkillID, NewLevel, MasterLevel: Integer; Reaction: Boolean = True): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['UpdateSkill']);

    WriteBool(Reaction);
    WriteShort(1);
    WriteInt(SkillID);
    WriteInt(NewLevel);
    WriteInt(MasterLevel);
    AddExpirationTime(Result, 0);
    WriteByte(1);
  end;
end;

function ShowSkill(Player: TMapleCharacter; SkillID: Integer; Level, EffectID: Byte): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowSkill']);

    WriteInt(Player.ID);
    WriteByte(EffectID);
    WriteInt(SkillID);
    WriteByte(Player.Level);
    WriteByte(Level);
  end;
end;

procedure UseBuff(Client: TMapleClient; SkillID, Time: Integer; PSkill: PActiveBuff; MSkill: PActiveMapBuff; AddedInfo: SmallInt);
var
  i: Integer;
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(OpHandler.SendOps['UseSkill']);

    PSkill.Stat.Serialize(Packet);

    for i := 0 to PSkill^.Vals.Count - 1 do
    begin
      WriteShort(PSkill^.Vals[i]);
      WriteInt(SkillID);
      WriteInt(Time * 1000);
    end;
    WriteShort(0);
    WriteShort(AddedInfo);
    WriteByte(0);   // Number of times you've been buffed total - only certain skills have this part
  end;
  Client.Write(Packet);

  if not PSkill^.HasMapBuff then    // xxx or IsUsingHide
    Exit;

  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(OpHandler.SendOps['BroadcastSkill']);

    WriteInt(Client.Player.ID);

    MSkill.Stat.Serialize(Packet);

  (*  if MSkill^.TypeList[Byte3] and $40 > 0 then
      WriteInt(SkillID)
    else
      for i := 0 to MSkill^.Values.Count - 1 do
        if MSkill^.Bytes[i] = Byte5 then
          WriteShort(MSkill^.Values[i])
        else
          WriteByte(MSkill^.Values[i]); *)

    WriteShort(0);
    WriteShort(AddedInfo);
  end;
  Client.Player.Map.BroadcastMessage(Client.Player, Packet);
end;

procedure UseMount(Client: TMapleClient; SkillID: Integer; PSkill: PActiveBuff; MSkill: PActiveMapBuff; MountID: Integer);
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(OpHandler.SendOps['UseSkill']);

    PSkill.Stat.Serialize(Packet);
    WriteShort(0);
    WriteInt(MountID);
    WriteInt(SkillID);
    WriteInt(0);
    WriteShort(0);
    WriteByte(0);
    WriteByte(0);   // Number of times you've been buffed total
  end;
  Client.Write(Packet);

  // xxx IsUsingHide

  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(OpHandler.SendOps['BroadcastSkill']);

    WriteInt(Client.Player.ID);

    PSkill.Stat.Serialize(Packet);
    WriteShort(0);
    WriteInt(MountID);
    WriteInt(SkillID);
    WriteInt(0);
    WriteShort(0);
    WriteByte(0);
    WriteByte(0);   // Number of times you've been buffed total
  end;
  Client.Player.Map.BroadcastMessage(Client.Player, Packet);
end;

procedure UsePirateBuff(Client: TMapleClient; SkillID, Time: Integer; PSkill: PActiveBuff; MSkill: PActiveMapBuff);
var
  i: Integer;
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(OpHandler.SendOps['UseSkill']);

    PSkill.Stat.Serialize(Packet);

    WriteShort(0);
    for i := 0 to PSkill^.Vals.Count - 1 do
    begin
      WriteShort(PSkill^.Vals[i]);
      WriteShort(0);
      WriteInt(SkillID);
      WriteInt(0);
      WriteByte(0);
      WriteShort(Time);
    end;
    WriteShort(0);
    WriteByte(0);
  end;
  Client.Write(Packet);

  // xxx exit if hiding

  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(OpHandler.SendOps['BroadcastSkill']);

    WriteInt(Client.Player.ID);

    MSkill.Stat.Serialize(Packet);

    WriteShort(0);
    for i := 0 to PSkill^.Vals.Count - 1 do
    begin
      WriteShort(PSkill^.Vals[i]);
      WriteShort(0);
      WriteInt(SkillID);
      WriteInt(0);
      WriteByte(0);
      WriteShort(Time);
    end;
    WriteShort(0);
  end;
  Client.Player.Map.BroadcastMessage(Client.Player, Packet);
end;

procedure EndBuff(Client: TMapleClient; ToEnd: TBuffStat);
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(OpHandler.SendOps['CancelSkill']);

    ToEnd.Serialize(Packet);
    WriteByte(0);
  end;
  Client.Write(Packet);

  // xxx exit when hiding
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(OpHandler.SendOps['BroadcastBuffEnd']);

    WriteInt(Client.Player.ID);
    ToEnd.Serialize(Packet);
  end;
  Client.Player.Map.BroadcastMessage(Client.Player, Packet);
end;

function ShowOwnBuffEffect(Player: TMapleCharacter; SkillID, SkillLv, EffectID: Integer; AdditionalInfo: Byte = 0): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowItemGainInChat']);

    WriteByte(EffectID);
    WriteInt(SkillID);
    WriteByte(Player.Level);
    WriteByte(SkillLv);
    WriteByte(AdditionalInfo);
  end;
end;

function ShowHPHealBySkill(Delta: Word): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowItemGainInChat']);  // xxx rename opcode, it is used for more than that

    WriteByte($A);
    WriteShort(Delta);
  end;
end;

function KeymapPacket(Map: TDictionary<Integer, PKeyBinding>): TMapleStream;
var
  i: Integer;
  Key: PKeyBinding;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['Keymap']);

    WriteByte(0);
    for i := 0 to 90 do
      if Map.TryGetValue(i, Key) then
      begin
        WriteByte(Key^.KeyType);
        WriteInt(Key^.Action);
      end
      else
      begin
        WriteByte(0);
        WriteInt(0);
      end;
  end;
end;

function QuickSlotPacket(Slot: TQuickSlot): TMapleStream;
var
  i: Integer;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['QuickSlot']);

    WriteByte(1);
    for i := 0 to High(Slot) do
      WriteInt(Slot[i]);
  end;
end;

function SpawnReactor(Reactor: TMapleReactor): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['SpawnReactor']);

    WriteInt(Reactor.ObjectID);
    WriteInt(Reactor.ID);
    WriteByte(Reactor.State);
    WritePos(Reactor.Position);
    WriteByte(0);
    WriteShort(0);
  end;
end;

function TriggerReactor(Reactor: TMapleReactor): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['TriggerReactor']);

    WriteInt(Reactor.ObjectID);
    WriteByte(Reactor.State);
    WritePos(Reactor.Position);
    WriteInt(0);
  end;
end;

function DestroyReactor(Reactor: TMapleReactor): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['DestroyReactor']);

    WriteInt(Reactor.ObjectID);
    WriteByte(Reactor.State);
    WritePos(Reactor.Position);
  end;
end;

function ShowChair(CharID, ItemID: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ShowChair']);

    WriteInt(CharID);
    WriteInt(ItemID);
  end;
end;

function ChairAction(ID: SmallInt = -1): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ChairAction']);

    WriteBool(ID > -1);
    if ID > -1 then
      WriteShort(ID);
  end;
end;

function ShowShop(ID: Integer; Shop: PShopInfo): TMapleStream;
var
  Item: PShopItemInfo;
  IDsDone: TList<Integer>;
  ShopCount: Word;
  Cost: Double;
  Rechargeables: TDictionary<Integer, Double>;
  IID: Integer;
begin
  IDsDone := TList<Integer>.Create;
  if Shop^.RechargeTier > 0 then
    Rechargeables := ShopDataProv.RechargeCosts[Shop^.RechargeTier]
  else
    Rechargeables := TDictionary<Integer, Double>.Create;
  ShopCount := Shop^.Items.Count + Rechargeables.Count;

  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['NPCShop']);

    WriteInt(Shop^.NPC);
    WriteShort(0);    // will be set later

    for Item in Shop^.Items do
    begin
      WriteInt(Item^.ItemID);
      WriteInt(Item^.Price);

      // I have no idea what this is at all, in Henesys Potion Shop they are ALWAYS 0
      WriteInt64(0);
      WriteInt(0);
      {$IFDEF VERSION88_UP}
      WriteInt(0);
      WriteByte(0);
      {$ENDIF}

      if IsRechargeable(Item^.ItemID) then
      begin
        IDsDone.Add(Item^.ItemID);
        Cost := 0.0;

        if Shop^.RechargeTier > 0 then
        begin
          Dec(ShopCount);
          if Rechargeables.ContainsKey(Item^.ItemID) then
            Cost := Rechargeables[Item^.ItemID];
        end;

        Write(Cost, 8);  // won't implement WriteDouble() just for doing this
      end
      else
        WriteShort(Item^.Quantity);

      // xxx add rechargeable bonus for rechargeables
      WriteShort(ItemDataProv.GetSlotMax(Item^.ItemID));
    end;

    for IID in Rechargeables.Keys do
      if not IDsDone.Contains(IID) then
      begin
        WriteInt(IID);
        WriteInt(0);
        WriteInt64(0);
        WriteInt(0);
        {$IFDEF VERSION88_UP}
        WriteInt(0);
        WriteByte(0);
        {$ENDIF}
        Cost := Rechargeables[IID];
        Write(Cost, 8);
        WriteShort(ItemDataProv.GetSlotMax(IID));    // xxx rechargeable bonus
      end;

    Seek(6, soBeginning);
    WriteShort(ShopCount);
  end;

  IDsDone.Free;
  if Shop^.RechargeTier = 0 then
    Rechargeables.Free;
end;

function ItemBought(Msg: Byte): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ItemBought']);

    // 0 = Success; 3 = Inventory full
    WriteByte(Msg);
  end;
end;

function GiveFameResponse(Mode: Byte; CharName: string; NewFame: SmallInt): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['FameResponse']);

    WriteByte(0);     // Opcode mode
    WriteMapleAnsiString(CharName);
    WriteByte(Mode);
    WriteShort(NewFame);
    WriteShort(0);
  end;
end;

function GiveFameErrorResponse(Status: Byte): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['FameResponse']);

    WriteByte(Status);
  end;
end;

function ReceiveFame(Mode: Byte; CharNameFrom: string): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['FameResponse']);

    WriteByte(5);   // Opcode mode
    WriteMapleAnsiString(CharNameFrom);
    WriteByte(Mode);
  end;
end;

function AranTutorialMaxStats: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['TemporaryUpdateStats']);

    WriteInt(3871);   // Some byte mask I guess, don't know how to decode it..
    WriteShort(999);  // STR
    WriteShort(999);  // DEX
    WriteShort(999);  // INT
    WriteShort(999);  // LUK
    WriteShort(255);
    WriteShort(999);
    WriteShort(999);
    WriteByte(120);  // Jump
    WriteByte(140);  // Speed
  end;
end;

function RemoveTemporaryStats: TMapleStream;
begin
  Result := TMapleStream.Create;
  Result.WriteShort(OpHandler.SendOps['RemoveTemporaryStats']);
end;

function SpawnTutorialSummon(Spawn: Boolean): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['SpawnTutorialSummon']);

    WriteBool(Spawn);
  end;
end;

function LockUI(Disabled: Boolean): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['LockUI']);

    WriteBool(Disabled);
    {$IFDEF VERSION88_UP}
    WriteInt(0);
    {$ENDIF}
  end;
end;

function DisableUI(Disabled: Boolean): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['DisableUI']);

    WriteBool(Disabled);
  end;
end;

function Whisper(Sender, Text: string; Channel: Byte): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['Whisper']);

    WriteByte($12);
    WriteMapleAnsiString(Sender);
    WriteShort(Channel);
    WriteMapleAnsiString(Text);
  end;
end;

function WhisperResponse(Target: string; Success: Boolean): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['Whisper']);

    WriteByte($A);
    WriteMapleAnsiString(Target);
    WriteBool(Success);
  end;
end;

function FindReplyWithMap(Target: string; MapID: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['Whisper']);

    WriteByte(9);
    WriteMapleAnsiString(Target);
    WriteByte(1);
    WriteInt(MapID);
    WriteInt64(0);
  end;
end;

function FindReply(Target: string; Channel: Byte): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['Whisper']);

    WriteByte(9);
    WriteMapleAnsiString(Target);
    WriteByte(3);
    WriteInt(Channel);
  end;
end;

function GroupChat(From, Msg: string; Mode: Byte): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['GroupChat']);

    WriteByte(Mode);
    WriteMapleAnsiString(From);
    WriteMapleAnsiString(Msg);
  end;
end;

function PartyCreated(ID: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PartyOperation']);

    WriteByte(8);
    WriteInt(ID);
    WriteInt(NO_MAP);
    WriteInt(NO_MAP);
    WriteInt(0);
    {$IFDEF VERSION88_UP}
    WriteShort(31);
    WriteShort(14);
    {$ENDIF}
  end;
end;

procedure AddPartyStatus(Channel: Integer; Party: TMapleParty; var Packet: TMapleStream);
var
  Members: TList<TMaplePartyCharacter>;
  MPC: TMaplePartyCharacter;
  i: Integer;
begin
  Members := TList<TMaplePartyCharacter>.Create(Party.Members);
  while Members.Count < 6 do
    Members.Add(TMaplePartyCharacter.Create);

  for MPC in Members do
    Packet.WriteInt(MPC.ID);

  for MPC in Members do
  begin
    Packet.WriteAnsiString(MPC.Name);
    // fill to maximum name length
    for i := Length(MPC.Name) to 12 do
      Packet.WriteByte(0);
  end;

  for MPC in Members do
    Packet.WriteInt(Integer(MPC.Job));

  for MPC in Members do
    Packet.WriteInt(MPC.Level);

  for MPC in Members do
    Packet.WriteInt(MPC.Channel);

  Packet.WriteInt(Party.Leader.ID);
  for MPC in Members do
    if MPC.Channel = Channel then
      Packet.WriteInt(MPC.MapID)
    else
      Packet.WriteInt(0);

  for MPC in Members do
  begin
    if MPC.Channel = Channel then
    begin
      Packet.WriteInt(MPC.DoorTown);
      Packet.WriteInt(MPC.DoorTarget);
      Packet.WriteInt(MPC.DoorPosition.X);
      Packet.WriteInt(MPC.DoorPosition.Y);
    end
    else
    begin
      Packet.WriteInt64(0);
      Packet.WriteInt64(0);
    end;
    {$IFNDEF VERSION83}
    Packet.WriteInt(0);
    {$ENDIF}
  end;

  {$IFDEF BIGBANG}
  for MPC in Members do
    if MPC.ID > 0 then
      Packet.WriteInt(255)
    else
      Packet.WriteInt(0);

  for i := 1 to 4 do
    Packet.WriteInt64(0);
  {$ENDIF}

  // Free members that were created to fill it up
  for MPC in Members do
    if not Party.Members.Contains(MPC) then
      MPC.Free;
  Members.Free;
end;

function UpdateParty(Channel: Integer; Party: TMapleParty; Op: TPartyOperation; Target: TMaplePartyCharacter): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PartyOperation']);

    case Op of
      poDisband, poExpel, poLeave:
      begin
        WriteByte($C);
        WriteInt(Party.ID);
        WriteInt(Target.ID);
        WriteBool(Op <> poDisband);
        if Op = poDisband then
          WriteInt(Target.ID)
        else
        begin
          WriteBool(Op = poExpel);
          WriteMapleAnsiString(Target.Name);
          AddPartyStatus(Channel, Party, Result);
        end;
      end;

      poJoin:
      begin
        WriteByte($F);
        WriteInt(Party.ID);
        WriteMapleAnsiString(Target.Name);
        AddPartyStatus(Channel, Party, Result);
      end;

      poUpdate:
      begin
        WriteByte($7);
        WriteInt(Party.ID);
        AddPartyStatus(Channel, Party, Result);
      end;

      poChangeLeader:
      begin
        WriteByte($1F);
        WriteInt(Target.ID);
        WriteByte(0);  // 1: Shows message that leader disconnected and it therefore changed
      end;
    end;
  end;
end;

function PartyStatusMessage(Msg: TPartyStatusMessage; CharName: string = ''): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PartyOperation']);

    WriteByte(Msg);
    if CharName <> '' then
      WriteMapleAnsiString(CharName);
  end;
end;

function PartyStringMessage(Msg: string): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PartyMessage']);
    WriteShort(11);

    WriteMapleAnsiString(Msg);
  end;
end;

function PartyInvite(From: TMapleCharacter): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PartyOperation']);
    WriteByte(4);

    WriteInt(From.Party.ID);
    WriteMapleAnsiString(From.Name);
    {$IFNDEF VERSION83}
    WriteInt(From.Level);
    WriteInt(Int32(From.Job));
    {$ENDIF}
    WriteByte(0);
  end;
end;

function UpdatePartyMemberHP(CID, HP, MaxHP: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['UpdatePartyMemberHP']);

    WriteInt(CID);
    WriteInt(HP);
    WriteInt(MaxHP);
  end;
end;

procedure AddJaguarInfo(var Packet: TMapleStream; NewID: Byte);
begin
  with Packet do
  begin
    WriteByte(NewID);
    WriteInt64(0);
    WriteInt64(0);
    WriteInt(0);
  end;
end;

function UpdateJaguar(NewID: Byte): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['UpdateJaguar']);
    AddJaguarInfo(Result, NewID);
  end;
end;

function ShowClock(Seconds: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['Clock']);
    WriteByte(2);   // 1 = Clock showing actual time; 2 = Countdown

    WriteInt(Seconds);
  end;
end;

end.
