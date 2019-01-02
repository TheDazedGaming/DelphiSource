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

unit ScriptHelper;

interface

uses Classes, SysUtils, RTTI, uSE2UnitManager, uSE2RunAccess, Generics.Collections,
     TypInfo, GameLogic, uSE2IncConsole, EventScript, LifeDataProvider;

const
  SCRIPT_EXT = '.ds';

type
  TClassImporter = class
  public
    class function GenerateSource(const C: array of TClass; UName: string = ''): string;
    class procedure Register(const C: array of TClass; UName: string = '');
  end;

  // Forward everything to a real MapleCharacter object
  TScriptMapleCharacter = class
  private
    FChar: TObject;
  public
    constructor Create(AChar: TObject);
    destructor Destroy; override;

    procedure ChangeMap(ID, Portal: Integer);
    function GetQuestStatus(ID: Word): Byte;
    procedure SendUpdateStats;

    // Pseudo-Properties
    function Gender: Byte;
    function Job: Word;
    function Level: Byte;
    function MapID: Integer;
    function Mesos: Integer;
    function Name: string;

    // Getters/Setters. Need to be public so RTTI can see them
    function GetSTR: Integer;
    function GetDEX: Integer;
    function GetINT: Integer;
    function GetLUK: Integer;
    function GetAP: Integer;
    function GetSP: Integer;
    function GetHP: Integer;
    function GetMP: Integer;
    function GetMaxHP: Integer;
    function GetMaxMP: Integer;
    function GetEventInstance: TEventInstance;
    procedure SetSTR(Value: Integer);
    procedure SetDEX(Value: Integer);
    procedure SetINT(Value: Integer);
    procedure SetLUK(Value: Integer);
    procedure SetAP(Value: Integer);
    procedure SetSP(Value: Integer);
    procedure SetHP(Value: Integer);
    procedure SetMP(Value: Integer);
    procedure SetMaxHP(Value: Integer);
    procedure SetMaxMP(Value: Integer);
    procedure SetEventInstance(Value: TEventInstance);

    property STR: Integer read GetSTR write SetSTR;
    property DEX: Integer read GetDEX write SetDEX;
    property INT: Integer read GetINT write SetINT;
    property LUK: Integer read GetLUK write SetLUK;
    property RemainingAP: Integer read GetAP write SetAP;
    property RemainingSP: Integer read GetSP write SetSP;
    property HP: Integer read GetHP write SetHP;
    property MP: Integer read GetMP write SetMP;
    property MaxHP: Integer read GetMaxHP write SetMaxHP;
    property MaxMP: Integer read GetMaxMP write SetMaxMP;
    property EventInstance: TEventInstance read GetEventInstance write SetEventInstance;
  end;

  TGeneralScriptManager = class
  protected
    FClient: TObject;
    FChar: TScriptMapleCharacter;
  public
    constructor Create(AClient: TObject); overload;
    destructor Destroy; override;

    function GetEventManager(Name: string): TEventManager;

    procedure ClearSavedLocation(LType: string);
    function GetSavedLocation(LType: string): Integer;
    procedure SaveLocation(LType: string);

    procedure GainItem(ID, Quantity: Integer);
    function GetItemQuantity(ID: Integer): Integer;
    function HasItem(ID: Integer): Boolean;
    function IsEquipped(ID: Integer): Boolean;
    procedure RemoveAll(ID: Integer);

    function GetQuestData(ID: Word): string;
    function IsQuestActive(ID: Word): Boolean;
    function IsQuestFinished(ID: Word): Boolean;
    function IsQuestNotStarted(ID: Word): Boolean;
    procedure UpdateQuest(ID: Word; CustomData: string; SetReqDone: Boolean);
    procedure SilentCompleteQuest(ID: Word);

    procedure DisableUI(Disabled: Boolean);
    procedure LockUI(Disabled: Boolean);
    procedure ShowGuideEffect(ID, Duration: Integer);
    procedure ShowGuideEffectWithText(Text: string; Width, Duration: Integer);
    procedure ShowInfo(Msg: string);
    procedure ShowInfoOnScreen(Msg: string);
    procedure ShowWZEffect(Path: string; AdditionalInfo: Integer);
    procedure PlayWZSound(Name: string);
    procedure UpdateTutorialSummon(Visible: Boolean);

    function GetAreaInfo(ID: Integer): string;
    procedure UpdateAreaInfo(ID: Integer; NewList: string);

    procedure UpdateSkill(ID, Level, MasterLevel: Integer; Reaction: Boolean);

    // Map stuff
    function CountPlayersOnMap(ID: Integer): Integer;
    procedure ForceRespawn(MapID: Integer);
    procedure KillAllMonsters(MapID: Integer);
    procedure SpawnMonster(MapID, MobID, X, Y, Effect: Integer);
    procedure SpawnNPC(MapID, NPC, X, Y: Integer; FacesRight: Boolean);
    procedure RemoveNPC(MapID, NPC: Integer);
    procedure Warp(Map: Integer); overload;
    procedure Warp(Map: Integer; Portal: Byte); overload;
    procedure Warp(Map: Integer; Portal: string); overload;

    function Version: Word;
    function Char: TScriptMapleCharacter;
    property Client: TObject read FClient;
  end;

function FindScript(ID: Integer; Folder: string; out Script: string): Boolean;

implementation

uses MapleServerHandler, MapleClient, MaplePacketCreator, MapleMap, MapleCharacter,
     QuestDataProvider, SkillDataProvider, MapleQuest, PlayerInventory, NPCConversation,
     Main, MapleItem, MapleInventory, MapleNPC, MapleMapObject, MapleMonster;

function FindScript(ID: Integer; Folder: string; out Script: string): Boolean;
var
  Dir: string;
begin
  Dir := ExtractFilePath(ParamStr(0)) + 'Scripts\' + Folder + '\';
  Script := Dir + IntToStr(ID) + SCRIPT_EXT;
  if FileExists(Script) then
    Exit(True);

  // Look if there's a file with the real script-name
  with MCDB.GetQuery do
  begin
    SQL.Text := 'SELECT script FROM scripts WHERE script_type LIKE :t AND objectid = :id';
    ParamByName('t').AsString := Folder + '%';
    ParamByName('id').Value := ID;
    Open;
    try
      if not EOF then
        Script := Dir + FieldByName('script').AsString + SCRIPT_EXT
      else
        Exit(False);

      Result := FileExists(Script);
    finally
      Close;
      Free;
    end;
  end;
end;

{ TClassImporter }

class function TClassImporter.GenerateSource(const C: array of TClass; UName: string = ''): string;
var
  Con: TRTTIContext;
  Typ: TRTTIType;
  M: TRTTIMethod;
  P: TRTTIProperty;
  T: TStringList;
  i, j: Integer;
  Names: TDictionary<string, Byte>;
  Addresses: TDictionary<Pointer, string>;
  GetP, SetP: Pointer;
  s: string;
begin
  if UName = '' then
    UName := Copy(C[0].ClassName, 2, 255);

  T := TStringList.Create;
  Con := TRTTIContext.Create;
  Names := TDictionary<string, Byte>.Create;
  Addresses := TDictionary<Pointer, string>.Create;
  try
    T.Add('unit ' + UName + ';');
    T.Add('interface');
    if C[0].InheritsFrom(TGeneralScriptManager) then
      T.Add('uses Events, ScriptMapleCharacter;')
    else if C[0] = TScriptMapleCharacter then
      T.Add('uses Events;');
    T.Add('type');
    if UName = 'Events' then
    begin
      T.Add('TEventInstance = class;');
      T.Add('TScriptMapleCharacter = class;');
    end;
    for j := 0 to High(C) do
    begin
      Typ := Con.GetType(C[j]);
      T.Add(Typ.Name + ' = class(TExternalObject)');
      T.Add('public');
      for M in Typ.GetMethods do
      begin
        if M.IsConstructor and (Length(M.GetParameters) = 0) then
          Break;

        if not Names.ContainsKey(M.Name) then
          Names.Add(M.Name, 1)
        else
          Names[M.Name] := Names[M.Name] + 1;
      end;

      for M in Typ.GetMethods do
      begin
        if M.IsConstructor then
        begin
          if Length(M.GetParameters) = 0 then
            Break   // TObject methods reached
          else
            Continue;  // We don't need constructors in scripts
        end
        else if M.IsDestructor or ((M.DispatchKind = dkVTable) and (Addresses.ContainsValue(M.Name))) or (Pos('(basic)', M.ToString) > 0) then
          Continue;

        Addresses.Add(M.CodeAddress, M.Name);

        if (Length(M.GetAttributes) > 0) and (M.GetAttributes[0] is TCast) then
          s := StringReplace(M.ToString, 'TObject', TCast(M.GetAttributes[0]).Cast, [rfReplaceAll])
        else
          s := M.ToString;

        i := T.Add(s + '; external;');
        if Names[M.Name] > 1 then
          T[i] := T[i] + ' overload;';
      end;

      for P in Typ.GetProperties do
      begin
        GetP := TRTTIInstanceProperty(P).PropInfo^.GetProc;
        SetP := TRTTIInstanceProperty(P).PropInfo^.SetProc;
        if Addresses.ContainsKey(GetP) and Addresses.ContainsKey(SetP) then
          T.Add(P.ToString + Format(' read %s write %s;', [Addresses[GetP], Addresses[SetP]]));
      end;
      T.Add('end;');

      Addresses.Clear;
      Names.Clear;
    end;
    // They aren't declared by default :(
    T.Add('procedure Inc(var i: Integer);');
    T.Add('procedure Dec(var i: Integer);');
    T.Add('implementation');
    T.Add('procedure Inc(var i: Integer); begin i := i + 1; end;');
    T.Add('procedure Dec(var i: Integer); begin i := i - 1; end;');
    T.Add('end.');
    Result := T.Text;
  finally
    Addresses.Free;
    Names.Free;
    Con.Free;
    T.Free;
  end;
end;

class procedure TClassImporter.Register(const C: array of TClass; UName: string = '');
var
  p: TSE2MethodUnit;
  AC: array of TClass;
  i: Integer;
begin
  // Strange exceptions if I don't make C constant. When it is constant I need to copy it to use it in anonymous methods.
  SetLength(AC, Length(C));
  for i := 0 to High(C) do
    AC[i] := C[i];

  p := TSE2MethodUnit.Create;
  if UName = '' then
    p.UnitName := Copy(C[0].ClassName, 2, 255)
  else
    p.UnitName := UName;

  p.DoRegisterMethods := procedure(const Target: TSE2RunAccess)
  var
    Con: TRTTIContext;
    Typ: TRTTIType;
    M: TRTTIMethod;
    Names: TDictionary<string, Byte>;
    i: Byte;
    j: Integer;
  begin
    Con := TRTTIContext.Create;
    Names := TDictionary<string, Byte>.Create;
    try
      for j := 0 to High(AC) do
      begin
        Typ := Con.GetType(AC[j]);

        for M in Typ.GetMethods do
        begin
          // We don't need con/destructors in scripts
          if M.IsConstructor then
          begin
            if Length(M.GetParameters) = 0 then
              Break   // TObject methods reached
            else
              Continue;
          end
          else if M.IsDestructor or ((M.DispatchKind = dkVTable) and (Names.ContainsKey(M.Name))) or (Pos('(basic)', M.ToString) > 0) then
            Continue;

          // Get overload-index
          if not Names.ContainsKey(M.Name) then
          begin
            i := 0;
            Names.Add(M.Name, 0);
          end
          else
          begin
            Names[M.Name] := Names[M.Name] + 1;
            i := Names[M.Name];
          end;

          Target.Method[Format('%s.%s[%d]', [Typ.Name, M.Name, i]), p.UnitName] := M.CodeAddress;
        end;
        Names.Clear;
      end;
    finally
      Con.Free;
      Names.Free;
    end;
  end;

  p.DoGetUnitSource := procedure(var Target: string)
  begin
    Target := GenerateSource(AC, UName);
  end;

  TSE2UnitManager.RegisterUnit(p);
end;

{ TGeneralScriptManager }

constructor TGeneralScriptManager.Create(AClient: TObject);
begin
  FClient := AClient;
  FChar := TScriptMapleCharacter.Create(TMapleClient(FClient).Player);
end;

destructor TGeneralScriptManager.Destroy;
begin
  if FChar.EventInstance = nil then
    FChar.Free;

  inherited;
end;

function TGeneralScriptManager.Char: TScriptMapleCharacter;
begin
  Result := FChar;
end;

function TGeneralScriptManager.GetEventManager(Name: string): TEventManager;
begin
  Result := TMapleClient(FClient).Channel.Events.GetEvent(Name);
end;

procedure TGeneralScriptManager.ClearSavedLocation(LType: string);
var
  iLType: Integer;
begin
  iLType := GetEnumValue(TypeInfo(TSavedLocation), LType);
  TMapleClient(FClient).Player.ClearSavedLocation(TSavedLocation(iLType));
end;

function TGeneralScriptManager.GetSavedLocation(LType: string): Integer;
var
  iLType: Integer;
begin
  iLType := GetEnumValue(TypeInfo(TSavedLocation), LType);
  Result := TMapleClient(FClient).Player.GetSavedLocation(TSavedLocation(iLType));
end;

procedure TGeneralScriptManager.SaveLocation(LType: string);
var
  iLType: Integer;
begin
  iLType := GetEnumValue(TypeInfo(TSavedLocation), LType);
  TMapleClient(FClient).Player.SaveLocation(TSavedLocation(iLType));
end;

function TGeneralScriptManager.GetQuestData(ID: Word): string;
begin
  if not TMapleClient(FClient).Player.Quests.ContainsKey(ID) then
    Exit('');

  Result := TPlayerQuestStatus(TMapleClient(FClient).Player.Quests[ID]).Data;
end;

function TGeneralScriptManager.IsQuestActive(ID: Word): Boolean;
begin
  Result := (TMapleClient(FClient).Player.Quests.ContainsKey(ID)) and
            (TPlayerQuestStatus(TMapleClient(FClient).Player.Quests[ID]).Status in
               [qsStarted, qsAllRequestsDone]);
end;

function TGeneralScriptManager.IsQuestFinished(ID: Word): Boolean;
begin
  Result := (TMapleClient(FClient).Player.Quests.ContainsKey(ID)) and
            (TPlayerQuestStatus(TMapleClient(FClient).Player.Quests[ID]).Status =
               qsCompleted);
end;

function TGeneralScriptManager.IsQuestNotStarted(ID: Word): Boolean;
begin
  Result := (not TMapleClient(FClient).Player.Quests.ContainsKey(ID)) or
            (TPlayerQuestStatus(TMapleClient(FClient).Player.Quests[ID]).Status = qsNotStarted);
end;

procedure TGeneralScriptManager.SilentCompleteQuest(ID: Word);
begin
  if TMapleClient(FClient).Player.Quests.ContainsKey(ID) then
  begin
    TPlayerQuestStatus(TMapleClient(FClient).Player.Quests[ID]).Status := qsCompleted;
    TPlayerQuestStatus(TMapleClient(FClient).Player.Quests[ID]).CompletionTime := GetServerTime;
  end;

  CompleteQuest(TMapleClient(FClient), ID, 0, 0, True);
end;

procedure TGeneralScriptManager.UpdateQuest(ID: Word; CustomData: string;
  SetReqDone: Boolean);
var
  Quest: TPlayerQuestStatus;
begin
  // For example used for Aran Intro (21002). The quest doesn't have to exist.
  if not TMapleClient(FClient).Player.Quests.ContainsKey(ID) then
  begin                                 // can be nil
    Quest := TPlayerQuestStatus.Create(QuestDataProv.Quests[ID], qsStarted, ID);
    TMapleClient(FClient).Player.Quests.Add(ID, Quest);
  end
  else
    Quest := TPlayerQuestStatus(TMapleClient(FClient).Player.Quests[ID]);

  Quest.Data := CustomData;
  if SetReqDone then
    Quest.Status := qsAllRequestsDone;

  TMapleClient(FClient).Write(MaplePacketCreator.UpdateQuest(ID, CustomData));
end;

procedure TGeneralScriptManager.PlayWZSound(Name: string);
begin
  TMapleClient(FClient).Write(MaplePacketCreator.PlayWZSound(Name));
end;

procedure TGeneralScriptManager.ShowGuideEffect(ID, Duration: Integer);
begin
  TMapleClient(FClient).Write(MaplePacketCreator.ShowGuideEffect(ID, Duration));
end;

procedure TGeneralScriptManager.ShowGuideEffectWithText(Text: string; Width,
  Duration: Integer);
begin
  TMapleClient(FClient).Write(MaplePacketCreator.ShowGuideEffectWithText(Text, Width, Duration));
end;

procedure TGeneralScriptManager.ShowInfo(Msg: string);
begin
  TMapleClient(FClient).Write(ShowOrangeText(Msg));
end;

procedure TGeneralScriptManager.ShowInfoOnScreen(Msg: string);
begin
  TMapleClient(FClient).Write(MaplePacketCreator.ShowInfoOnScreen(Msg));
end;

procedure TGeneralScriptManager.ShowWZEffect(Path: string;
  AdditionalInfo: Integer);
begin
  TMapleClient(FClient).Write(MaplePacketCreator.ShowWZEffect(Path, AdditionalInfo));
end;

procedure TGeneralScriptManager.GainItem(ID, Quantity: Integer);
begin
  if Quantity >= 0 then
    TMapleClient(FClient).Player.GainItemByID(ID, Quantity)
  else
    TMapleClient(FClient).Player.RemoveItemByID(ID, -Quantity, False, False);

  TMapleClient(FClient).Write(ShowItemGain(ID, Quantity, True));
end;

function TGeneralScriptManager.GetItemQuantity(ID: Integer): Integer;
var
  L: TList<TItem>;
  I: TItem;
  Inv: TMapleInventory;
begin
  Result := 0;
  Inv := TMapleClient(FClient).Player.Inventory[GetInventory(ID)];
  L := Inv.ListByID(ID);
  try
    for I in L do
      Inc(Result, I.Quantity);
  finally
    L.Free;
  end;
end;

function TGeneralScriptManager.HasItem(ID: Integer): Boolean;
begin
  Result := TMapleClient(FClient).Player.Inventory[GetInventory(ID)].ContainsID(ID);
end;

function TGeneralScriptManager.IsEquipped(ID: Integer): Boolean;
begin
  Result := TMapleClient(FClient).Player.Inventory[miEquipped].ContainsID(ID);
end;

procedure TGeneralScriptManager.RemoveAll(ID: Integer);
var
  L: TList<TItem>;
  I: TItem;
  Inv: TMapleInventory;
begin
  Inv := TMapleClient(FClient).Player.Inventory[GetInventory(ID)];
  L := Inv.ListByID(ID);
  try
    for I in L do
    begin
      Inv.RemoveItem(I.Position, I.Quantity, False);
      TMapleClient(FClient).Write(MaplePacketCreator.ClearInventorySlot(Inv.InvType, I.Position));
      I.Free;
    end;
  finally
    L.Free;
  end;
end;

function TGeneralScriptManager.GetAreaInfo(ID: Integer): string;
begin
  // ID: Internal Quest ID, e.g. 20021 for Cygnus Intro
  if not TMapleClient(FClient).Player.AreaData.TryGetValue(ID, Result) then
    Result := '';
end;

procedure TGeneralScriptManager.UpdateAreaInfo(ID: Integer; NewList: string);
begin
  TMapleClient(FClient).Player.AreaData.AddOrSetValue(ID, NewList);
  TMapleClient(FClient).Write(MaplePacketCreator.UpdateAreaInfo(ID, NewList));
end;

procedure TGeneralScriptManager.UpdateTutorialSummon(Visible: Boolean);
begin
  TMapleClient(FClient).Write(SpawnTutorialSummon(Visible));
end;

procedure TGeneralScriptManager.UpdateSkill(ID, Level, MasterLevel: Integer;
  Reaction: Boolean);
var
  L: Byte;
begin
  L := TMapleClient(FClient).Player.GetSkillLevel(SkillDataProv.GetPlayerSkill(ID));

  if (L > 0) and (Level = -1) then
  begin
    // Delete
    TMapleClient(FClient).Player.Skills.Remove(SkillDataProv.GetPlayerSkill(ID));
    TMapleClient(FClient).Write(MaplePacketCreator.UpdateSkill(ID, Level, MasterLevel, Reaction));
    Exit;
  end;

  if (L = 0) or (L <> Level) then
  begin
    TMapleClient(FClient).Player.ChangeSkillLevel(SkillDataProv.GetPlayerSkill(ID), Level, False);
    TMapleClient(FClient).Write(MaplePacketCreator.UpdateSkill(ID, Level, MasterLevel, Reaction));
  end;
end;

procedure TGeneralScriptManager.LockUI(Disabled: Boolean);
begin
  TMapleClient(FClient).Write(MaplePacketCreator.LockUI(Disabled));
end;

procedure TGeneralScriptManager.DisableUI(Disabled: Boolean);
begin
  TMapleClient(FClient).Write(MaplePacketCreator.DisableUI(Disabled));
end;

function TGeneralScriptManager.Version: Word;
begin
  Result := MAPLE_VERSION;
end;

function GetWarpMap(C: TMapleCharacter; ID: Integer): TMapleMap;
begin
  if C.EventInstance = nil then
    Result := TMapleClient(C.Client).Channel.MapProvider.LoadMap(ID)
  else
    Result := C.EventInstance.Maps.LoadMap(ID);
end;

procedure TGeneralScriptManager.ForceRespawn(MapID: Integer);
var
  M: TMapleMap;
begin
  M := GetWarpMap(TMapleClient(FClient).Player, MapID);
  M.ForceRespawn;
end;

procedure TGeneralScriptManager.KillAllMonsters(MapID: Integer);
var
  M: TMapleMap;
begin
  M := GetWarpMap(TMapleClient(FClient).Player, MapID);
  M.KillAllMonsters;
end;

procedure TGeneralScriptManager.SpawnMonster(MapID, MobID, X, Y, Effect: Integer);
var
  M: TMapleMap;
begin
  M := GetWarpMap(TMapleClient(FClient).Player, MapID);
  M.SpawnMonster(MobID, Point(X, Y), 0, Effect);
end;

procedure TGeneralScriptManager.SpawnNPC(MapID, NPC, X, Y: Integer; FacesRight: Boolean);
var
  M: TMapleMap;
  N: TMapleNPC;
  Obj: TMapleMapObject;
begin
  M := GetWarpMap(TMapleClient(FClient).Player, MapID);
  for Obj in M.GetMapObjects do
    if (Obj.GetType = otNPC) and (TMapleNPC(Obj).ID = NPC) then
      Exit;

  N := TMapleNPC.Create(NPC, '');
  N.Position := Point(X, Y);
  N.MinClickPos := X - 50;
  N.MaxClickPos := X + 50;
  N.FacesRight := FacesRight;
  M.AddMapObject(N);
  //M.BroadcastMessage(N.GetSpawnPacket);  script gets executed before SendObjectsToClient (at least if it is a Map-Script)
end;

procedure TGeneralScriptManager.RemoveNPC(MapID, NPC: Integer);
var
  M: TMapleMap;
  Obj: TMapleMapObject;
begin
  M := GetWarpMap(TMapleClient(FClient).Player, MapID);
  for Obj in M.GetMapObjects do
    if (Obj.GetType = otNPC) and (TMapleNPC(Obj).ID = NPC) then
    begin
      TMapleClient(FClient).Write(TMapleNPC(Obj).GetStopControlling);
      TMapleClient(FClient).Write(TMapleNPC(Obj).GetRemovePacket);
      Exit;
    end;
end;

function TGeneralScriptManager.CountPlayersOnMap(ID: Integer): Integer;
begin
  Result := GetWarpMap(TMapleClient(FClient).Player, ID).Characters.Count;
end;

procedure TGeneralScriptManager.Warp(Map: Integer);
begin
  TMapleClient(FClient).Player.ChangeMap(GetWarpMap(TMapleClient(FClient).Player, Map));
end;

procedure TGeneralScriptManager.Warp(Map: Integer; Portal: Byte);
var
  NewMap: TMapleMap;
begin
  NewMap := TMapleClient(FClient).Channel.MapProvider.LoadMap(Map);
  TMapleClient(FClient).Player.ChangeMap(NewMap, NewMap.GetPortal(Portal));
end;

procedure TGeneralScriptManager.Warp(Map: Integer; Portal: string);
var
  NewMap: TMapleMap;
begin
  NewMap := TMapleClient(FClient).Channel.MapProvider.LoadMap(Map);
  TMapleClient(FClient).Player.ChangeMap(NewMap, NewMap.GetPortal(Portal));
end;

{ TScriptMapleCharacter }

constructor TScriptMapleCharacter.Create(AChar: TObject);
begin
  FChar := AChar;
end;

destructor TScriptMapleCharacter.Destroy;
begin
  TMapleCharacter(FChar).EventChar := nil;

  inherited;
end;

procedure TScriptMapleCharacter.ChangeMap(ID, Portal: Integer);
var
  M: TMapleMap;
begin
  M := GetWarpMap(TMapleCharacter(FChar), ID);
  TMapleCharacter(FChar).ChangeMap(M, M.GetPortal(Portal));
end;

procedure TScriptMapleCharacter.SendUpdateStats;
begin
  TMapleCharacter(FChar).SendUpdateStats;
end;

function TScriptMapleCharacter.Gender: Byte;
begin
  Result := TMapleCharacter(FChar).Gender;
end;

function TScriptMapleCharacter.GetAP: Integer;
begin
  Result := TMapleCharacter(FChar).RemainingAP;
end;

function TScriptMapleCharacter.GetDEX: Integer;
begin
  Result := TMapleCharacter(FChar).Dex;
end;

function TScriptMapleCharacter.GetEventInstance: TEventInstance;
begin
  Result := TMapleCharacter(FChar).EventInstance;
end;

function TScriptMapleCharacter.GetHP: Integer;
begin
  Result := TMapleCharacter(FChar).HP;
end;

function TScriptMapleCharacter.GetINT: Integer;
begin
  Result := TMapleCharacter(FChar).Int;
end;

function TScriptMapleCharacter.GetLUK: Integer;
begin
  Result := TMapleCharacter(FChar).Luk;
end;

function TScriptMapleCharacter.GetMaxHP: Integer;
begin
  Result := TMapleCharacter(FChar).MaxHP;
end;

function TScriptMapleCharacter.GetMaxMP: Integer;
begin
  Result := TMapleCharacter(FChar).MaxMP;
end;

function TScriptMapleCharacter.GetMP: Integer;
begin
  Result := TMapleCharacter(FChar).MP;
end;

function TScriptMapleCharacter.GetQuestStatus(ID: Word): Byte;
var
  Q: TObject;
begin
  if TMapleCharacter(FChar).Quests.TryGetValue(ID, Q) then
    Result := Byte(TPlayerQuestStatus(Q).Status)
  else
    Result := 0;
end;

function TScriptMapleCharacter.GetSP: Integer;
begin
  Result := TMapleCharacter(FChar).RemainingSP;
end;

function TScriptMapleCharacter.GetSTR: Integer;
begin
  Result := TMapleCharacter(FChar).Str;
end;

function TScriptMapleCharacter.Job: Word;
begin
  Result := Word(TMapleCharacter(FChar).Job);
end;

function TScriptMapleCharacter.Level: Byte;
begin
  Result := TMapleCharacter(FChar).Level;
end;

function TScriptMapleCharacter.MapID: Integer;
begin
  Result := TMapleCharacter(FChar).Map.ID;
end;

function TScriptMapleCharacter.Mesos: Integer;
begin
  Result := TMapleCharacter(FChar).Mesos;
end;

function TScriptMapleCharacter.Name: string;
begin
  Result := TMapleCharacter(FChar).Name;
end;

procedure TScriptMapleCharacter.SetAP(Value: Integer);
begin
  TMapleCharacter(FChar).RemainingAP := Value;
end;

procedure TScriptMapleCharacter.SetDEX(Value: Integer);
begin
  TMapleCharacter(FChar).Dex := Value;
end;

procedure TScriptMapleCharacter.SetEventInstance(Value: TEventInstance);
begin
  TMapleCharacter(FChar).EventChar := Self;
  TMapleCharacter(FChar).EventInstance := Value;
end;

procedure TScriptMapleCharacter.SetINT(Value: Integer);
begin
  TMapleCharacter(FChar).Int := Value;
end;

procedure TScriptMapleCharacter.SetLUK(Value: Integer);
begin
  TMapleCharacter(FChar).Luk := Value;
end;

procedure TScriptMapleCharacter.SetHP(Value: Integer);
begin
  TMapleCharacter(FChar).HP := Value;
  TMapleCharacter(FChar).UpdateSingleStat(msHP, Value);
end;

procedure TScriptMapleCharacter.SetMaxHP(Value: Integer);
begin
  TMapleCharacter(FChar).MaxHP := Value;
  TMapleCharacter(FChar).UpdateSingleStat(msMaxHP, Value);
end;

procedure TScriptMapleCharacter.SetMaxMP(Value: Integer);
begin
  TMapleCharacter(FChar).MaxMP := Value;
  TMapleCharacter(FChar).UpdateSingleStat(msMaxMP, Value);
end;

procedure TScriptMapleCharacter.SetMP(Value: Integer);
begin
  TMapleCharacter(FChar).MP := Value;
  TMapleCharacter(FChar).UpdateSingleStat(msMP, Value);
end;

procedure TScriptMapleCharacter.SetSP(Value: Integer);
begin
  TMapleCharacter(FChar).RemainingSP := Value;
  TMapleCharacter(FChar).UpdateSingleStat(msRemainingSP, Value);
end;

procedure TScriptMapleCharacter.SetSTR(Value: Integer);
begin
  TMapleCharacter(FChar).Str := Value;
end;

initialization
  TSE2Console.Write := Log;
  TClassImporter.Register([TScriptMapleCharacter]);

end.
