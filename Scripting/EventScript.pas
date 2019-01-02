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

unit EventScript;

interface

uses Windows, SysUtils, Classes, Generics.Collections, uSE2Compiler, uSE2Runtime,
     uSE2PEData, uSE2Errors, uSE2Opcode, Variants, MapDataProvider, Scheduler;

type
  TEventInstance = class;

  TEventManager = class
  private
    FName: string;
    FCompiler: TSE2Compiler;
    FCode: TSE2PE;
    FRuntime: TSE2Runtime;
    FInstances: TDictionary<string, TEventInstance>;

    procedure CompileError(Sender: TObject; ErrorType: TSE2ErrorType; ErrorUnit, ErrorText: string; ErrorPos, ErrorLine: Integer; UserData: TObject);
    procedure RuntimeError(Sender: TObject; Exp: ExceptClass; const Msg: string; CodePos: Integer; const CallStack: string);
  public
    constructor Create(const EventName: string);
    destructor Destroy; override;

    function CreateInstance(const Name: string): TEventInstance;
    procedure RemoveInstance(const Name: string);
    function Invoke(const Method: string; const ParamModes: array of TSE2ParamMode;
      const ParamTypes: array of TSE2TypeIdent; const Params: array of const; MustCall: Boolean = True): Variant;
  end;

  TEventScriptManager = class
  private
    FEvents: TDictionary<string, TEventManager>;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEvent(const Name: string): TEventManager;
  end;

  TCast = class(TCustomAttribute)
  private
    FCast: string;
  public
    constructor Create(const ARealClass: string);
    property Cast: string read FCast;
  end;

  TEventInstance = class
  private
    FManager: TEventManager;
    FName: string;
    FCharacters: TList<TObject>;
    FMaps: TMapDataProvider;
    FClockStart, FClockTime: Cardinal;
    FTimers: TList<THandle>;
  public
    constructor Create(AManager: TEventManager; const AName: string);
    destructor Destroy; override;

    [TCast('TScriptMapleCharacter')]
    function FirstPlayer: TObject;

    procedure RegisterPlayer(Char: TObject);
    function RevivePlayer(Char: TObject): Boolean;
    procedure PlayerDisconnected(Char: TObject);
    procedure UnregisterPlayer(Char: TObject);

    procedure StartClock(RemainingSeconds: Integer);
    procedure Schedule(const Method: string; Delay: Integer);

    property Maps: TMapDataProvider read FMaps;
  end;

implementation

uses Main, ScriptHelper, MapleCharacter, MapleMap, MaplePacketCreator;

{ TEventManager }

constructor TEventManager.Create(const EventName: string);
var
  Script: TStringList;
begin
  FName := EventName;

  FCompiler := TSE2Compiler.Create;
  FCompiler.OnCompilerError := CompileError;
  Script := TStringList.Create;
  try
    Script.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Scripts\Event\' + FName);
    if Pos('end.', Script.Text) < Length(Script.Text) - 10 then
      Script.Add('begin end.');
    FCode := FCompiler.Compile('program Script; uses Events;' + Script.Text);
  finally
    Script.Free;
  end;

  FRuntime := TSE2Runtime.Create;
  FRuntime.OnError := RuntimeError;
  FRuntime.AppCode := FCode;
  FRuntime.Initialize;
  FRuntime.Run;

  FInstances := TDictionary<string, TEventInstance>.Create;
end;

destructor TEventManager.Destroy;
var
  I: TEventInstance;
begin
  if Assigned(FRuntime) then
  begin
    FRuntime.Finalize;
    FRuntime.AppCode := nil;
    FRuntime.Free;
    FCode.Free;
  end;

  FCompiler.Free;

  for I in FInstances.Values do
    I.Free;

  FInstances.Free;

  inherited;
end;

function TEventManager.Invoke(const Method: string;
  const ParamModes: array of TSE2ParamMode; const ParamTypes: array of TSE2TypeIdent;
  const Params: array of const; MustCall: Boolean = True): Variant;
var
  P: Pointer;
begin
  Result := Variants.Null;
  P := FRuntime.CodeAccess.FindMethod(Method, '', ParamModes, ParamTypes);
  if P <> nil then
    Result := FRuntime.Call(P, Params)
  else if MustCall then
    raise Exception.Create('Method not found: ' + Method);
end;

procedure TEventManager.CompileError(Sender: TObject; ErrorType: TSE2ErrorType;
  ErrorUnit, ErrorText: string; ErrorPos, ErrorLine: Integer;
  UserData: TObject);
begin
  raise Exception.Create(ErrorText);
end;

procedure TEventManager.RuntimeError(Sender: TObject; Exp: ExceptClass;
  const Msg: string; CodePos: Integer; const CallStack: string);
begin
  raise Exception.Create(Msg);
end;

function TEventManager.CreateInstance(const Name: string): TEventInstance;
begin
  Result := TEventInstance.Create(Self, Name);
  FInstances.Add(Name, Result);
end;

procedure TEventManager.RemoveInstance(const Name: string);
begin
  FInstances.Remove(Name);
end;

{ TEventScriptManager }

constructor TEventScriptManager.Create;
var
  F: TSearchRec;
begin
  FEvents := TDictionary<string, TEventManager>.Create;

  if FindFirst(ExtractFilePath(ParamStr(0)) + 'Scripts\Event\*' + SCRIPT_EXT, faAnyFile, F) <> 0 then
    Exit;

  repeat
    FEvents.Add(ChangeFileExt(F.Name, ''), TEventManager.Create(F.Name));
  until FindNext(F) <> 0;
  FindClose(F);
end;

destructor TEventScriptManager.Destroy;
var
  M: TEventManager;
begin
  for M in FEvents.Values do
    M.Free;

  FEvents.Free;

  inherited;
end;

function TEventScriptManager.GetEvent(const Name: string): TEventManager;
begin
  if not FEvents.TryGetValue(Name, Result) then
    Result := nil;
end;

{ TEventInstanceManager }

constructor TEventInstance.Create(AManager: TEventManager; const AName: string);
begin
  FManager := AManager;
  FName := AName;
  FCharacters := TList<TObject>.Create;
  FMaps := TMapDataProvider.Create;
  FTimers := TList<THandle>.Create;
end;

destructor TEventInstance.Destroy;
var
  SMC: TObject;
  T: THandle;
begin
  Log('[EventInstance] Destructing');
  FManager.RemoveInstance(FName);
  for SMC in FCharacters do
  begin
    TScriptMapleCharacter(SMC).EventInstance := nil;
    SMC.Free;
  end;
  FCharacters.Free;
  FMaps.Free;

  for T in FTimers do
    Sched.CancelSchedule(T);
  FTimers.Free;

  inherited;
end;

function TEventInstance.FirstPlayer: TObject;
begin
  Result := FCharacters[0];
end;

procedure TEventInstance.RegisterPlayer(Char: TObject);
begin
  FCharacters.Add(Char);
  TScriptMapleCharacter(Char).EventInstance := Self;
  FManager.Invoke('PlayerEntry', [pmIn, pmIn], [btObject, btObject], [Self, Char]);
end;

function TEventInstance.RevivePlayer(Char: TObject): Boolean;
var
  V: Variant;
begin
  V := FManager.Invoke('RevivePlayer', [pmIn, pmIn, pmResult], [btObject, btObject, btBoolean], [Self, Char], False);
  Result := VarIsNull(V) or Boolean(V);   // True: Standard procedure; False: Script warped
end;

procedure TEventInstance.PlayerDisconnected(Char: TObject);
begin
  FCharacters.Remove(Char);
  FManager.Invoke('PlayerDisconnected', [pmIn, pmIn], [btObject, btObject], [Self, Char], False);

  if FCharacters.Count = 0 then
    Free;
end;

procedure TEventInstance.UnregisterPlayer(Char: TObject);
begin
  FCharacters.Remove(Char);
  TScriptMapleCharacter(Char).EventInstance := nil;
  Char.Free;
end;

procedure TEventInstance.Schedule(const Method: string; Delay: Integer);
var
  T: THandle;
begin
  T := Sched.AddSchedule(Delay * 1000, procedure
  begin
    FTimers.Remove(T);
    FManager.Invoke(Method, [pmIn], [btObject], [Self]);
  end);
  FTimers.Add(T);
end;

procedure TEventInstance.StartClock(RemainingSeconds: Integer);
var
  M: TMapleMap;
begin
  FClockStart := GetTickCount;
  FClockTime := RemainingSeconds;
  for M in FMaps.Maps do
    M.BroadcastMessage(ShowClock(FClockTime));
end;

{ TCastAttribute }

constructor TCast.Create(const ARealClass: string);
begin
  FCast := ARealClass;
end;

initialization
  TClassImporter.Register([TEventManager, TEventInstance, TScriptMapleCharacter], 'Events');

end.
