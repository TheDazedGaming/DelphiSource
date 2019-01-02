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

unit QuestScript;

interface

uses Classes, SysUtils, uSE2Compiler, uSE2Runtime, uSE2PEData, uSE2Opcode,
     uSE2Errors, ScriptHelper, NPCConversation;

type
  TQuestScriptManager = class;

  TQuestScript = class
  private
    FQuestMan: TQuestScriptManager;
    FCompiler: TSE2Compiler;
    FCode: TSE2PE;
    FRuntime: TSE2Runtime;
    FStart, FComplete: Pointer;

    procedure CompileError(Sender: TObject; ErrorType: TSE2ErrorType; ErrorUnit, ErrorText: string; ErrorPos, ErrorLine: Integer; UserData: TObject);
    procedure RuntimeError(Sender: TObject; Exp: ExceptClass; const Msg: string; CodePos: Integer; const CallStack: string);
    procedure Aborted(Sender: TObject);
  public
    constructor Create(FileName: string; QMan: TQuestScriptManager);
    destructor Destroy; override;

    procedure Abort;
    procedure Start(Mode, LastType, Selection: Integer);
    procedure Complete(Mode, LastType, Selection: Integer);
  end;

  TQuestScriptManager = class(TNPCConversation)
  private
    FQuestID: Integer;
    FScript: TQuestScript;
    FStart: Boolean;
  public
    constructor Create(QuestID, NPCID: Integer; Client: TObject; FileName: string; Start: Boolean); overload;
    destructor Destroy; override;

    procedure Action(Mode, LastType, Selection: Integer); override;
    function Run: Boolean;

    // called by the script only!
    procedure StartQuest;
    procedure FinishQuest;
  end;

implementation

uses Main, MapleClient;

{ TQuestScript }

constructor TQuestScript.Create(FileName: string; QMan: TQuestScriptManager);
var
  Script: TStringList;
begin
  FQuestMan := QMan;

  FCompiler := TSE2Compiler.Create;
  FCompiler.OnCompilerError := CompileError;
  Script := TStringList.Create;
  try
    Script.LoadFromFile(FileName);
    FCode := FCompiler.Compile('program Script; uses QuestScriptManager, ScriptMapleCharacter;' + Script.Text + ' begin end.');
  finally
    Script.Free;
  end;

  FRuntime := TSE2Runtime.Create;
  FRuntime.OnError := RuntimeError;
  FRuntime.OnAborted := Aborted;
  FRuntime.AppCode := FCode;
  FRuntime.Initialize;
  FRuntime.Run;
  FStart := FRuntime.CodeAccess.FindMethod('Start', '', [pmIn, pmIn, pmIn, pmIn], [btObject, btS32, btS32, btS32]);
  FComplete := FRuntime.CodeAccess.FindMethod('Complete', '', [pmIn, pmIn, pmIn, pmIn], [btObject, btS32, btS32, btS32]);
end;

destructor TQuestScript.Destroy;
begin
  if Assigned(FRuntime) then
  begin
    FRuntime.Finalize;
    FRuntime.AppCode := nil;
    FRuntime.Free;
    FCode.Free;
  end;

  FCompiler.Free;

  inherited;
end;

procedure TQuestScript.Abort;
begin
  if FRuntime.CodePos > 0 then
    FRuntime.Abort
  else
    Aborted(nil);
end;

procedure TQuestScript.Aborted(Sender: TObject);
begin
  Log('[Quest] Destructing conversation!');
  Free;
end;

procedure TQuestScript.CompileError(Sender: TObject; ErrorType: TSE2ErrorType;
  ErrorUnit, ErrorText: string; ErrorPos, ErrorLine: Integer;
  UserData: TObject);
begin
  raise Exception.Create(ErrorText);
end;

procedure TQuestScript.RuntimeError(Sender: TObject; Exp: ExceptClass;
  const Msg: string; CodePos: Integer; const CallStack: string);
begin
  raise Exception.Create(Msg);
end;

procedure TQuestScript.Start(Mode, LastType, Selection: Integer);
begin
  if FStart <> nil then
    FRuntime.Call(FStart, [FQuestMan, Mode, LastType, Selection])
  else
    Log('Start called, but not found in script');
end;

procedure TQuestScript.Complete(Mode, LastType, Selection: Integer);
begin
  if FComplete <> nil then
    FRuntime.Call(FComplete, [FQuestMan, Mode, LastType, Selection])
  else
    Log('Complete called, but not found in script');
end;

{ TQuestScriptManager }

constructor TQuestScriptManager.Create(QuestID, NPCID: Integer;
  Client: TObject; FileName: string; Start: Boolean);
begin
  inherited Create(Client);
  FEnded := False;
  FNPCID := NPCID;
  FQuestID := QuestID;
  FStart := Start;

  try
    FScript := TQuestScript.Create(FileName, Self);
  except
    Log(ExtractFileName(FileName) + ': ' + Exception(ExceptObject).Message);
    FScript := nil;
  end;
end;

destructor TQuestScriptManager.Destroy;
begin
  if Assigned(FScript) then
    FScript.Abort;

  inherited;
end;

function TQuestScriptManager.Run: Boolean;
begin
  // Do NOT run this directly in Create, it will fuck things up if it gets disposed while still in the constructor.
  Result := Assigned(FScript);
  if Result then
    if FStart then
      FScript.Start(1, 0, -1)
    else
      FScript.Complete(1, 0, -1);
end;

procedure TQuestScriptManager.StartQuest;
begin
  TMapleClient(FClient).Player.StartQuest(FQuestID, FNPCID);
end;

procedure TQuestScriptManager.FinishQuest;
begin
  TMapleClient(FClient).Player.FinishQuest(FQuestID, FNPCID);
end;

procedure TQuestScriptManager.Action(Mode, LastType, Selection: Integer);
begin
  if Assigned(FScript) then
    if FStart then
      FScript.Start(Mode, LastType, Selection)
    else
      FScript.Complete(Mode, LastType, Selection)
  else
    Dispose;
end;

initialization
  TClassImporter.Register([TQuestScriptManager]);

end.
