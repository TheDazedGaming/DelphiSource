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

unit PortalScript;

interface

uses Classes, SysUtils, uSE2Compiler, uSE2Runtime, uSE2PEData, uSE2Opcode,
     uSE2Errors, ScriptHelper, NPCConversation;

type
  TPortalScript = class(TGeneralScriptManager)
  private
    FCompiler: TSE2Compiler;
    FCode: TSE2PE;
    FRuntime: TSE2Runtime;
    FEnter: Pointer;

    FChildScript: TPortalScript;
    FFilePath: string;
    FID: Integer;

    procedure CompileError(Sender: TObject; ErrorType: TSE2ErrorType; ErrorUnit, ErrorText: string; ErrorPos, ErrorLine: Integer; UserData: TObject);
    procedure RuntimeError(Sender: TObject; Exp: ExceptClass; const Msg: string; CodePos: Integer; const CallStack: string);
  public
    constructor Create(FileName: string; Client: TObject; ID: Integer = -1);
    destructor Destroy; override;

    procedure Enter;

    procedure BlockPortal;
    procedure PlayPortalSound;
    procedure InvokeNPC(ID: Integer);
    procedure RunScript(Name: string);
    procedure ShowInstructionBubble(Msg: string; Width, Height: Integer);
    procedure ShowMapEffect(Name: string);
    procedure UnblockPortal;
    procedure UpdateRemainingSP(Value: Integer);

    function ID: Integer;
  end;

implementation

uses Main, MapleCharacter, MapleClient, MaplePacketCreator;

{ TPortalScript }

constructor TPortalScript.Create(FileName: string; Client: TObject; ID: Integer = -1);
var
  Script: TStringList;
begin
  inherited Create(Client);

  FChildScript := nil;
  FFilePath := ExtractFilePath(FileName);
  FID := ID;

  FCompiler := TSE2Compiler.Create;
  FCompiler.OnCompilerError := CompileError;
  Script := TStringList.Create;
  try
    Script.LoadFromFile(FileName);
    FCode := FCompiler.Compile('program Script; uses PortalScript, ScriptMapleCharacter;' + Script.Text + ' begin end.');
  finally
    Script.Free;
  end;

  FRuntime := TSE2Runtime.Create;
  FRuntime.OnError := RuntimeError;
  FRuntime.AppCode := FCode;
  FRuntime.Initialize;
  FRuntime.Run;
  FEnter := FRuntime.CodeAccess.FindMethod('Enter', '', [pmIn], [btObject]);
end;

destructor TPortalScript.Destroy;
begin
  if Assigned(FRuntime) then
  begin
    FRuntime.Finalize;
    FRuntime.AppCode := nil;
    FRuntime.Free;
    FCode.Free;
  end;

  FCompiler.Free;

  if Assigned(FChildScript) then
    FChildScript.Free;

  inherited;
end;

procedure TPortalScript.CompileError(Sender: TObject; ErrorType: TSE2ErrorType;
  ErrorUnit, ErrorText: string; ErrorPos, ErrorLine: Integer;
  UserData: TObject);
begin
  raise Exception.Create(ErrorText);
end;

procedure TPortalScript.RuntimeError(Sender: TObject; Exp: ExceptClass;
  const Msg: string; CodePos: Integer; const CallStack: string);
begin
  raise Exception.Create(Msg);
end;

procedure TPortalScript.Enter;
begin
  if FEnter <> nil then
    FRuntime.Call(FEnter, [Self])
  else
    Log('PortalScript corrupt');
end;

procedure TPortalScript.BlockPortal;
begin
  if FID > -1 then
  begin
    if not TMapleClient(FClient).Player.UsedPortals.Contains(FID) then
      TMapleClient(FClient).Player.UsedPortals.Add(FID);
    TMapleClient(FClient).Write(PortalBlocked);
  end
  else
    Log('[PortalScript] Blocking portal failed - no ID');
end;

function TPortalScript.ID: Integer;
begin
  Result := FID;
end;

procedure TPortalScript.InvokeNPC(ID: Integer);
var
  F: string;
begin
  if Assigned(TMapleClient(FClient).Player.CurConversation) then
    Exit;

  if FindScript(ID, 'NPC', F) then
  begin
    TMapleClient(FClient).Player.CurConversation := TNPCConversation.Create(ID, FClient, F);
    TMapleClient(FClient).Player.CurConversation.Start;
  end;
end;

procedure TPortalScript.PlayPortalSound;
begin
  TMapleClient(FClient).Write(ShowSpecialEffect(sePortal));
end;

procedure TPortalScript.RunScript(Name: string);
begin
  FChildScript := TPortalScript.Create(FFilePath + Name + SCRIPT_EXT, FClient);
  FChildScript.Enter;
end;

procedure TPortalScript.ShowInstructionBubble(Msg: string; Width, Height: Integer);
begin
  TMapleClient(FClient).Write(InstructionBubble(Msg, Width, Height));
end;

procedure TPortalScript.ShowMapEffect(Name: string);
begin
  TMapleClient(FClient).Write(MaplePacketCreator.ShowMapEffect(Name));
end;

procedure TPortalScript.UnblockPortal;
begin
  if FID > -1 then
    TMapleClient(FClient).Player.UsedPortals.Remove(FID)
  else
    Log('[PortalScript] Unblocking portal failed - no ID');
end;

procedure TPortalScript.UpdateRemainingSP(Value: Integer);
begin
  TMapleClient(FClient).Player.RemainingSP := Value;
  TMapleClient(FClient).Player.UpdateSingleStat(msRemainingSP, Value);
end;

initialization
  TClassImporter.Register([TPortalScript]);

end.
