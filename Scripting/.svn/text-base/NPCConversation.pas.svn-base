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

unit NPCConversation;

interface

uses Classes, SysUtils, uSE2Compiler, uSE2Runtime, uSE2PEData, uSE2Opcode,
     uSE2Errors, ScriptHelper, MapleNPC;

type
  TNPCConversation = class;

  TNPCScript2 = class
  private
    FConversation: TNPCConversation;
    FCompiler: TSE2Compiler;
    FCode: TSE2PE;
    FRuntime: TSE2Runtime;
    FAction: Pointer;

    procedure CompileError(Sender: TObject; ErrorType: TSE2ErrorType; ErrorUnit, ErrorText: string; ErrorPos, ErrorLine: Integer; UserData: TObject);
    procedure RuntimeError(Sender: TObject; Exp: ExceptClass; const Msg: string; CodePos: Integer; const CallStack: string);
    procedure Aborted(Sender: TObject);
  public
    constructor Create(FileName: string; Conv: TNPCConversation);
    destructor Destroy; override;

    procedure Abort;
    procedure Action(Mode, LastType, Selection: Integer);
    procedure Start;
  end;

  TNPCConversation = class(TGeneralScriptManager)
  private
    FScript: TNPCScript2;
  protected
    FEnded: Boolean;
    FNPCID: Integer;
  public
    constructor Create(NPCID: Integer; Client: TObject; FileName: string); overload;
    destructor Destroy; override;

    procedure Dispose;

    procedure Start;
    procedure Action(Mode, LastType, Selection: Integer); virtual;

    procedure SendAcceptDecline(Msg: string);
    procedure SendMapSelection(Choices: string);
    procedure SendNext(Msg: string); overload;
    procedure SendNext(Msg: string; Speaker: Byte); overload;
    procedure SendNextPrev(Msg: string); overload;
    procedure SendNextPrev(Msg: string; Speaker: Byte); overload;
    procedure SendNextPrev(Msg: string; Speaker: Byte; OtherNPC: Integer); overload;
    procedure SendPrev(Msg: string); overload;
    procedure SendPrev(Msg: string; Speaker: Byte); overload;
    procedure SendPrev(Msg: string; Speaker: Byte; OtherNPC: Integer); overload;
    procedure SendOK(Msg: string); overload;
    procedure SendOK(Msg: string; Speaker: Byte); overload;
    procedure SendOK(Msg: string; Speaker: Byte; OtherNPC: Integer); overload;
    procedure SendSimple(Msg: string);
    procedure SendYesNo(Msg: string);
    procedure ShowSpecialAnimation(Name: string);

    procedure ChangeJob(NewID: Word);
    procedure ClearInventorySlot(Inv, Slot: Integer);
    procedure EndConversation;
    procedure GainExp(Value: Integer);
    procedure GainMesos(Value: Integer);
    procedure IncreaseSlotLimit(InvID, RowsToAdd: Byte);
    procedure TrembleEffect(Mode, Delay: Integer);
    procedure UseItem(ID: Integer);

    function NPCID: Integer;
  end;

implementation

uses Main, MapleClient, MapleCharacter, MaplePacketCreator, GameLogic, PlayerInventory,
     MapleMapObject;

{ TNPCScript2 }

constructor TNPCScript2.Create(FileName: string; Conv: TNPCConversation);
var
  Script: TStringList;
begin
  FConversation := Conv;

  FCompiler := TSE2Compiler.Create;
  FCompiler.OnCompilerError := CompileError;
  Script := TStringList.Create;
  try
    Script.LoadFromFile(FileName);
    FCode := FCompiler.Compile('program Script;' + Script.Text);
  finally
    Script.Free;
  end;
end;

destructor TNPCScript2.Destroy;
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

procedure TNPCScript2.CompileError(Sender: TObject; ErrorType: TSE2ErrorType; ErrorUnit,
  ErrorText: string; ErrorPos, ErrorLine: integer; UserData: TObject);
begin
  raise Exception.Create('Line ' + IntToStr(ErrorLine) + ': ' + ErrorText);
end;

procedure TNPCScript2.RuntimeError(Sender: TObject; Exp: ExceptClass;
const Msg: string; CodePos: integer; const CallStack: string);
begin
  raise Exception.Create(Msg);
end;

procedure TNPCScript2.Abort;
begin
  if FRuntime.CodePos > 0 then
    FRuntime.Abort
  else
    Aborted(nil);
end;

procedure TNPCScript2.Aborted(Sender: TObject);
begin
  Log('[NPC] Destructing conversation!');
  Free;
end;

procedure TNPCScript2.Action(Mode, LastType, Selection: Integer);
begin
  if FAction = nil then
    raise Exception.Create('Method not found: Action');
  FRuntime.Call(FAction, [FConversation, Mode, LastType, Selection]);
end;

procedure TNPCScript2.Start;
begin
  FRuntime := TSE2Runtime.Create;
  FRuntime.OnError := RuntimeError;
  FRuntime.OnAborted := Aborted;
  FRuntime.AppCode := FCode;
  FRuntime.Initialize;
  FRuntime.Run;
  FAction := FRuntime.CodeAccess.FindMethod('Action', '', [pmIn, pmIn, pmIn, pmIn], [btObject, btS32, btS32, btS32]);
  if FAction <> nil then
    FRuntime.Call(FAction, [FConversation, 1, 0, -1])
  else
    Log('[NPC] Only main script function');
end;

{ TNPCConversation }

constructor TNPCConversation.Create(NPCID: Integer; Client: TObject; FileName: string);
begin
  inherited Create(Client);
  FEnded := False;
  FNPCID := NPCID;
  try
    FScript := TNPCScript2.Create(FileName, Self);
  except
    Log(ExtractFileName(FileName) + ': ' + Exception(ExceptObject).Message);
    FScript := nil;
  end;
end;

destructor TNPCConversation.Destroy;
begin
  if Assigned(FScript) then
    FScript.Abort;

  inherited;
end;

procedure TNPCConversation.Action(Mode, LastType, Selection: Integer);
begin
  FScript.Action(Mode, LastType, Selection);
end;

procedure TNPCConversation.Start;
begin
  if FScript <> nil then
    FScript.Start
  else
    Dispose;
end;

procedure TNPCConversation.Dispose;
begin
  if not FEnded then  // After being warped another conversation might have already started
    TMapleClient(FClient).Player.CurConversation := nil;
  Free;
end;

procedure TNPCConversation.EndConversation;
begin
  // Needed when warping to a map which executes a NPC in its enter-script
  TMapleClient(FClient).Player.CurConversation := nil;
  FEnded := True;
end;

procedure TNPCConversation.SendAcceptDecline(Msg: string);
begin
  TMapleClient(FClient).Write(TMapleNPC.GetTalkPacket(FNPCID, 12, Msg, []));
end;

procedure TNPCConversation.SendMapSelection(Choices: string);
begin
  TMapleClient(FClient).Write(TMapleNPC.GetTalkPacket(FNPCID, 14, Choices, []));
end;

procedure TNPCConversation.SendNext(Msg: string);
begin
  SendNext(Msg, 0);
end;

procedure TNPCConversation.SendNext(Msg: string; Speaker: Byte);
begin
  TMapleClient(FClient).Write(TMapleNPC.GetTalkPacket(FNPCID, 0, Msg, [0, 1], Speaker));
end;

procedure TNPCConversation.SendNextPrev(Msg: string);
begin
  SendNextPrev(Msg, 0, 0);
end;

procedure TNPCConversation.SendNextPrev(Msg: string; Speaker: Byte);
begin
  SendNextPrev(Msg, Speaker, 0);
end;

procedure TNPCConversation.SendNextPrev(Msg: string; Speaker: Byte; OtherNPC: Integer);
begin
  TMapleClient(FClient).Write(TMapleNPC.GetTalkPacket(FNPCID, 0, Msg, [1, 1], Speaker, OtherNPC));
end;

procedure TNPCConversation.SendPrev(Msg: string);
begin
  SendPrev(Msg, 0, 0);
end;

procedure TNPCConversation.SendPrev(Msg: string; Speaker: Byte);
begin
  SendPrev(Msg, Speaker, 0);
end;

procedure TNPCConversation.SendPrev(Msg: string; Speaker: Byte; OtherNPC: Integer);
begin
  TMapleClient(FClient).Write(TMapleNPC.GetTalkPacket(FNPCID, 0, Msg, [1, 0], Speaker, OtherNPC));
end;

procedure TNPCConversation.SendOK(Msg: string);
begin
  SendOK(Msg, 0);
end;

procedure TNPCConversation.SendOK(Msg: string; Speaker: Byte);
begin
  SendOK(Msg, Speaker, 0);
end;

procedure TNPCConversation.SendOK(Msg: string; Speaker: Byte; OtherNPC: Integer);
begin
  TMapleClient(FClient).Write(TMapleNPC.GetTalkPacket(FNPCID, 0, Msg, [0, 0], Speaker, OtherNPC));
end;

procedure TNPCConversation.SendSimple(Msg: string);
begin
  TMapleClient(FClient).Write(TMapleNPC.GetTalkPacket(FNPCID, 4, Msg, []));
end;

procedure TNPCConversation.SendYesNo(Msg: string);
begin
  TMapleClient(FClient).Write(TMapleNPC.GetTalkPacket(FNPCID, 1, Msg, []));
end;

procedure TNPCConversation.ShowSpecialAnimation(Name: string);
var
  Obj: TMapleMapObject;
begin
  for Obj in TMapleClient(FClient).Player.Map.GetMapObjects do
    if (Obj.GetType = otNPC) and (TMapleNPC(Obj).ID = FNPCID) then
    begin
      TMapleClient(FClient).Write(TMapleNPC(Obj).GetSpecialAnimation(Name));
      Exit;
    end;
end;

procedure TNPCConversation.ChangeJob(NewID: Word);
begin
  TMapleClient(FClient).Player.ChangeJob(TMapleJob(NewID));
end;

procedure TNPCConversation.ClearInventorySlot(Inv, Slot: Integer);
begin
  // Used by an Aran quest to remove the player's weapon o.o
  if Slot < 0 then
    TMapleClient(FClient).Player.Inventory[miEquipped].RemoveSlot(Slot)
  else
    TMapleClient(FClient).Player.Inventory[TMapleInventoryType(Inv)].RemoveSlot(Slot);

  TMapleClient(FClient).Write(MaplePacketCreator.ClearInventorySlot(
    TMapleInventoryType(Inv), Slot, False));
end;

procedure TNPCConversation.GainExp(Value: Integer);
begin
  TMapleClient(FClient).Player.GainExp(Value, True, True);
end;

procedure TNPCConversation.GainMesos(Value: Integer);
begin
  TMapleClient(FClient).Player.ModifyMesos(Value, True, False, True);
end;

procedure TNPCConversation.IncreaseSlotLimit(InvID, RowsToAdd: Byte);
begin
  TMapleClient(FClient).Player.IncreaseSlotLimit(TMapleInventoryType(InvID), RowsToAdd);
end;

function TNPCConversation.NPCID: Integer;
begin
  Result := FNPCID;
end;

procedure TNPCConversation.TrembleEffect(Mode, Delay: Integer);
begin
  TMapleClient(FClient).Write(MaplePacketCreator.TrembleEffect(Mode, Delay));
end;

procedure TNPCConversation.UseItem(ID: Integer);
begin
  TMapleClient(FClient).Player.UseItem(ID);
end;

initialization
  TClassImporter.Register([TNPCConversation]);

end.
