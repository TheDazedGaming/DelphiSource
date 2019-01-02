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

unit QuestActionHandler;

interface

uses SysUtils, PacketProcessor, MapleClient, MapleStream, MaplePacketCreator,
     QuestScript, ScriptHelper;

type
  TQuestAction = (qaGiveItem, qaStart, qaComplete, qaForfeit,
                  qaScriptStart, qaScriptEnd);

  TQuestActionHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

implementation

uses Main, PlayerInventory, MapleQuest;

{ TQuestActionHandler }

class procedure TQuestActionHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Action: TQuestAction;
  Quest: Word;
  NPC, Amount, Item: Integer;
  S: string;

  function Verify: Boolean;
  begin
    case Action of
      qaScriptStart: Result := (not C.Player.Quests.ContainsKey(Quest)) or (TPlayerQuestStatus(C.Player.Quests[Quest]).Status = qsNotStarted);
      qaScriptEnd: Result := C.Player.Quests.ContainsKey(Quest) and (TPlayerQuestStatus(C.Player.Quests[Quest]).Status in [qsStarted, qsAllRequestsDone]);
      else Result := False;
    end;
  end;

begin
  NPC := 0;
  Action := TQuestAction(Packet.ReadByte);
  Quest := Packet.ReadShort;

  Log('[Quest] %d, %d', [Byte(Action), Quest]);

  if Action in [qaStart, qaComplete, qaScriptStart, qaScriptEnd] then
  begin
    NPC := Packet.ReadInt;
    Packet.Skip(4);
  end;

  case Action of
    qaGiveItem:  // called when you "lost" a quest item that was given to you by a NPC
    begin
      Amount := Packet.ReadInt;
      Item := Packet.ReadInt;
      C.Write(ShowItemGain(Item, Amount, True));
      C.Player.GainItemByID(Item, Amount);
    end;
    qaStart: C.Player.StartQuest(Quest, NPC);
    qaComplete: C.Player.FinishQuest(Quest, NPC);
    qaForfeit: C.Player.ForfeitQuest(Quest);
    qaScriptStart, qaScriptEnd:
    begin
      if Verify and (not Assigned(C.Player.CurConversation)) and FindScript(Quest, 'Quest', S) then
      begin
        C.Player.CurConversation := TQuestScriptManager.Create(Quest, NPC, C, S, Action = qaScriptStart);
        if not TQuestScriptManager(C.Player.CurConversation).Run then
        begin
          C.Player.CurConversation.Free;
          C.Player.CurConversation := nil;
        end;
      end;
    end

    else Log('[Quest] WARNING: Unhandled action!!');
  end;
end;

initialization
  HandlerClasses.Add('QuestAction', TQuestActionHandler);

end.
