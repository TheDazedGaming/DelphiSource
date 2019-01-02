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

unit PlayerChatHandler;

interface

uses SysUtils, MapleClient, Generics.Collections, PacketProcessor, MapleStream,
     MaplePacketCreator;

type
  TPublicChatHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TWhisperHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TGroupChatType = (gcBuddy, gcParty, gcGuild, gcAlliance);
  TGroupChatHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

implementation

uses Main, MapleCharacter;

{ TPublicChatHandler }

class procedure TPublicChatHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Text: string;
  BubbleOnly: Boolean;
begin
  {$IFDEF VERSION88_UP}
  Packet.Skip(4);  // Tickcount
  {$ENDIF}
  Text := Packet.ReadMapleAnsiString;
  BubbleOnly := Packet.ReadByte > 0;

  Log('[%s] %s', [C.Player.Name, Text]);

  // Command processing <3
  if not C.HandleCommand(Text) then
    C.Player.Map.BroadcastMessage(nil, ChatText(C.Player.ID, Text, C.Player.IsGM, BubbleOnly));
end;

{ TWhisperHandler }

class procedure TWhisperHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Mode, Ch: Integer;
  DestName, Text: string;
  Dest: TMapleCharacter;
  OK: Boolean;
begin
  Mode := Packet.ReadByte;
  {$IFDEF VERSION88_UP}
  Packet.Skip(4);  // Tickcount
  {$ENDIF}
  DestName := Packet.ReadMapleAnsiString;

  case Mode of
    5:   // /find command
    begin
      Dest := C.Channel.Player[DestName];
      if (Dest <> nil) and (Ord(C.Player.IsGM) >= Ord(Dest.IsGM)) then
        C.Write(FindReplyWithMap(DestName, Dest.Map.ID))
      else
      begin
        Ch := C.World.Find(DestName);
        if (Dest <> nil) or (Ch = -1) then
          C.Write(WhisperResponse(DestName, False))
        else
        if (Dest = nil) and (Ch > -1) then
          C.Write(FindReply(DestName, Ch))
      end;
    end;

    6:   // Whisper
    begin
      Text := Packet.ReadMapleAnsiString;

      if C.HandleCommand(Text) then
        Exit;

      Dest := C.Channel.Player[DestName];
      if Assigned(Dest) then
      begin
        TMapleClient(Dest.Client).Write(Whisper(C.Player.Name,
          Text, C.Channel.Index));
        OK := True;
      end
      else
      begin
        OK := C.World.IsConnected(DestName);
        if OK then
          C.World.Whisper(C.Player.Name,
            DestName, Text, C.Channel.Index);
      end;

      C.Write(WhisperResponse(DestName, OK));
    end;

    else Log('Unknown Whisper-Packet mode: %d', [Mode]);
  end;
end;

{ TGroupChatHandler }

class procedure TGroupChatHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Mode: TGroupChatType;
  RecipientCount, i: Integer;
  Recipients: TList<Integer>;
  Msg: string;
begin
  Packet.Skip(4);  // Tickcount
  Mode := TGroupChatType(Packet.ReadByte);
  RecipientCount := Packet.ReadByte;
  Recipients := TList<Integer>.Create;
  try
    for i := 0 to RecipientCount - 1 do
      Recipients.Add(Packet.ReadInt);
    Msg := Packet.ReadMapleAnsiString;

    if (C.HandleCommand(Msg)) or ((Length(Msg) > 70) and (not C.IsGM)) then
      Exit;

    case Mode of
      gcBuddy: C.World.BuddyChat(Recipients, C.Player.ID, C.Player.Name, Msg);
      gcParty: if C.Player.Party <> nil then
                 C.World.PartyChat(C.Player.Party.ID, Msg, C.Player.Name);

      else Log('Unsupported Group-Chat mode: %d', [Byte(Mode)]);
    end;
  finally
    Recipients.Free;
  end;
end;

initialization
  HandlerClasses.Add('PublicChat', TPublicChatHandler);
  HandlerClasses.Add('Whisper', TWhisperHandler);
  HandlerClasses.Add('GroupChat', TGroupChatHandler);

end.
