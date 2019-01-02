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

unit AfterLoginHandler;

interface

uses SysUtils, PacketProcessor, MapleClient, MapleStream, MaplePacketCreator, MapleCharacter,
     Generics.Collections, ZDataset, Utils, WinSock, MTRand;

type
  TServerListReqHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TServerStatusReqHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TCharListReqHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TViewAllCharsRequestHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TViewAllCharsConnectHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TViewAllCharsConnectRegisterPICHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TViewAllCharsConnectWithPICHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

implementation

uses Main, Settings, MapleServerHandler, CharOperationHandler;

{ TServerListReqHandler }

class procedure TServerListReqHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  i: Integer;
begin
  for i := 0 to frmSettings.LVWorlds.Items.Count - 1 do
    C.Write(GetServerList(MSH.Worlds[i]));

  C.Write(GetEndOfServerList);
end;

{ TServerStatusReqHandler }

class procedure TServerStatusReqHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  WorldID: Word;
begin
  // On ssHighlyPopulated a warning is displayed, on ssFull you can't enter the world

  WorldID := Packet.ReadShort;
  if frmSettings.LVWorlds.Items[WorldID].Checked then
    C.Write(GetServerStatus(ssNormal))
  else  // unuseable
    C.Write(GetServerStatus(ssFull));
end;

{ TCharListReqHandler }

class procedure TCharListReqHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
begin
  {$IFNDEF EMS}
  Packet.Skip(1);  // wtf? always 2?
  {$ENDIF}
  C.World := MSH.Worlds[Packet.ReadByte];
  C.Channel := C.World.Channels[Packet.ReadByte];

  //Log('Client is connecting to Server %d; Channel %d', [C.World.Index, C.Channel.Index + 1]);

  C.Write(GetCharList(C, C.World.Index));
end;

{ TViewAllCharsRequestHandler }

class procedure TViewAllCharsRequestHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
const Query = 'SELECT * FROM characters WHERE accountid = %d;';
var
  World, Unk, i, j: Integer;
  Worlds, LoadIDs: TList<Integer>;
  Chars, CharsInWorld: TList<TMapleCharacter>;
  Q: TZQuery;
begin
  try
    Q := C.DB.GetQuery;
    Q.SQL.Text := Format(Query, [C.AccID]);
    Q.Open;

    Worlds := TList<Integer>.Create;
    LoadIDs := TList<Integer>.Create;
    // Load all characters into the list
    while not Q.EOF do
    begin
      World := Q.FieldByName('world').AsInteger;
      if not Worlds.Contains(World) then
        Worlds.Add(World);

      LoadIDs.Add(Q.FieldByName('id').AsInteger);
      Q.Next;
    end;

    Q.Close;
    Q.Free;

    Chars := TList<TMapleCharacter>.Create;
    for i := 0 to LoadIDs.Count - 1 do
      Chars.Add(TMapleCharacter.LoadFromDB(LoadIDs[i], C, False));
    LoadIDs.Free;

    Unk := Chars.Count + (3 - Chars.Count mod 3);

    C.Write(ShowAllCharacters(Chars.Count, Unk));
    for i := 0 to Worlds.Count - 1 do
    begin
      CharsInWorld := TList<TMapleCharacter>.Create;
      try
        for j := 0 to Chars.Count - 1 do
          if TMapleCharacter(Chars[j]).World = Worlds[i] then
            CharsInWorld.Add(Chars[j]);
        C.Write(ShowAllCharactersInfo(Worlds[i], CharsInWorld));
      finally
        CharsInWorld.Free;
      end;
    end;

    Worlds.Free;

    for i := 0 to Chars.Count - 1 do
      Chars[i].Free;
    Chars.Free;
  except
    Log('ViewAllChars failed! ' + Exception(ExceptObject).Message);
  end;
end;

{ TViewAllCharsConnectHandler }

class procedure TViewAllCharsConnectHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  CharID: Integer;
begin
  CharID := Packet.ReadInt;
  C.World := MSH.Worlds[Packet.ReadInt];
  C.Channel := C.World.Channels[Rand.RandInt(0, C.World.Channels.Count - 1)];

  CharOperationHandler.DoSelectChar(CharID, C);
end;

{ TViewAllCharsConnectRegisterPICHandler }

class procedure TViewAllCharsConnectRegisterPICHandler.HandlePacket(
  Packet: TMapleStream; C: TMapleClient);
var
  PIC: string;
  CharID: Integer;
begin
  Packet.Skip(1);  // always 1? Status?
  CharID := Packet.ReadInt;
  C.World := MSH.Worlds[Packet.ReadInt];
  C.Channel := C.World.Channels[Rand.RandInt(0, C.World.Channels.Count - 1)];
  // Skip the two Mac strings
  Packet.Skip(Packet.ReadShort);
  Packet.Skip(Packet.ReadShort);

  PIC := Packet.ReadMapleAnsiString;
  if not (Length(PIC) in [6..12]) then
  begin
    Log(C.AccountName + ' = Hacker (Packet editing)');
    C.Disconnect;
    Exit;
  end;

  C.PIC := PIC;
  DoSelectChar(CharID, C);
end;

{ TViewAllCharsConnectWithPICHandler }

class procedure TViewAllCharsConnectWithPICHandler.HandlePacket(
  Packet: TMapleStream; C: TMapleClient);
var
  PIC: string;
  CharID: Integer;
begin
  PIC := Packet.ReadMapleAnsiString;
  CharID := Packet.ReadInt;
  C.World := MSH.Worlds[Packet.ReadInt];
  C.Channel := C.World.Channels[Rand.RandInt(0, C.World.Channels.Count - 1)];

  if C.PIC <> PIC then
    C.Write(PICWrong)
  else
    DoSelectChar(CharID, C);
end;

initialization
  HandlerClasses.Add('ServerListRequest', TServerListReqHandler);
  HandlerClasses.Add('ServerListRerequest', TServerListReqHandler);
  HandlerClasses.Add('ServerStatusRequest', TServerStatusReqHandler);
  HandlerClasses.Add('BackToWorldList', TPacketHandler);
  HandlerClasses.Add('CharListRequest', TCharListReqHandler);
  HandlerClasses.Add('ViewAllCharsRequest', TViewAllCharsRequestHandler);
  HandlerClasses.Add('ViewAllCharsConnect', TViewAllCharsConnectHandler);
  HandlerClasses.Add('ViewAllChars', TPacketHandler);
  HandlerClasses.Add('ViewAllCharsConnectRegisterPIC', TViewAllCharsConnectRegisterPICHandler);
  HandlerClasses.Add('ViewAllCharsConnectWithPIC', TViewAllCharsConnectWithPICHandler);

finalization


end.
