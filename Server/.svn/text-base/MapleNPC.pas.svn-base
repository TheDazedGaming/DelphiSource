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

unit MapleNPC;

interface

uses SysUtils, MapleMapObject, MapleStream;

type
  TMapleNPC = class(TLoadedLife)
  private
    FName: string;

    procedure AddSpawnData(var Packet: TMapleStream);
  public
    constructor Create(ID: Integer; Name: string); reintroduce;

    function GetType: TObjectType; override;
    function SendSpawnDataTo(Client: TObject): Boolean; override;
    procedure SendDestroyDataTo(Client: TObject); override;

    function GetSpawnPacket: TMapleStream;
    function GetControlPacket: TMapleStream;
    function GetRemovePacket: TMapleStream;
    function GetSpecialAnimation(Name: string): TMapleStream;
    class function GetTalkPacket(ID: Integer; MsgType: Byte; Text: string;
      EndBytes: array of const; Speaker: Byte = 0; OtherNPC: Integer = 0): TMapleStream;
    function GetStopControlling: TMapleStream;

    property Name: string read FName;
  end;

implementation

uses MapleClient, MaplePacketCreator, MapleServerHandler;

{ TMapleNPC }

constructor TMapleNPC.Create(ID: Integer; Name: string);
begin
  inherited Create(ID);
  FName := Name;
end;

procedure TMapleNPC.AddSpawnData(var Packet: TMapleStream);
begin
  with Packet do
  begin
    WriteInt(ObjectID);
    WriteInt(ID);
    WritePos(Self.Position);
    WriteBool(FacesRight);
    WriteShort(Foothold);
    WriteShort(MinClickPos);
    WriteShort(MaxClickPos);
  end;
end;

function TMapleNPC.GetControlPacket: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ControlNPC']);

    WriteBool(True);
    AddSpawnData(Result);
    WriteBool(True);
  end;
end;

function TMapleNPC.GetRemovePacket: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['RemoveNPC']);

    WriteInt(ObjectID);
  end;
end;

function TMapleNPC.GetSpawnPacket: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['SpawnNPC']);

    AddSpawnData(Result);
    WriteBool(True);
  end;
end;

function TMapleNPC.GetSpecialAnimation(Name: string): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['SpecialNPCAnimation']);

    WriteInt(ObjectID);
    WriteMapleAnsiString(Name);    // is found in Npc.wz
  end;
end;

function TMapleNPC.GetStopControlling: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['ControlNPC']);

    WriteBool(False);
    WriteInt(ObjectID);
  end;
end;

class function TMapleNPC.GetTalkPacket(ID: Integer; MsgType: Byte; Text: string;
  EndBytes: array of const; Speaker: Byte = 0; OtherNPC: Integer = 0): TMapleStream;
var
  i: Integer;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['NPCTalk']);

    WriteByte(4);
    WriteInt(ID);
    {$IFDEF VERSION89_UP}
    if MsgType > 0 then
      Inc(MsgType);   // in v84+ all MsgTypes after 0 were increased by one
    {$ENDIF}
    WriteByte(MsgType);
    WriteByte(Speaker);

    if Speaker in [4, 5] then
      WriteInt(OtherNPC);

    if MsgType = 15 then
      {$IFNDEF VERSION89_UP}
      WriteInt(0);  // 0 = Dimensional Mirror (Mu Lung Temple, Face of Carnivalian, ..) 1 = Neo City, 2+ = EOF
      {$ELSE}
      WriteInt64(0);
      {$ENDIF}

    // Fucks things up if it stays \r\n, so parse that
    Text := StringReplace(Text, '\r', #13, [rfReplaceAll]);
    Text := StringReplace(Text, '\n', #10, [rfReplaceAll]);
    WriteMapleAnsiString(Text);

    for i := 0 to High(EndBytes) do
      WriteByte(EndBytes[i].VInteger);
  end;
end;

function TMapleNPC.GetType: TObjectType;
begin
  Result := otNPC;
end;

function TMapleNPC.SendSpawnDataTo(Client: TObject): Boolean;
begin
  TMapleClient(Client).Write(GetSpawnPacket);
  TMapleClient(Client).Write(GetControlPacket);

  Result := True;
end;

procedure TMapleNPC.SendDestroyDataTo(Client: TObject);
begin
  TMapleClient(Client).Write(GetRemovePacket);
end;

end.
