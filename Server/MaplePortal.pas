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

unit MaplePortal;

interface

uses SysUtils, Types, GameLogic;

type
  TMaplePortal = class
  private
    FID: Integer;
    FName: string;
    FPosition: TPoint;
    FToMap: Integer;
    FToName: string;
    FScript: string;
    FOnlyOnce: Boolean;
  public
    constructor Create(PortalID: Integer);

    procedure Enter(Client: TObject);

    property ID: Integer read FID;
    property Name: string read FName write FName;
    property Position: TPoint read FPosition write FPosition;
    property ToMap: Integer read FToMap write FToMap;
    property ToName: string read FToName write FToName;
    property Script: string read FScript write FScript;
    property OnlyOnce: Boolean read FOnlyOnce write FOnlyOnce;
  end;

implementation

uses Main, MapleClient, MapleMap, MaplePacketCreator, ScriptHelper, PortalScript;

{ TMaplePortal }

constructor TMaplePortal.Create(PortalID: Integer);
begin
  FID := PortalID;
end;

procedure TMaplePortal.Enter(Client: TObject);
var
  F: string;
  LoadedMap: TMapleMap;
  OldMap: Integer;
begin
  if not TMapleClient(Client).Player.CanUsePortal(FID) then
  begin
    TMapleClient(Client).Write(PortalBlocked);
    Exit;
  end;

  OldMap := TMapleClient(Client).Player.Map.ID;

  if FToMap <> NO_MAP then
  begin
    LoadedMap := TMapleClient(Client).Channel.MapProvider.LoadMap(FToMap);
    TMapleClient(Client).Player.ChangeMap(LoadedMap, LoadedMap.GetPortal(ToName));
  end
  else
  if FScript <> '' then
  begin
    if FOnlyOnce then
      TMapleClient(Client).Player.UsedPortals.Add(FID);

    F := ExtractFilePath(ParamStr(0)) + 'Scripts\Portal\' + FScript + SCRIPT_EXT;
    if FileExists(F) then
    begin
      Log('[PortalScript] Executing: %s%s', [FScript, SCRIPT_EXT]);

      with TPortalScript.Create(F, Client, FID) do
      begin
        Enter;
        Free;
      end;
    end
    else
      Log('[PortalScript] File not found: %s%s', [FScript, SCRIPT_EXT]);
  end;

  // Map wasn't changed?
  if OldMap = TMapleClient(Client).Player.Map.ID then
    TMapleClient(Client).Write(PortalBlocked);
end;

end.
