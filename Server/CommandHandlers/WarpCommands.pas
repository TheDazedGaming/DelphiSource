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

unit WarpCommands;

interface

uses SysUtils, CommandProcessor, MapleClient, Utils;

implementation

uses Main, MapleMap;

procedure Warp(Cmd: string; C: TMapleClient);
var
  MapID, Portal: Integer;
  Target: TMapleMap;
  Parameters: TArrayofstring;
begin
  Parameters := Explode(' ', Cmd);
  if not TryStrToInt(Parameters[0], MapID) then
  begin
    Log('[Warping] MapID parsing failed for %s', [Cmd]);
    Exit;
  end;

  Portal := 0;
  if Length(Parameters) > 1 then
    try
      Portal := StrToInt(Parameters[1]);
    except
      Log('[Warping] Syntax Error @ Portal Parameter');
      Portal := 0;
    end;

  Parameters := nil;

  Target := C.Channel.MapProvider.LoadMap(MapID);

  if Target <> nil then
  begin
    Log('[Warping] Target: ' + IntToStr(Target.ID));
    C.Player.ChangeMap(Target, Target.GetPortal(Portal));
  end
  else
    Log('[Warping] Map %d does not exist!', [MapID]);
end;

initialization
  CmdHandlers.Add('warp', Warp);
  CmdHandlers.Add('map', Warp);

end.
