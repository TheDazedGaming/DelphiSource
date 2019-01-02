(*
    This file is part of the Delphi MapleStory Server

    	Copyright (C) 2009-2010 Hendi

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

unit MapCommands;

interface

uses SysUtils, CommandProcessor, MapleClient, Utils;

implementation

uses Main, MapleMonster;

{ /summon }

procedure Summon(Cmd: string; C: TMapleClient);
var
  Params: TArrayofstring;
  Mob, Q, Effect, i: Integer;
begin
  Params := Explode(' ', Cmd);
  case Length(Params) of
    2:
    begin
      Q := StrToInt(Params[1]);
      Effect := 0;
    end;
    3:
    begin
      Q := StrToInt(Params[1]);
      Effect := StrToInt(Params[2]);
    end;

    else
    begin
      Q := 1;
      Effect := 0;
    end;
  end;

  Mob := StrToInt(Params[0]);
  for i := 1 to Q do
    C.Player.Map.SpawnMonster(Mob, C.Player.Position, 0, Effect);
  Params := nil;
end;

initialization
  CmdHandlers.Add('summon', Summon);

end.
