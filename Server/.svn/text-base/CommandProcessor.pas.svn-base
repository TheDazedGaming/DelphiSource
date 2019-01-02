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

unit CommandProcessor;

interface

uses SysUtils, Generics.Collections, MapleClient;

type
  TCommandHandler = procedure(Cmd: string; C: TMapleClient);

  TCommandProcessor = class
  private
    FClient: TMapleClient;
  public
    constructor Create(Client: TMapleClient);

    function HandleCommand(Cmd: string): Boolean;
  end;

var
  CmdHandlers: TDictionary<string, TCommandHandler>;

implementation

uses Main;

{ TCommandProcessor }

constructor TCommandProcessor.Create(Client: TMapleClient);
begin
  FClient := Client;
end;

function TCommandProcessor.HandleCommand(Cmd: string): Boolean;
var
  Space: Integer;
  CmdName: string;
begin
  if (Length(Cmd) < 2) or (Cmd[1] <> '/') then
    Exit(False);

  Space := Pos(' ', Cmd);
  if Space > 0 then
    CmdName := LowerCase(Copy(Cmd, 2, Space - 2))
  else
    CmdName := LowerCase(Copy(Cmd, 2, Length(Cmd) - 1));

  if not CmdHandlers.ContainsKey(CmdName) then
    Exit(False);

  // Now let the handler for the command process it.
  CmdHandlers[CmdName](Copy(Cmd, Space + 1, Length(Cmd)), FClient);

  Result := True;
end;

initialization
  CmdHandlers := TDictionary<string, TCommandHandler>.Create;

finalization
  FreeAndNil(CmdHandlers);

end.
