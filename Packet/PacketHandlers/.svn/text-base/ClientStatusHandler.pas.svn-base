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

unit ClientStatusHandler;

interface

uses PacketProcessor, MapleClient, MapleStream;

type
  TClientErrorHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TAtLoginScreenHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TAuthErrorHandler = class(TPacketHandler)
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TPacketErrorHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TMapChanged2Handler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

implementation

uses Main, MapleServerHandler;

{ TClientErrorHandler }

class procedure TClientErrorHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
begin                                                          // -2 because of the short for the string length
  Log('Received %d byte long client-error report from %s', [Packet.Size - 2, C.Connection.Binding.PeerIP]);
  //Log(Packet.ReadMapleAnsiString);
  // I don't really want to log the full report here, as it is sent by the v74 client on almost EVERY startup.
end;

{ TAtLoginScreenHandler }

class procedure TAtLoginScreenHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
begin
  // Lol. Uber surveillance - GMS 0.74+ makes it possible.
  Log('%s is at the login screen now.', [C.Connection.Binding.PeerIP]);
  {$IFDEF VERSION88_UP}
  if LOGIN_WORKAROUND then
  begin
    C.AccID := 2;
    if C.GetLoginState > lsNotLoggedIn then
      C.CheckLogin('admin', 'admin')
    else
      C.CheckLogin('adminT', 'admin');
  end;
  {$ENDIF}
end;

{ TPacketErrorHandler }

class procedure TPacketErrorHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
begin
  Packet.Skip(12);
  Log('Client has EOF: ' + Packet.ToStringFromCurPos);
end;

{ TMapChanged2Handler }

class procedure TMapChanged2Handler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  i: Integer;
begin
  i := Packet.ReadInt;
  if i <> 0 then
    Log('[MapChanged2] It actually does something. i = %d', [i]);
end;

{ TAuthErrorHandler }

class procedure TAuthErrorHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
begin
  Packet.ReadByte;
  Log('[Authorization] Error Code %d', [Packet.ReadInt]);
  Log('You didn''t create the auto-login account or didn''t inject an auth-server bypass.');
end;

initialization
  HandlerClasses.Add('ClientError', TClientErrorHandler);
  HandlerClasses.Add('PacketError', TPacketErrorHandler);
  HandlerClasses.Add('AuthError', TAuthErrorHandler);

  HandlerClasses.Add('AtLoginScreen', TAtLoginScreenHandler);

  HandlerClasses.Add('MapChanged2', TMapChanged2Handler);
  HandlerClasses.Add('ServerChanged', TPacketHandler);

finalization

end.
