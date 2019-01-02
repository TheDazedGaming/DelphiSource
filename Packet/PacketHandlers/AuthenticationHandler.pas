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

unit AuthenticationHandler;

interface

uses SysUtils, MapleClient, MapleStream, PacketProcessor, MaplePacketCreator;

type
  TPasswordHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TPinHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TChangePinHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  {$IFDEF EMS}
  TEMSCryptoReqHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;
  {$ENDIF}

implementation

uses Main, Settings;

{ TPasswordHandler }

class procedure TPasswordHandler.HandlePacket(Packet: TMapleStream; C: TMapleClient);
var
  ID, PW: string;
begin
  if Packet.Size < 8 then
    Exit;

  ID := Packet.ReadMapleAnsiString;
  PW := Packet.ReadMapleAnsiString;

  Log('Login: %s', [ID]);

  C.CheckLogin(ID, PW);
end;

{ TPinHandler }

class procedure TPinHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Action, Action2: Byte;
  Pin: Integer;
begin
  Action := Packet.ReadByte;

  //Log('Action: ' + IntToStr(Action));

  if Action in [1, 2] then
  begin
    Action2 := Packet.ReadByte;

    //Log('Action2: ' + IntToStr(Action2));
    case Action2 of
      0:
      begin
        {$IFDEF EMS}
        Packet.Skip(4);    // Account-ID
        {$ENDIF}
        Pin := StrToInt(Packet.ReadMapleAnsiString);

        case Action of
          1: C.CheckPin(Pin);
          2: if C.CheckPin(Pin, False) then
               C.Write(PinOperation(poRegister));
        end;
      end;

      1: if frmSettings.cbEnablePins.Checked then
           C.Write(PinOperation(poEnter))
         else   // Don't show the dialog, just accept
           C.Write(PinOperation(poAccepted));
    end;
  end
  else
    if Action = 0 then    // User pressed Cancel in Pin-Dialog
      C.UpdateLoginState(lsNotLoggedIn);
end;

{ TChangePinHandler }

class procedure TChangePinHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  Action: Byte;
  Pin: string;
begin
  Action := Packet.ReadByte;
  if Action = 1 then
  begin
    Pin := Packet.ReadMapleAnsiString;
    if Length(Pin) = 4 then
      C.Pin := StrToInt(Pin);
  end;
end;

{$IFDEF EMS}
{ TEMSCryptoReqHandler }

class procedure TEMSCryptoReqHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
begin
  C.Write(EMSCrypto);
  C.Write(EMSCryptoEnd);
end;
{$ENDIF}

initialization
  HandlerClasses.Add('Password', TPasswordHandler);
  HandlerClasses.Add('PinOperation', TPinHandler);
  HandlerClasses.Add('ChangePin', TChangePinHandler);

  {$IFDEF EMS}
  HandlerClasses.Add('EMSCryptoReq', TEMSCryptoReqHandler);
  {$ENDIF}

finalization

end.
