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

unit PacketProcessor;

interface

uses Windows, Classes, SysUtils, Generics.Collections, MapleClient, MapleCrypt,
     MapleStream, MaplePacketCreator, IdIOHandlerStack, IdServerIOHandlerStack,
     IdStream, Math;

type
  TPacketHandler = class
  public
    // it's static, so we don't need to construct the class before using it
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); virtual; abstract;
  end;

  THandlerClass = class of TPacketHandler;

  TPacketProcessor = class
  private
    FMapleClient: TMapleClient;
  public
    constructor Create(Client: TMapleClient);
    destructor Destroy; override;

    procedure ProcessPacket(var Data: TMemoryStream);
  end;

  TMapleIOHandler = class(TIdIOHandlerStack)
  protected
    FReadCrypto, FWriteCrypto: TMapleDecoder;
    FCache: TBytes;

    function ReadDataFromSource(var VBuffer: TBytes): Integer; override;
    function WriteDataToTarget(const ABuffer: TBytes; const AOffset, ALength: Integer): Integer; override;

    procedure Cache(Data: TBytes; Len: Integer);
  public
    procedure Open; override;
    procedure Close; override;

    procedure WriteUnencrypted(Data: TMemoryStream);
  end;

  TMapleServerIOHandler = class(TIdServerIOHandlerStack)
  protected
    procedure InitComponent; override;
  end;

var
  HandlerClasses: TDictionary<string, THandlerClass>;

implementation

uses Main, MapleServerHandler;

{ TPacketProcessor }

constructor TPacketProcessor.Create(Client: TMapleClient);
begin
  FMapleClient := Client;
end;

destructor TPacketProcessor.Destroy;
begin
  inherited;
end;

procedure TPacketProcessor.ProcessPacket(var Data: TMemoryStream);
var
  MS: TMapleStream;
  Opcode, PacketLength: SmallInt;
  BC: Integer;
begin
  {if FBufferedPacket <> nil then
  begin
    // Copy the newly arrived content to the end of the existing
    FBufferedPacket.CopyFrom(Data, 0);
    Data.Free;
    Data := FBufferedPacket;
    FBufferedPacket := nil;    // So it doesn't think we have something stored
  end;

  if (Data.Size - Data.Position) < PacketLength then
  begin
    (* Sometimes the incoming data is not complete - you would d/c when jumping
       down in HHG1 very fast (many packets are sent). So save it up and
       merge it with the next packet to not run in such trouble.     *)
 //   if Data.Size = 1460 then
 //   begin
      FBufferedPacket := TMemoryStream.Create;
      FBufferedPacket.CopyFrom(Data, 0);

      Exit(True);
  //  end;

   // Log('[ !! FATAL !! ] Not enough data to decode!!');
 //   Exit(False);
  end;

   }

  Data.Position := 0;
  while Data.Position < Data.Size do
  begin
    Data.Read(PacketLength, 2);
    Data.Read(Opcode, 2);
    BC := Data.Position;  // save old position; will be changed by CopyFrom()
    MS := TMapleStream.Create;
    if PacketLength - 2 > 0 then
      MS.CopyFrom(Data, PacketLength - 2);
    MS.Position := 0;
    Data.Position := BC + PacketLength - 2;

    if (OpHandler.ReceiveOps.ContainsKey(Opcode)) and (HandlerClasses.ContainsKey(OpHandler.ReceiveOps[Opcode])) then
    begin
      //Log('Found handler for packet: ' + HandlerClasses[OpHandler.ReceiveOps[Opcode]].ClassName);
      //Log(MS.ToString);

      // Silly IDE... it underlines HandlePacket red, although the code is valid.
      if HandlerClasses[OpHandler.ReceiveOps[Opcode]] <> TPacketHandler then
        HandlerClasses[OpHandler.ReceiveOps[Opcode]].HandlePacket(MS, FMapleClient);
    end
    else
    begin                                      // decimal, hex
      Log('[WARNING] Could NOT handle packet! Opcode: %d [$%:1.2x]', [Opcode]);
      Log(MS.ToString);
    end;

    MS.Free;
  end;

  Data.Free;
end;

{ TMapleIOHandler }

procedure TMapleIOHandler.Open;
begin
  inherited;

  // xxx Make IV random
  FReadCrypto := TMapleDecoder.Create(IV_RECV, MAPLE_VERSION);
  FWriteCrypto := TMapleDecoder.Create(IV_SEND, $FFFF - MAPLE_VERSION);
  FCache := nil;
end;

procedure TMapleIOHandler.Close;
begin
  if FReadCrypto <> nil then
    FreeAndNil(FReadCrypto);
  if FWriteCrypto <> nil then
    FreeAndNil(FWriteCrypto);

  inherited;
end;

procedure TMapleIOHandler.Cache(Data: TBytes; Len: Integer);
var
  L1: Integer;
begin
  L1 := Length(FCache);
  SetLength(FCache, Length(FCache) + Len);
  Move(Data[0], FCache[L1], Len);
end;

function TMapleIOHandler.ReadDataFromSource(var VBuffer: TBytes): Integer;
var
  Len, RealLen, LenBefore: Integer;
  Buf, Res: TBytes;
begin
  Len := inherited;  // this also puts the data into VBuffer

  if FCache <> nil then
  begin
    // Make room for cache in array
    Move(VBuffer[0], VBuffer[Length(FCache)], Length(VBuffer) - Length(FCache));
    // Copy cache before new data
    Move(FCache[0], VBuffer[0], Length(FCache));
    Inc(Len, Length(FCache));
    FCache := nil;
  end;

  if (Len = 0) or (not FReadCrypto.CheckPacket(VBuffer)) then
  begin
    if Len > 0 then
      Log('Packet check failed, disconnecting! [%s]', [Binding.PeerIP]);
    Exit(0);
  end;

  Res := nil;
  // More than 1 packet can be received with one call, so handle them all
  while Len > 0 do
  begin
    RealLen := TMapleDecoder.GetPacketLength(VBuffer);
    if Len - 4 < RealLen then
    begin
      Cache(VBuffer, Len);
      Break;
    end;

    // Decrypt incoming data and add it to Res
    Buf := Copy(VBuffer, 4, RealLen);   // packet data without the 4 byte header
    FReadCrypto.DecryptPacket(Buf);
    LenBefore := Length(Res);  // start at LenBefore so it doesn't overwrite data that was already decrypted
    SetLength(Res, Length(Res) + RealLen + 2);
    Move(Buf[0], Res[LenBefore + 2], RealLen);
    Buf := nil;
    // Write Length as a short so we can split it up again
    Res[LenBefore] := Byte(RealLen);
    Res[LenBefore + 1] := RealLen shr 8;

    // Remove first packet from array
    Move(VBuffer[RealLen + 4], VBuffer[0], Length(VBuffer) - (RealLen + 4));
    SetLength(VBuffer, Length(VBuffer) - (RealLen + 4));
    Dec(Len, RealLen + 4);  // packet with it's 4 byte header was removed
  end;

  VBuffer := nil;
  VBuffer := Res;
  Result := Length(VBuffer);
  Res := nil;
end;

function TMapleIOHandler.WriteDataToTarget(const ABuffer: TBytes; const AOffset,
  ALength: Integer): Integer;
var
  VBuf: TBytes;
begin
  VBuf := Copy(ABuffer, AOffset, ALength);
  FWriteCrypto.EncryptPacket(VBuf);
  Result := inherited WriteDataToTarget(VBuf, 0, Length(VBuf));
  VBuf := nil;
end;

procedure TMapleIOHandler.WriteUnencrypted(Data: TMemoryStream);
var
  Size, LBufSize: Integer;
  LBuffer: TBytes;
begin
  Data.Position := 0;
  Size := Data.Size;
  try
    while Size > 0 do
    begin
      SetLength(LBuffer, FSendBufferSize);
      LBufSize := Min(Size, FSendBufferSize);
      LBufSize := TIdStreamHelper.ReadBytes(Data, LBuffer, LBufSize);
      SetLength(LBuffer, LBufSize);
      inherited WriteDataToTarget(LBuffer, 0, LBufSize);
      Dec(Size, LBufSize);
    end;
  finally
    LBuffer := nil;
  end;
end;

{ TMapleServerIOHandler }

procedure TMapleServerIOHandler.InitComponent;
begin
  inherited;
  // Tell the server to use our MapleIOHandler for new connections
  IOHandlerSocketClass := TMapleIOHandler;
end;

initialization
  HandlerClasses := TDictionary<string, THandlerClass>.Create;

  // Empty handlers
  HandlerClasses.Add('AttackOnMount', TPacketHandler);  // Extra packet with Skill ID & facing direction, not useful
  HandlerClasses.Add('ExtraTickCount', TPacketHandler);  // Tick counts, tick counts everywhere -__-
  HandlerClasses.Add('UIAction', TPacketHandler);

finalization
  FreeAndNil(HandlerClasses);

end.
