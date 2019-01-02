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

unit Trade;

interface

uses Generics.Collections, MapleCharacter, MapleClient, MapleItem,
     MapleStream, MapleServerHandler, GameLogic, ItemDataProvider, PlayerInventory;

type
  TInteractionError = (ieOccupied = 2, ieDeclined);
  TInteractionType = (itTrade = 3);

  TTrade = class
  private
    FChar: TMapleCharacter;
    FOpened, FConfirmed: Boolean;
    FPartner: TTrade;

    FItems: TDictionary<Byte, TItem>;
    FMesos: Integer;

    procedure Init;

    // Packets
    function AddPartner: TMapleStream;
    function Close: TMapleStream;
    function ErrorMessage(Err: TInteractionError; const CharName: string): TMapleStream;
    function GetChat(const Msg: string; IsSelf: Boolean): TMapleStream;
    function GetInvite: TMapleStream;
    function GetItem(Index: Byte; Item: TItem): TMapleStream;
    function GetMesos(Index: Byte; Amount: Integer): TMapleStream;
    function RoomClosedError: TMapleStream;
    function Start: TMapleStream;
    procedure WriteChar(var Packet: TMapleStream; Index: Byte; MC: TMapleCharacter);
  public
    constructor Create(AChar: TMapleCharacter); overload;
    constructor Create(AChar: TMapleCharacter; APartner: TTrade); overload;
    destructor Destroy; override;

    procedure Accept;
    procedure AddItem(Inventory, Position, Count, TradeSlot: SmallInt);
    procedure AddMesos(Amount: Integer);
    procedure Cancel;
    procedure Chat(const Msg: string);
    procedure Confirm;
    procedure Decline;
    procedure Invite(APartner: TMapleCharacter);

    property Char: TMapleCharacter read FChar;
    property Opened: Boolean read FOpened;
    property Partner: TTrade read FPartner;

    property Items: TDictionary<Byte, TItem> read FItems;
    property Mesos: Integer read FMesos;
  end;

implementation

uses Main, MaplePacketCreator;

{ TTrade }

constructor TTrade.Create(AChar: TMapleCharacter);
begin
  FChar := AChar;
  FOpened := True;
  TMapleClient(FChar.Client).Write(Start);
  Init;
end;

constructor TTrade.Create(AChar: TMapleCharacter; APartner: TTrade);
begin
  FChar := AChar;
  FOpened := False;
  FPartner := APartner;
  TMapleClient(FChar.Client).Write(GetInvite);
  Init;
end;

destructor TTrade.Destroy;
begin
  Log('[%s] Destructing Trade', [FChar.Name]);
  FItems.Free;

  inherited;
end;

procedure TTrade.Init;
begin
  FConfirmed := False;
  FItems := TDictionary<Byte, TItem>.Create;
  FMesos := 0;
end;

procedure TTrade.Accept;
begin
  if (FPartner.Char.Trade = nil) or (TTrade(FPartner.Char.Trade).Partner <> Self) then
  begin
    TMapleClient(FChar.Client).Write(RoomClosedError);
    Cancel;
    Exit;
  end;

  FOpened := True;
  TMapleClient(FChar.Client).Write(Start);
  TMapleClient(FPartner.Char.Client).Write(AddPartner);
end;

procedure TTrade.AddItem(Inventory, Position, Count, TradeSlot: SmallInt);
var
  InvType: TMapleInventoryType;
  Item, ItemCopy: TItem;
begin
  InvType := TMapleInventoryType(Inventory);
  Item := FChar.Inventory[InvType][Position];

  if FConfirmed or (not Assigned(FPartner)) or (not FPartner.Opened) or
     (not Assigned(Item)) or FItems.ContainsKey(TradeSlot) {or not ItemDataProv.IsTradable(Item)} then
    Exit;

  if IsRechargeable(Item.ID) then
    Count := Item.Quantity
  else if (Count < 1) or (Count > SmallInt(Item.Quantity)) then
    Exit;

  ItemCopy := Item.Copy;
  ItemCopy.Position := TradeSlot;
  ItemCopy.Quantity := Count;
  FChar.RemoveItemFromSlot(InvType, Position, Count, True);
  FItems.Add(TradeSlot, ItemCopy);

  TMapleClient(FChar.Client).Write(GetItem(0, ItemCopy));
  TMapleClient(FPartner.Char.Client).Write(GetItem(1, ItemCopy));
end;

procedure TTrade.AddMesos(Amount: Integer);
begin
  if FConfirmed or (not Assigned(FPartner)) or (not FPartner.Opened) or
     (Amount < 0) or (Amount > FChar.Mesos) or
     ((FChar.Level <= 15) and (FMesos + Amount > 1000000)) or
     (UInt32(FMesos) + UInt32(Amount) > UInt32(MAXINT)) then   // signed int > MAXINT would be negative
    Exit;

  FChar.ModifyMesos(-Amount, False);
  Inc(FMesos, Amount);

  TMapleClient(FChar.Client).Write(GetMesos(0, Amount));
  TMapleClient(FPartner.Char.Client).Write(GetMesos(1, Amount));
end;

procedure TTrade.Cancel;
var
  I: TItem;
begin
  for I in FItems.Values do
    FChar.AddItem(I, False);

  FChar.ModifyMesos(FMesos, False);
  FChar.Trade := nil;

  if FOpened then
    TMapleClient(FChar.Client).Write(Close);

  if Assigned(FPartner.Char.Trade) and (TTrade(FPartner.Char.Trade).Partner = Self) then
    TTrade(FPartner.Char.Trade).Cancel;

  Free;
end;

procedure TTrade.Chat(const Msg: string);
begin
  TMapleClient(FChar.Client).Write(GetChat(Msg, True));
  TMapleClient(FPartner.Char.Client).Write(GetChat(Msg, False));
end;

procedure TTrade.Confirm;
begin

end;

procedure TTrade.Decline;
begin
  if (FPartner.Char.Trade = nil) or (TTrade(FPartner.Char.Trade).Partner <> Self) then
    Exit;

  TMapleClient(FPartner.Char.Client).Write(ErrorMessage(ieDeclined, FChar.Name));
  Cancel;
end;

procedure TTrade.Invite(APartner: TMapleCharacter);
begin
  if FPartner <> nil then
    Exit;

  if APartner.IsOccupied then
  begin
    TMapleClient(FChar.Client).Write(ErrorMessage(ieOccupied, APartner.Name));
    Cancel;
    Exit;
  end;

  FPartner := TTrade.Create(APartner, Self);
  APartner.Trade := FPartner;
end;

// Packets

function TTrade.AddPartner: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PlayerInteraction']);
    WriteByte(4);  // Opcode mode

    WriteChar(Result, 1, FChar);
  end;
end;

function TTrade.Close: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PlayerInteraction']);
    WriteByte(10);  // Opcode mode: Leave

    WriteByte(0);  // Index
    WriteByte(2);  // LeaveType
  end;
end;

function TTrade.ErrorMessage(Err: TInteractionError; const CharName: string): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PlayerInteraction']);
    WriteByte(3);  // Opcode mode

    WriteByte(Err);
    WriteMapleAnsiString(CharName);
  end;
end;

function TTrade.GetChat(const Msg: string; IsSelf: Boolean): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PlayerInteraction']);
    WriteByte(6);  // Opcode mode: Chat

    WriteByte(8);
    WriteBool(not IsSelf);  // Blue or black text
    WriteMapleAnsiString(FChar.Name + ' : ' + Msg);
  end;
end;

function TTrade.GetInvite: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PlayerInteraction']);
    WriteByte(2);  // Opcode mode: Invite
    WriteByte(itTrade);

    WriteMapleAnsiString(FPartner.Char.Name);
    WriteInt(1);  // ID - don't need that
  end;
end;

function TTrade.GetItem(Index: Byte; Item: TItem): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PlayerInteraction']);
    WriteByte(15);  // Opcode mode: Item

    WriteByte(Index);
    WriteByte(Item.Position);
    AddItemInfo(Result, Item, False);
  end;
end;

function TTrade.GetMesos(Index: Byte; Amount: Integer): TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PlayerInteraction']);
    WriteByte(16);  // Opcode mode: Mesos

    WriteByte(Index);
    WriteInt(Amount);
  end;
end;

function TTrade.RoomClosedError: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PlayerInteraction']);
    WriteByte(5);  // Opcode mode
    WriteByte(0);
    WriteByte(1);
  end;
end;

function TTrade.Start: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(OpHandler.SendOps['PlayerInteraction']);
    WriteByte(5);  // Opcode mode: Create
    WriteByte(itTrade);

    WriteByte(2);  // Max-Users
    WriteBool(FPartner <> nil);

    if FPartner <> nil then
    begin
      WriteChar(Result, 0, FPartner.Char);
      WriteChar(Result, 1, FChar);
    end
    else
      WriteChar(Result, 0, FChar);

    WriteByte(-1);
  end;
end;

procedure TTrade.WriteChar(var Packet: TMapleStream; Index: Byte; MC: TMapleCharacter);
begin
  Packet.WriteByte(Index);
  AddCharLook(Packet, MC, False);
  Packet.WriteMapleAnsiString(MC.Name);
  Packet.WriteShort(Word(MC.Job));
end;

end.
