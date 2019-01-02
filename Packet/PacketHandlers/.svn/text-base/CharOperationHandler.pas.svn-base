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

unit CharOperationHandler;

interface

uses SysUtils, PacketProcessor, MapleClient, MapleStream, MaplePacketCreator,
     MapleCharacter, MapleItem, MapleInventory, WinSock, ItemDataProvider, Utils,
     GameLogic, DateUtils, TypInfo, SkillDataProvider, QuestDataProvider;

type
  TNameCheckHandler = class(TPacketHandler)
  public
   class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TCreateCharHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TDeleteCharHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TSelectCharHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TSelectCharRegisterPICHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

  TSelectCharWithPICHandler = class(TPacketHandler)
  public
    class procedure HandlePacket(Packet: TMapleStream; C: TMapleClient); override;
  end;

procedure DoSelectChar(const CharID: Integer; C: TMapleClient);

implementation

uses Main, Settings, Skills, MapleQuest;

{ TNameCheckHandler }

class procedure TNameCheckHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  CharName: string;
begin
  CharName := Packet.ReadMapleAnsiString;
  Log('Character name check: ' + CharName);

  // check if Character already is in DB
  C.Write(NameCheckResponse(CharName, CharIDByName(CharName, C.DB) > -1));
end;

{ TCreateCharHandler }

class procedure TCreateCharHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  NewChar: TMapleCharacter;
  CharOK: Boolean;
  iHair, iHairColor: Integer;
  Eq: TEquip;
  Top, Bottom, Shoes, Weapon: Integer;

  function CheckAdvCygnus: Boolean;
  begin
    if NewChar.Gender = 0 then
    begin
      if ((NewChar.Face <> 20000) and (NewChar.Face <> 20001) and (NewChar.Face <> 20002)) or
         ((iHair <> 30000) and (iHair <> 30020) and (iHair <> 30030)) or
         ((Top  <> 1040002) and (Top <> 1040006) and (Top <> 1040010)) or
         ((Bottom <> 1060002) and (Bottom <> 1060006)) then
      begin
        Log('Wrong 1.0: Face: %d; iHair: %d; Top: %d; Bottom: %d', [NewChar.Face, iHair, Top, Bottom]);
        Exit(False);
      end;
    end
    else
    if NewChar.Gender = 1 then
    begin
      if ((NewChar.Face <> 21000) and (NewChar.Face <> 21001) and (NewChar.Face <> 21002)) or
         ((iHair <> 31000) and (iHair <> 31040) and (iHair <> 31050)) or
         ((Top <> 1041002) and (Top <> 1041006) and (Top <> 1041010) and (Top <> 1041011)) or
         ((Bottom <> 1061002) and (Bottom <> 1061008)) then
      begin
        Log('Wrong 1.1: Face: %d; iHair: %d; Top: %d; Bottom: %d', [NewChar.Face, iHair, Top, Bottom]);
        Exit(False);
      end;
    end
    else
      Exit(False);  // gender = both lol

    if ((Weapon <> 1302000) and (Weapon <> 1322005) and (Weapon <> 1312004)) or
       ((Shoes <> 1072001) and (Shoes <> 1072005) and (Shoes <> 1072037) and (Shoes <> 1072038)) then
    begin
      Log('Wrong 2: Weapon: %d; Shoes: %d', [Weapon, Shoes]);
      Exit(False);
    end;

    Result := True;
  end;

  function CheckAran: Boolean;
  begin
    if NewChar.Gender = 0 then
      Result := (iHair = 30000) or (iHair = 30020) or (iHair = 30030)
    else
      Result := (iHair = 31000) or (iHair = 31040) or (iHair = 31050);

    if Result then
      Result := (Top = 1042167) and (Bottom = 1062115) and
                (Weapon = 1442079) and (Shoes = 1072383);

    if Result then
      if NewChar.Gender = 0 then
        Result := (NewChar.Face = 20100) or (NewChar.Face = 20401) or (NewChar.Face = 20402)
      else
        Result := (NewChar.Face = 21700) or (NewChar.Face = 21201) or (NewChar.Face = 21002);

    if not Result then
      Log('Aran failure: Hair: %d; Face: %d; Top: %d; Bottom: %d; Weapon: %d; Shoes: %d',
          [iHair, NewChar.Face, Top, Bottom, Weapon, Shoes]);
  end;

begin
  Log(Packet.ToString);
  NewChar := TMapleCharacter.CreateDefault(C);
  with NewChar do
  begin
    Name := Packet.ReadMapleAnsiString;
    CharType := TCharacterType(Packet.ReadInt);
    Log('CharType: %s', [GetEnumName(TypeInfo(TCharacterType), Ord(CharType))]);
    {$IFDEF VERSION88_UP}
    Packet.Skip(2);  // 1 = Dual Blade
    {$ENDIF}
    Face := Packet.ReadInt;     // Aran = 20100
    iHair := Packet.ReadInt;
    iHairColor := Packet.ReadInt;
    Hair := iHair + iHairColor;
    SkinColor := TMapleSkinColor(Packet.ReadInt);

    {$IFDEF BIGBANG}
    if CharType <> ctResistance then
    begin
    {$ENDIF}
    // Top
    Top := Packet.ReadInt;      // Aran = 1042167
    Eq := ItemDataProv.LoadEquip(Top);
    Eq.Position := -5;
    Inventory[miEquipped].AddFromDB(Eq);
    // Bottom
    Bottom := Packet.ReadInt;   // Aran = 1062115
    Eq := ItemDataProv.LoadEquip(Bottom);
    Eq.Position := -6;
    Inventory[miEquipped].AddFromDB(Eq);
    {$IFDEF BIGBANG}
    end
    else     // Resistance has overalls
    begin
      Top := Packet.ReadInt;
      Eq := ItemDataProv.LoadEquip(Top);
      Eq.Position := -5;
      Inventory[miEquipped].AddFromDB(Eq);
      Packet.Skip(4);
    end;
    {$ENDIF}
    // Shoes
    Shoes := Packet.ReadInt;    // Aran = 1072383
    Eq := ItemDataProv.LoadEquip(Shoes);
    Eq.Position := -7;
    Inventory[miEquipped].AddFromDB(Eq);
    // Weapon
    Weapon := Packet.ReadInt;   // Aran = 1442079 (Basic Polearm)
    Eq := ItemDataProv.LoadEquip(Weapon);
    Eq.Position := -11;
    Inventory[miEquipped].AddFromDB(Eq);

    // Add beginner books to Etc & Blessing of the Fairy skill
    case CharType of
      ctCygnusKnight:
      begin
        Inventory[miEtc].AddFromDB(MapleItem.TItem.Create(4161047, 1, 1));
        ChangeSkillLevel(SkillDataProv.GetPlayerSkill(Noblesse.BlessingOfTheFairy), 0, False);
        // Special quest to activate the other intro quests
        Quests.Add(20022, TPlayerQuestStatus.Create(nil, qsStarted, 20022));
        TPlayerQuestStatus(Quests[20022]).Data := '1';
      end;
      ctAdventurer:
      begin
        Inventory[miEtc].AddFromDB(MapleItem.TItem.Create(4161001, 1, 1));
        ChangeSkillLevel(SkillDataProv.GetPlayerSkill(Beginner.BlessingOfTheFairy), 0, False);
      end;
      ctAran:
      begin
        Inventory[miEtc].AddFromDB(MapleItem.TItem.Create(4161048, 1, 1));
        ChangeSkillLevel(SkillDataProv.GetPlayerSkill(Legend.BlessingOfTheFairy), 0, False);
      end;
    end;

    {$IFNDEF EMS}
    Gender := Packet.ReadByte;    // at least they kept the gender in GMS 0.74+
    {$ELSE}
    Gender := 0;
    {$ENDIF}
    STR := 12;
    DEX := 5;
    INT := 4;
    LUK := 4;

    {$IFNDEF BIGBANG}
    if CharType = ctAran then
      CharOK := CheckAran
    else
      CharOK := CheckAdvCygnus;
    {$ELSE}  // Lots of new Hairs/Faces in v93. Can't be bothered to check them.
    CharOK := True;
    {$ENDIF}

    // Common checks if still OK
    if CharOK then
    begin
      if not Integer(NewChar.SkinColor) in [0..3] then
      begin
        Log('Wrong 3: %d', [Integer(NewChar.SkinColor)]);
        CharOK := False;
      end;

      if not iHairColor in [0, 2, 3, 7] then
      begin
        Log('Wrong 4: iHairColor: %d', [iHairColor]);
        CharOK := False;
      end;
    end;

    if not CharOK then
    begin
      Log('Received malformed character-create packet - CharName: ' + Name);
      FreeAndNil(NewChar);
      Exit;
    end;
  end;

  case NewChar.CharType of
    ctCygnusKnight: NewChar.Job := mjNoblesse;
    ctAran: NewChar.Job := mjLegend;
    ctEvan: NewChar.Job := mjLegendEvan;
    {$IFDEF BIGBANG}
    ctResistance: NewChar.Job := mjCitizen;
    {$ENDIF}
  end;

  NewChar.SaveToDB(False);

  Log(NewChar.ToString);

  C.Write(AddNewCharEntry(NewChar, not CharOK));

  NewChar.Free;    // only used as a storage for information, will be loaded from DB again
end;

{ TDeleteCharHandler }

class procedure TDeleteCharHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
{$IFNDEF VERSION82_UP}
var
  Date, CharID: Integer;
  Year, Month, Day: Word;
  DT: TDateTime;
begin
  Date := Packet.ReadInt;
  CharID := Packet.ReadInt;

  Year := Date div 10000;
  Month := (Date - Year * 10000) div 100;
  Day := Date - Year * 10000 - Month * 100;

  // If it is completely wrong don't even check (that would raise an exception)
  // People that are not born yet can't delete characters :p
  if (Year < 1900) or (Year > YearOf(Now)) or (not Month in [1..12]) or (not Day in [1..31]) then
  begin
    C.Write(DeleteCharResponse(CharID, dsDateWrong));
    Exit;
  end;

  DT := EncodeDate(Year, Month, Day);
  Log('Delete request [%d] - %s', [CharID, DateToStr(DT)]);

  C.CheckBirthdate(DT, CharID); // This will do the rest
{$ELSE}   // Version 82+  (using Personal Identification Code)
var
  PIC: string;
  CharID: Integer;
begin
  PIC := Packet.ReadMapleAnsiString;
  CharID := Packet.ReadInt;

  if not (Length(PIC) in [6..12]) then
  begin
    Log(C.AccountName + ' = Hacker (Packet editing)');
    C.Disconnect;
    Exit;
  end;

  if C.PIC <> PIC then
    C.Write(PICWrong)
  else
    C.DeleteCharacter(CharID);
{$ENDIF}
end;

procedure DoSelectChar(const CharID: Integer; C: TMapleClient);
var
  Addy: in_addr;
  IP: TIPArray;
begin
  C.UpdateLoginState(lsServerTransition);

  // ChannelServer IP
  IP := TransformIP(frmSettings.edtCSIP.Text);
  Addy.S_un_b.s_b1 := IP[0];
  Addy.S_un_b.s_b2 := IP[1];
  Addy.S_un_b.s_b3 := IP[2];
  Addy.S_un_b.s_b4 := IP[3];

  C.Write(GetServerIP(Addy, C.Channel.Port, CharID));
end;

{ TSelectCharHandler }

class procedure TSelectCharHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  CharID: Integer;
begin
  CharID := Packet.ReadInt;
  //Packet.ReadMapleAnsiString;    // Macs
  //Log('MAC(s): ' + Macs);

  DoSelectChar(CharID, C);
end;

{ TSelectCharRegisterPICHandler }

class procedure TSelectCharRegisterPICHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  PIC: string;
  CharID: Integer;
begin
  Packet.Skip(1);  // always 1? Status?
  CharID := Packet.ReadInt;
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

{ TSelectCharWithPICHandler }

class procedure TSelectCharWithPICHandler.HandlePacket(Packet: TMapleStream;
  C: TMapleClient);
var
  PIC: string;
  CharID: Integer;
begin
  PIC := Packet.ReadMapleAnsiString;
  CharID := Packet.ReadInt;

  if C.PIC <> PIC then
    C.Write(PICWrong)
  else
    DoSelectChar(CharID, C);
end;

initialization
  HandlerClasses.Add('NameCheck', TNameCheckHandler);
  HandlerClasses.Add('CreateChar', TCreateCharHandler);
  HandlerClasses.Add('DeleteChar', TDeleteCharHandler);
  HandlerClasses.Add('SelectChar', TSelectCharHandler);
  HandlerClasses.Add('SelectCharRegisterPIC', TSelectCharRegisterPICHandler);
  HandlerClasses.Add('SelectCharWithPIC', TSelectCharWithPICHandler);

finalization

end.
