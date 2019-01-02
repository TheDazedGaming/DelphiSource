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

unit MapleClient;

interface

uses Classes, SysUtils, IdContext, DECHash, DECFmt, MapleCrypt,
     Generics.Collections, MapleCharacter, ZDataset, ChannelServer,
     DatabaseConnection, Utils, MapleStream, WorldServer, SyncObjs, MapleParty;

type
  TLoginStatus = (lsOK, lsBanned = 3, lsInvalidPW, lsNotRegistered, lsSystemError, lsAlreadyLoggedIn);
  TLoginState = (lsNotLoggedIn, lsServerTransition, lsLoggedIn);

  TMapleClient = class
  private
    FDB: TDatabaseConnection;
    FCon: TIdContext;
    FPacketProcessor: TObject;
    FCommandProcessor: TObject;
    FRecvCrit, FSendCrit: TCriticalSection;

    FAccountName, FPIC: string;
    FPin: Integer;
    FAccID: Integer;
    FBirthday: TDateTime;
    FGender: Byte;
    FCreationTime: TDateTime;
    FIsGM: Boolean;
    FLoggedIn, FServerTransition: Boolean;

    FWorld: TWorldServer;
    FChannel: TChannelServer;
    FPlayer: TMapleCharacter;

    function FinishLogin: TLoginStatus;

    function LoadCharactersInternal(WorldID: Byte): TList<TMiniCharInfo>;
    procedure SetPin(const Value: Integer);
    procedure SetPIC(const Value: string);
    {$IFNDEF EMS}
    procedure UpdatePasswordHash(const AccName, PW: string);
    {$ENDIF}
    function GetAccountName: string;
  public
    constructor Create(Con: TIdContext);
    destructor Destroy; override;

    procedure ReceivePacket(var Data: TMemoryStream);
    procedure Write(Data: TMapleStream; FreeData: Boolean = True);

    procedure BeforeReconnect;
    procedure Disconnect;

    procedure CheckLogin(AccName, PW: string);
    function CheckPin(Pin: Integer; SendCorrect: Boolean = True): Boolean;
    procedure CheckBirthdate(Date: TDateTime; CharID: Integer);
    procedure DeleteCharacter(ID: Integer);

    function HandleCommand(Cmd: string): Boolean;

    function LoadCharacters(WorldID: Byte): TList<TMapleCharacter>;
    function LoadCharacterNames(WorldID: Byte): TList<string>;

    function GetLoginState: TLoginState;
    procedure UpdateLoginState(const Value: TLoginState);

    property DB: TDatabaseConnection read FDB;
    property Connection: TIdContext read FCon;
    property AccountName: string read GetAccountName write FAccountName;
    property AccID: Integer read FAccID write FAccID;
    property Pin: Integer read FPin write SetPin;
    property PIC: string read FPIC write SetPIC;
    property Gender: Byte read FGender write FGender;
    property CreationTime: TDateTime read FCreationTime;
    property IsGM: Boolean read FIsGM;
    property IsLoggedIn: Boolean read FLoggedIn;

    property World: TWorldServer read FWorld write FWorld;
    property Channel: TChannelServer read FChannel write FChannel;
    property Player: TMapleCharacter read FPlayer write FPlayer;
  end;

implementation

uses Main, MaplePacketCreator, MapleServerHandler, Settings, PacketProcessor,
     CommandProcessor;

{ TMapleClient }

constructor TMapleClient.Create(Con: TIdContext);
var
  Hello: TMemoryStream;
  IO: TMapleIOHandler;
begin
  FCon := Con;

  FAccountName := '';
  FGender := 0;   // xxx Always male lol... well, DB doesn't support it yet
  FLoggedIn := False;
  FServerTransition := False;

  FDB := TDatabaseConnection.Create(frmSettings.edtSQLName.Text);

  FPacketProcessor := TPacketProcessor.Create(Self);
  FCommandProcessor := TCommandProcessor.Create(Self);
  FSendCrit := TCriticalSection.Create;
  FRecvCrit := TCriticalSection.Create;

  IO := TMapleIOHandler(FCon.Connection.IOHandler);
  Hello := GetHello(IV_RECV, IV_SEND);
  try
    IO.WriteUnencrypted(Hello);
  finally
    Hello.Free;
  end;
end;

destructor TMapleClient.Destroy;
begin
  // This is also called when the connection is closed

  if Assigned(FPlayer) then
    try
      FPlayer.Map.RemovePlayer(FPlayer);

      if (not FServerTransition) and (FLoggedIn) then
        FWorld.LoggedOff(FPlayer.Name, FPlayer.ID, FChannel.Index, FPlayer.BuddyList.GetBuddyIDs);

      if FPlayer.Party <> nil then
      begin
        FPlayer.PartyChar.Channel := -2;
        FWorld.UpdateParty(FPlayer.Party.ID, poUpdate, FPlayer.PartyChar);
      end;
    finally
      if Assigned(FChannel) then
        FChannel.RemovePlayer(FPlayer)
      else
        Log('[FATAL] No channel was assigned to the MapleClient of player %s!', [FPlayer]);

      FreeAndNil(FPlayer);
    end;

  if (not FServerTransition) and (FLoggedIn) then
    UpdateLoginState(lsNotLoggedIn);

  FreeAndNil(FPacketProcessor);
  FreeAndNil(FCommandProcessor);
  FreeAndNil(FDB);
  FreeAndNil(FSendCrit);
  FreeAndNil(FRecvCrit);

  inherited;
end;

procedure TMapleClient.Disconnect;
begin
  FCon.Connection.Disconnect;
end;

procedure TMapleClient.Write(Data: TMapleStream; FreeData: Boolean = True);
{ Send a packet to the client }
begin
  FSendCrit.Enter;
  try
    FCon.Connection.IOHandler.Write(Data);
  finally
    FSendCrit.Leave;
    if FreeData then
      FreeAndNil(Data);
  end;
end;

procedure TMapleClient.ReceivePacket(var Data: TMemoryStream);
{ Receive a packet from the client, called by OnExecute of the Indy-Server only! }
begin
  FRecvCrit.Enter;
  try
    TPacketProcessor(FPacketProcessor).ProcessPacket(Data);
  finally
    FRecvCrit.Leave;
  end;
end;

function TMapleClient.HandleCommand(Cmd: string): Boolean;
{ This is called everytime the player sends a chat-message, function self-explanatory }
begin
  if Assigned(FCommandProcessor) then
    Result := TCommandProcessor(FCommandProcessor).HandleCommand(Cmd)
  else
    Result := False;
end;

function TMapleClient.LoadCharacters(WorldID: Byte): TList<TMapleCharacter>;
var
  IDs: TList<TMiniCharInfo>;
  i: Integer;
begin
  IDs := LoadCharactersInternal(WorldID);
  try
    Result := TList<TMapleCharacter>.Create;

    for i := 0 to IDs.Count - 1 do
      Result.Add(TMapleCharacter.LoadFromDB(TMiniCharInfo(IDs[i]).ID, Self, False));
  finally
    IDs.Free;
  end;
end;

function TMapleClient.LoadCharacterNames(WorldID: Byte): TList<string>;
var
  MCI: TMiniCharInfo;
  Infos: TList<TMiniCharInfo>;
begin
  Result := TList<string>.Create;
  Infos := LoadCharactersInternal(WorldID);
  for MCI in Infos do
    Result.Add(MCI.Name);
  Infos.Free;
end;

function TMapleClient.LoadCharactersInternal(WorldID: Byte): TList<TMiniCharInfo>;
const
  Query = 'SELECT id, name FROM characters WHERE accountid = %d AND world = %d;';
var
  Q: TZQuery;
begin
  Result := TList<TMiniCharInfo>.Create;
  try
    Q := FDB.GetQuery;
    Q.SQL.Text := Format(Query, [FAccID, WorldID]);
    Q.Open;

    while not Q.EOF do
    begin
      Result.Add(TMiniCharInfo.Create(Q.FieldByName('id').AsInteger, Q.FieldByName('name').AsString));
      Q.Next;
    end;

    Q.Close;
    FreeAndNil(Q);
  except
    Log('Database exception @ LoadCharactersInternal: ' + Exception(ExceptObject).Message);
  end;
end;

procedure TMapleClient.SetPIC(const Value: string);
begin
  with FDB.GetQuery do
    try
      SQL.Text := 'UPDATE accounts SET `pic` = :pic WHERE `id` = :aid';
      ParamByName('pic').Value := AnsiString(Value);
      ParamByName('aid').Value := FAccID;
      ExecSQL;
    finally
      Free;
    end;

  FPIC := Value;
end;

procedure TMapleClient.SetPin(const Value: Integer);
{ Updates the database pin and also sends a PinAssigned packet }
var
  Q: TZQuery;
begin
  Q := FDB.GetQuery;
  try
    Q.SQL.Text := Format('UPDATE accounts SET pin = %d WHERE id = %d', [Value, FAccID]);
    Q.ExecSQL;
  finally
    Q.Free;
  end;

  FPin := Value;
  Write(PinAssigned);
end;

procedure TMapleClient.UpdateLoginState(const Value: TLoginState);
var
  Q: TZQuery;
begin
  Q := FDB.GetQuery;
  try
    Q.SQL.Text := 'UPDATE accounts SET loggedin = :li, lastlogin = CURRENT_TIMESTAMP() WHERE id = :id';
    Q.ParamByName('li').Value := Integer(Value);
    Q.ParamByName('id').Value := FAccID;
    Q.ExecSQL;
  finally
    Q.Free;
  end;

  if Value = lsNotLoggedIn then
  begin
    FLoggedIn := False;
    FServerTransition := False;
  end
  else
  begin
    FServerTransition := Value = lsServerTransition;
    FLoggedIn := not FServerTransition;
  end;
end;

{$IFNDEF EMS}
procedure TMapleClient.UpdatePasswordHash(const AccName, PW: string);
{ Calculates the SHA512 hash of the password, makes a new salt and updates the information in the database }
const
  Query = 'UPDATE `accounts` SET `password` = ''%s'', `salt` = ''%s'' WHERE name = ''%s''';
var
  NewSalt, NewPW: string;
begin
  NewSalt := RandomHexStr(32);
  NewPW := string(THash_SHA512.CalcBinary(RawByteString(PW + NewSalt), TFormat_HEX));

  with FDB.GetQuery do
    try
      SQL.Text := Format(Query, [NewPW, NewSalt, AccName]);
      ExecSQL;
    finally
      Free;
    end;
end;
{$ENDIF}

procedure TMapleClient.BeforeReconnect;
begin
  FRecvCrit.Enter;
  FSendCrit.Enter;
end;

procedure TMapleClient.CheckBirthdate(Date: TDateTime; CharID: Integer);
begin
  if Date = FBirthday then
  begin
    Log('Date correct! Deleting...');
    DeleteCharacter(CharID);
  end
  else
  begin
    Log('Wrong date!');
    Write(DeleteCharResponse(CharID, dsDateWrong));
  end;
end;

procedure TMapleClient.DeleteCharacter(ID: Integer);
var
  Ex: Boolean;
begin
  Ex := False;
  try
    with FDB.GetQuery do
    begin
      // Hacking: Tries to delete a character he/she doesn't own.
      SQL.Text := 'SELECT accountid FROM characters WHERE id = ' + IntToStr(ID);
      Open;
      if FieldByName('accountid').AsInteger <> FAccID then
      begin
        Close;
        Free;
        Disconnect;
        Exit;
      end;
      Close;

      // Everything OK, delete it
      SQL.Text := 'DELETE FROM characters WHERE id = ' + IntToStr(ID);
      ExecSQL;
      Close;
      Free;
    end;
  except
    Ex := True;
    Log('DATABASE ERROR while deleting char ' + IntToStr(ID));
    Write(DeleteCharResponse(ID, dsDBError));
  end;

  if not Ex then
    Write(DeleteCharResponse(ID, dsOK));
end;

function TMapleClient.GetAccountName: string;
begin
  if FAccountName <> '' then
    Exit(FAccountName);

  with FDB.GetQuery do
  begin
    SQL.Text := 'SELECT name FROM accounts WHERE id = :id';
    ParamByName('id').Value := FAccID;
    Open;

    if not EOF then
      Result := FieldByName('name').AsString
    else
    begin
      Log('[Strange] GetAccountName failed for id %d', [FAccID]);
      Result := '';
    end;

    FAccountName := Result;

    Close;
    Free;
  end;
end;

function TMapleClient.GetLoginState: TLoginState;
var
  Q: TZQuery;
  LastLogin: TDateTime;
begin
  Result := lsLoggedIn;    // Standard

  Q := FDB.GetQuery;
  Q.SQL.Text := 'SELECT loggedin, lastlogin FROM accounts WHERE id = ' + IntToStr(FAccID);
  Q.Open;
  try
    if Q.EOF then
      raise Exception.Create('FATAL: GetLoginState EOF');

    Result := TLoginState(Q.FieldByName('loggedin').AsInteger);
    if Result = lsServerTransition then
    begin
      LastLogin := Q.FieldByName('lastlogin').AsDateTime;
      // connecting to chanserver timeout
      if LastLogin + 0.00035 < Now then   // 0.00035 is the TDateTime equivalent of 30 seconds
      begin
        Result := lsNotLoggedIn;
        UpdateLoginState(Result);
      end;
    end;

    FLoggedIn := Result = lsLoggedIn;
  finally
    Q.Close;
    Q.Free;
  end;
end;

function TMapleClient.FinishLogin: TLoginStatus;
begin
  if GetLoginState > lsNotLoggedIn then
  begin
    FLoggedIn := False;
    Exit(lsAlreadyLoggedIn);
  end;

  UpdateLoginState(lsLoggedIn);
  Result := lsOK;
end;

procedure TMapleClient.CheckLogin(AccName, PW: string);
const
  Query = 'SELECT id, password, salt, birthday, pin, pic, createdat, banned, gm FROM ' +
          'accounts WHERE name = ''%s'';';
var
  Q: TZQuery;

  procedure WrongPassword;
  begin
    Write(GetLoginFailed(lsInvalidPW));
  end;

  procedure PasswordCorrect;
  var
    Banned: Integer;
    Status: TLoginStatus;
  begin
    FAccountName := AccName;
    try
      FPin := Q.FieldByName('pin').AsInteger;
    except
      FPin := -1;
      Log('[WARNING] Could NOT read pin from database - not set?');
    end;
    try
      FPIC := Q.FieldByName('pic').AsString;
    except
      FPIC := '';
    end;
    FAccID := Q.FieldByName('id').AsInteger;
    FBirthday := Q.FieldByName('birthday').AsDateTime;
    FCreationTime := Q.FieldByName('createdat').AsDateTime;
    FIsGM := Q.FieldByName('gm').AsInteger <> 0;
    Banned := Q.FieldByName('banned').AsInteger;

    if Banned > 0 then
      Write(GetLoginFailed(lsBanned))
    else
    begin
      Status := FinishLogin;

      if Status = lsOK then
        Write(GetAuthSuccessRequestPin(Self))
      else
        Write(GetLoginFailed(Status));
    end;
  end;

begin
  Q := FDB.GetQuery;
  Q.SQL.Text := Format(Query, [AccName]);
  Q.Open;

  // EOF is true when the name wasn't found
  if not Q.EOF then
  begin
    // Dont check EMS passwords - I don't know how to decrypt them.
    {$IFNDEF EMS}
    // Is this a new account (no salt set yet/it is wrong, password = SHA1)
    if (Q.FieldByName('salt').IsNull) or (Length(Q.FieldByName('salt').AsString) < 16) then
    begin
      // Is the pw in the DB a SHA1 hash or maybe even the plain password?
      if (string(THash_SHA1.CalcBinary(RawByteString(PW), TFormat_HEX)) =
          Uppercase(Q.FieldByName('password').AsString)) or (PW = Q.FieldByName('password').AsString) then
      begin
        // Generate a salt and calculate the SHA512 hash, then save it to the DB
        UpdatePasswordHash(AccName, PW);
        PasswordCorrect;
      end
      else    // SHA1 / plain pw did not match
        WrongPassword;
    end
    else   // Calculate SHA512 hash with salt using Delphi Encryption Compendium
    if string(THash_SHA512.CalcBinary(RawByteString(PW + Q.FieldByName('salt').AsString), TFormat_HEX)) <>
       Uppercase(Q.FieldByName('password').AsString) then
    begin
      WrongPassword;
    end
    else   // correct
    begin
    {$ENDIF}
      PasswordCorrect;
    {$IFNDEF EMS}
    end;
    {$ENDIF}
  end
  else   // This is not an registered ID.
    Write(GetLoginFailed(lsNotRegistered));

  Q.Close;
  FreeAndNil(Q);
end;

function TMapleClient.CheckPin(Pin: Integer; SendCorrect: Boolean = True): Boolean;
begin
  Result := FPin = Pin;

  // does the entered pin match the one from the database?
  if Result then
  begin
    if SendCorrect then
      Write(PinOperation(poAccepted))
  end
  else
    Write(PinOperation(poInvalid));
end;

end.
