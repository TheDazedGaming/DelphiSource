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

unit BuddyList;

interface

uses Generics.Collections, DatabaseConnection, Utils, SysUtils, MapleStream;

const
  DEFAULT_GROUP = 'Default Group';

type
  TBuddyAddResult = (brFull, brAlreadyOnList, brOK);
  TBuddyOperation = (boAdded, boDeleted);
  TBuddylistMode = (bmUpdate = 7, bmAddRequest = 9, bmFull = $B, bmOppositeFull,
                    bmNameInvalid = $F, bmChannelUpdate = $14);

  TBuddyListEntry = record
    Name, Group: string;
    CID, Channel: Integer;
    Visible: Boolean;

    constructor Create(AName, AGroup: string; ACharID, AChannel: Integer; AVisible: Boolean); overload;
  end;
  PBuddyListEntry = ^TBuddyListEntry;

  TPendingRequest = record
    Name: string;
    ID, Job, Level: Integer;
  end;
  PPendingRequest = ^TPendingRequest;

  TBuddyEnumerator = TDictionary<Integer, PBuddyListEntry>.TValueEnumerator;

  TBuddyList = class
  private
    FBuddies: TDictionary<Integer, PBuddyListEntry>;
    FCapacity: Integer;
    FClient: TObject;
    FPendingRequests: TQueue<PPendingRequest>;

    function GetByID(ID: Integer): PBuddyListEntry;
    function GetByName(Name: string): PBuddyListEntry;
    function GetCount: Integer;
  public
    constructor Create(const ACapacity: Integer; AClient: TObject);
    destructor Destroy; override;

    procedure Add(Name, Group: string; ID, Channel: Integer; Visible: Boolean);

    function Contains(const CharID: Integer): Boolean;
    function ContainsVisible(const CharID: Integer): Boolean;

    function GetBuddyIDs: TArrayofInteger;
    function GetEnumerator: TBuddyEnumerator;
    function IsFull: Boolean;

    procedure LoadFromDatabase(const Connection: TDatabaseConnection; CharID: Integer);
    procedure AddBuddyRequest(ChannelFrom: Integer; Request: PPendingRequest);

    procedure SendError(Error: TBuddylistMode);
    procedure SendNextRequest;
    procedure SendRequest(Request: PPendingRequest);
    procedure SendUpdate;
    procedure SendUpdateChannel(CharID, Channel: Integer);

    function PollPendingRequest: PPendingRequest;

    property Buddies: TDictionary<Integer, PBuddyListEntry> read FBuddies;
    property Buddy[ID: Integer]: PBuddyListEntry read GetByID; default;
    property Buddy[Name: string]: PBuddyListEntry read GetByName; default;
    property Capacity: Integer read FCapacity;
    property Count: Integer read GetCount;
  end;

function CreateRequest(Name: string; ID, Job, Level: Integer): PPendingRequest;

implementation

uses MapleClient, MapleServerHandler;

function CreateRequest(Name: string; ID, Job, Level: Integer): PPendingRequest;
begin
  New(Result);
  Result^.Name := Name;
  Result^.ID := ID;
  Result^.Job := Job;
  Result^.Level := Level;
end;

{ TBuddyListEntry }

constructor TBuddyListEntry.Create(AName, AGroup: string; ACharID,
  AChannel: Integer; AVisible: Boolean);
begin
  Name := AName;
  Group := AGroup;
  CID := ACharID;
  Channel := AChannel;
  Visible := AVisible;
end;

{ TBuddyList }

constructor TBuddyList.Create(const ACapacity: Integer; AClient: TObject);
begin
  FBuddies := TDictionary<Integer, PBuddyListEntry>.Create;
  FCapacity := ACapacity;
  FClient := AClient;
  FPendingRequests := TQueue<PPendingRequest>.Create;
end;

destructor TBuddyList.Destroy;
var
  BLE: PBuddyListEntry;
  PR: PPendingRequest;
begin
  for BLE in FBuddies.Values do
    Dispose(BLE);
  FBuddies.Free;

  for PR in FPendingRequests do
    Dispose(PR);
  FPendingRequests.Free;

  inherited;
end;

procedure TBuddyList.AddBuddyRequest(ChannelFrom: Integer; Request: PPendingRequest);
begin
  Add(Request^.Name, DEFAULT_GROUP, Request^.ID, ChannelFrom, False);
  if FPendingRequests.Count = 0 then
    SendRequest(Request)
  else
    FPendingRequests.Enqueue(Request);
end;

function TBuddyList.Contains(const CharID: Integer): Boolean;
begin
  Result := FBuddies.ContainsKey(CharID);
end;

function TBuddyList.ContainsVisible(const CharID: Integer): Boolean;
begin
  Result := FBuddies.ContainsKey(CharID) and (PBuddyListEntry(FBuddies[CharID])^.Visible);
end;

function TBuddyList.GetByID(ID: Integer): PBuddyListEntry;
begin
  for Result in FBuddies.Values do
    if Result^.CID = ID then
      Exit;

  Result := nil;
end;

function TBuddyList.GetByName(Name: string): PBuddyListEntry;
begin
  for Result in FBuddies.Values do
    if SameText(Result^.Name, Name) then    // fast ASM :D
      Exit;

  Result := nil;
end;

function TBuddyList.GetBuddyIDs: TArrayofInteger;
var
  i, ID: Integer;
begin
  SetLength(Result, FBuddies.Count);
  i := 0;
  for ID in FBuddies.Keys do
  begin
    Result[i] := ID;
    Inc(i);
  end;
end;

function TBuddyList.GetEnumerator: TBuddyEnumerator;
begin
  Result := FBuddies.Values.GetEnumerator;
end;

function TBuddyList.GetCount: Integer;
begin
  Result := FBuddies.Count;
end;

function TBuddyList.IsFull: Boolean;
begin
  Result := FBuddies.Count >= FCapacity;
end;

procedure TBuddyList.LoadFromDatabase(const Connection: TDatabaseConnection;
  CharID: Integer);
begin
  with Connection.GetQuery do
  begin
    SQL.Text := 'SELECT b.buddyid, b.group, b.pending, c.name, c.level, c.job FROM buddies as b, ' +
                'characters as c WHERE c.id = b.buddyid AND b.characterid = :id';
    ParamByName('id').Value := CharID;
    Open;
    try
      while not EOF do
      begin
        if FieldByName('pending').AsInteger = 1 then
          FPendingRequests.Enqueue(CreateRequest(FieldByName('name').AsString, FieldByName('buddyid').AsInteger,
                                                 FieldByName('job').AsInteger, FieldByName('level').AsInteger))
        else
          Add(FieldByName('name').AsString, FieldByName('group').AsString,
              FieldByName('buddyid').AsInteger, -1, True);

        Next;
      end;
      Close;

      SQL.Text := 'DELETE FROM buddies WHERE pending = 1 AND characterid = :id';
      ParamByName('id').Value := CharID;
      ExecSQL;
    finally
      Free;
    end;
  end;
end;

procedure TBuddyList.Add(Name, Group: string; ID, Channel: Integer; Visible: Boolean);
var
  PBLE: PBuddyListEntry;
begin
  New(PBLE);
  PBLE^ := TBuddyListEntry.Create(Name, Group, ID, Channel, Visible);

  if FBuddies.ContainsKey(ID) then
    Dispose(FBuddies[ID]);

  FBuddies.AddOrSetValue(ID, PBLE);
end;

function TBuddyList.PollPendingRequest: PPendingRequest;
begin
  if FPendingRequests.Count > 0 then
    Result := FPendingRequests.Dequeue
  else
    Result := nil;
end;

procedure TBuddyList.SendError(Error: TBuddylistMode);
var
  P: TMapleStream;
begin
  P := TMapleStream.Create;
  with P do
  begin
    WriteShort(OpHandler.SendOps['Buddylist']);

    WriteByte(Error);
  end;

  TMapleClient(FClient).Write(P);
end;

procedure TBuddyList.SendUpdateChannel(CharID, Channel: Integer);
var
  P: TMapleStream;
begin
  P := TMapleStream.Create;
  with P do
  begin
    WriteShort(OpHandler.SendOps['Buddylist']);
    WriteByte(bmChannelUpdate);

    WriteInt(CharID);
    WriteByte(0);
    WriteInt(Channel);
  end;

  TMapleClient(FClient).Write(P);
end;

procedure TBuddyList.SendNextRequest;
var
  Pending: PPendingRequest;
begin
  Pending := PollPendingRequest;
  if Pending <> nil then
    SendRequest(Pending);
end;

procedure TBuddyList.SendRequest(Request: PPendingRequest);
var
  P: TMapleStream;
begin
  P := TMapleStream.Create;
  with P do
  begin
    WriteShort(OpHandler.SendOps['Buddylist']);
    WriteByte(bmAddRequest);

    WriteInt(Request^.ID);
    WriteMapleAnsiString(Request^.Name);
    {$IFDEF VERSION88_UP}
    WriteInt(Request^.Level);
    WriteInt(Request^.Job);
    {$ENDIF}
    WriteInt(Request^.ID);
    WriteAnsiString(RightPaddedStr(Request^.Name, #0, 13));
    WriteByte(1);
    WriteInt(31);
    WriteAnsiString(RightPaddedStr(DEFAULT_GROUP, #0, 17));
    WriteByte(0);
  end;

  TMapleClient(FClient).Write(P);
end;

procedure TBuddyList.SendUpdate;
var
  Buddy: PBuddyListEntry;
  i: Integer;
  P: TMapleStream;
begin
  P := TMapleStream.Create;
  with P do
  begin
    WriteShort(OpHandler.SendOps['Buddylist']);
    WriteByte(bmUpdate);

    WriteByte(Count);
    for Buddy in FBuddies.Values do
      if Buddy^.Visible then
      begin
        WriteInt(Buddy^.CID);
        WriteAnsiString(RightPaddedStr(Buddy^.Name, #0, 13));
        WriteByte(0);
        WriteInt(Buddy^.Channel);
        WriteAnsiString(RightPaddedStr(Buddy^.Group, #0, 17));
      end;

    for i := 0 to Count - 1 do
      WriteInt(0);
  end;

  TMapleClient(FClient).Write(P);
end;

end.
