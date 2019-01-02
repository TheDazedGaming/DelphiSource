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

unit DropDataProvider;

interface

uses SysUtils, Generics.Collections;

type
  TDropInfo = record
    IsMesos: Boolean;
    ItemID: Integer;
    MinAmount, MaxAmount: Integer;
    QuestID: Word;
    Chance: Cardinal;
  end;
  TDropsInfo = TList<TDropInfo>;

  TDropDataProvider = class
  private
    FDrops: TDictionary<Integer, TDropsInfo>;
  public
    constructor Create;
    destructor Destroy; override;

    function HasDrops(oID: Integer): Boolean;
    function GetDrops(oID: Integer): TDropsInfo;
  end;

var
  DropDataProv: TDropDataProvider;

implementation

uses MapleServerHandler;

{ TDropDataProvider }

constructor TDropDataProvider.Create;
begin
  FDrops := TDictionary<Integer, TDropsInfo>.Create;
end;

destructor TDropDataProvider.Destroy;
var
  Drops: TDropsInfo;
begin
  for Drops in FDrops.Values do
    Drops.Free;
  FreeAndNil(FDrops);

  inherited;
end;

function TDropDataProvider.HasDrops(oID: Integer): Boolean;
begin
  if FDrops.ContainsKey(oID) then
    Exit(True);

  with MCDB.GetQuery do
  begin
    SQL.Text := 'SELECT chance FROM drop_data WHERE dropperid = ' + IntToStr(oID);
    Open;

    Result := not EOF;

    Close;
    Free;
  end;
end;

function TDropDataProvider.GetDrops(oID: Integer): TDropsInfo;
var
  Drop: TDropInfo;
  L: TDropsInfo;
begin
  if FDrops.ContainsKey(oID) then
    Exit(FDrops[oID]);

  with MCDB.GetQuery do
  begin
    SQL.Text := 'SELECT * FROM drop_data WHERE dropperid = ' + IntToStr(oID);
    Open;

    while not EOF do
    begin
      Drop.IsMesos := Pos('is_mesos', FieldByName('flags').AsString) > 0;
      Drop.ItemID := FieldByName('itemid').AsInteger;
      Drop.MinAmount := FieldByName('minimum_quantity').AsInteger;
      Drop.MaxAmount := FieldByName('maximum_quantity').AsInteger;
      Drop.QuestID := FieldByName('questid').AsInteger;
      Drop.Chance := FieldByName('chance').AsInteger;

      if not FDrops.ContainsKey(oID) then
      begin
        L := TDropsInfo.Create;
        L.Add(Drop);
        FDrops.Add(oID, L);
      end
      else
        FDrops[oID].Add(Drop);

      Next;
    end;

    Close;
    Free;
  end;

  if FDrops.ContainsKey(oID) then
    Result := FDrops[oID]
  else
    Result := nil;
end;

end.
