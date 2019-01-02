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

unit ReactorDataProvider;

interface

uses SysUtils, Types, Generics.Collections, ZDataset, MapleReactor;

type
  TReactorDataProvider = class
  private
    FReactorStats: TDictionary<Integer, PReactorStats>;

    procedure LoadStates(var Stats: PReactorStats; const ID: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function GetReactorStats(const ID: Integer): PReactorStats;
  end;

var
  ReactorDataProv: TReactorDataProvider;   // Singleton

implementation

uses MapleServerHandler;

function StringIndex(const SearchString: string; StrList: array of string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(StrList) do
    if CompareText(SearchString, StrList[I]) = 0 then
      Exit(I);
end;

{ TReactorDataProvider }

constructor TReactorDataProvider.Create;
begin
  FReactorStats := TDictionary<Integer, PReactorStats>.Create;
end;

destructor TReactorDataProvider.Destroy;
var
  PRS: PReactorStats;
  PRSIL: TList<PReactorStateInfo>;
  PRSI: PReactorStateInfo;
begin
  for PRS in FReactorStats.Values do
  begin
    for PRSIL in PRS^.States.Values do
    begin
      for PRSI in PRSIL do
        Dispose(PRSI);

      PRSIL.Free;
    end;

    PRS^.States.Free;
    Dispose(PRS);
  end;

  FReactorStats.Free;

  inherited;
end;

function TReactorDataProvider.GetReactorStats(const ID: Integer): PReactorStats;
begin
  if FReactorStats.ContainsKey(ID) then
    Exit(FReactorStats[ID]);

  with MCDB.GetQuery do
  begin
    SQL.Text := 'SELECT * FROM reactor_data WHERE reactorid = ' + IntToStr(ID);
    Open;

    try
      if EOF then
        Exit(nil);

      New(Result);
      Result^.MaxStates := FieldByName('max_states').AsInteger;
      Result^.Link := FieldByName('link').AsInteger;
      Result^.RemoveInFieldSet := Pos('remove_in_field_set', FieldByName('flags').AsString) > 0;
      Result^.ActivateByTouch := Pos('activate_by_touch', FieldByName('flags').AsString) > 0;

      Result^.States := TDictionary<Byte, TList<PReactorStateInfo>>.Create;
      LoadStates(Result, ID);

      FReactorStats.Add(ID, Result);
    finally
      Close;
      Free;
    end;
  end;
end;

procedure TReactorDataProvider.LoadStates(var Stats: PReactorStats; const ID: Integer);
var
  L: TList<PReactorStateInfo>;
  Info: PReactorStateInfo;
  bState: Byte;
begin
  with MCDB.GetQuery do
  begin
    SQL.Text := 'SELECT * FROM reactor_events WHERE reactorid = ' + IntToStr(ID);
    Open;

    try
      while not EOF do
      begin
        bState := FieldByName('state').AsInteger;
        if not Stats^.States.ContainsKey(bState) then
        begin
          L := TList<PReactorStateInfo>.Create;
          Stats^.States.Add(bState, L);
        end
        else
          L := Stats^.States[bState];

        New(Info);
        Info^.NextState := FieldByName('next_state').AsInteger;

        case StringIndex(FieldByName('event_type').AsString, ['plain_advance_state',
          'no_clue', 'no_clue2', 'hit_from_left', 'hit_from_right', 'hit_by_skill',
          'hit_by_item']) of
          0, 1, 2: Info^.SType := 0;
          3: Info^.SType := 2;    // left
          4: Info^.SType := 3;    // right
          5: Info^.SType := 5;    // skill
          6: Info^.SType := 100;  // item
        end;

        Info^.ItemID := FieldByName('itemid').AsInteger;
        Info^.ItemQuantity := FieldByName('quantity').AsInteger;
        Info^.Lt := Point(FieldByName('ltx').AsInteger, FieldByName('lty').AsInteger);
        Info^.Rb := Point(FieldByName('rbx').AsInteger, FieldByName('rby').AsInteger);
        Info^.Timeout := FieldByName('timeout').AsInteger;

        L.Add(Info);

        Next;
      end;
    finally
      Close;
      Free;
    end;
  end;
end;

end.
