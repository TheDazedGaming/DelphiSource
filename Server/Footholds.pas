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

unit Footholds;

interface

uses Types, Generics.Collections, Generics.Defaults;

type
  TFoothold = class
  private
    FP1, FP2: TPoint;
    FID, FNext, FPrev: Integer;
  public
    constructor Create(const P1, P2: TPoint; AID: Integer);

    function IsWall: Boolean;

    property Next: Integer read FNext write FNext;
    property Prev: Integer read FPrev write FPrev;
    property X1: Integer read FP1.X;
    property X2: Integer read FP2.X;
    property Y1: Integer read FP1.Y;
    property Y2: Integer read FP2.Y;
  end;

  TFootholdTree = class
  private
    FFootholds: TList<TFoothold>;
    FP1, FP2: TPoint;
  public
    constructor Create(const P1, P2: TPoint);
    destructor Destroy; override;

    function FindBelow(const P: TPoint): TFoothold;
    procedure Insert(F: TFoothold);
  end;

implementation

uses Main;

{ TFoothold }

constructor TFoothold.Create(const P1, P2: TPoint; AID: Integer);
begin
  FP1 := P1;
  FP2 := P2;
  FID := AID;
end;

function TFoothold.IsWall: Boolean;
begin
  Result := FP1.X = FP2.X;
end;

{ TFootholdTree }

constructor TFootholdTree.Create(const P1, P2: TPoint);
begin
  FP1 := P1;
  FP2 := P2;
  FFootholds := TList<TFoothold>.Create;
end;

destructor TFootholdTree.Destroy;
var
  F: TFoothold;
begin
  for F in FFootholds do
    F.Free;

  FFootholds.Free;

  inherited;
end;

function TFootholdTree.FindBelow(const P: TPoint): TFoothold;
var
  XMatches: TList<TFoothold>;
  F: TFoothold;
  CalcY: Integer;
  S1, S2, S4, S5, Alpha, Beta: Double;
begin
  XMatches := TList<TFoothold>.Create(TComparer<TFoothold>.Construct(
    function(const Left, Right: TFoothold): Integer
    begin
      if Left.Y1 < Right.Y1 then
        Result := -1
      else if Left.Y1 > Right.Y1 then
        Result := 1
      else
        Result := 0;
    end));

  for F in FFootholds do
    if (F.X1 <= P.X) and (F.X2 >= P.X) then
      XMatches.Add(F);

  XMatches.Sort;

  try
    for F in XMatches do
      if (not F.IsWall) and (F.Y1 <> F.Y2) then
      begin
        S1 := Abs(F.Y2 - F.Y1);
        S2 := Abs(F.X2 - F.X1);
        S4 := Abs(P.X - F.X1);
        Alpha := ArcTan(S2 / S1);
        Beta := ArcTan(S1 / S2);
        S5 := Cos(Alpha) * (S4 / Cos(Beta));
        if F.Y2 < F.Y1 then
          CalcY := F.Y1 - Trunc(S5)
        else
          CalcY := F.Y1 + Trunc(S5);

        if CalcY >= P.Y then
          Exit(F);
      end
      else if (not F.IsWall) and (F.Y1 >= P.Y) then
        Exit(F);
  finally
    XMatches.Free;
  end;

  Result := nil;
end;

procedure TFootholdTree.Insert(F: TFoothold);
begin
  FFootholds.Add(F);
end;

end.
