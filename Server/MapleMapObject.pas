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

unit MapleMapObject;

interface

uses Types;

type
  TObjectType = (otUnknown, otNPC, otMonster, otItem, otPlayer, otDoor, otSummon,
                 otShop, otMist, otReactor);

  TMapleMapObject = class
  protected
    FPosition: TPoint;
    FObjectID: Integer;
  public
    // Fucking unit references force me to always cast it...
    function SendSpawnDataTo(Client: TObject): Boolean; virtual; abstract;
    procedure SendDestroyDataTo(Client: TObject); virtual; abstract;

    function GetObjectID: Integer; virtual;
    function GetType: TObjectType; virtual;

    property Position: TPoint read FPosition write FPosition;
    property ObjectID: Integer read GetObjectID write FObjectID;
  end;

  TAnimatedMapObject = class(TMapleMapObject)
  protected
    FStance: Integer;
  public
    property Stance: Integer read FStance write FStance;
  end;

  TLoadedLife = class(TAnimatedMapObject)
  protected
    FID: Integer;
    FFoothold, FMinClickPos, FMaxClickPos: Integer;
    FFacesRight: Boolean;
  public
    constructor Create(const LifeID: Integer); virtual;

    property ID: Integer read FID;
    property Foothold: Integer read FFoothold write FFoothold;
    property MinClickPos: Integer read FMinClickPos write FMinClickPos;
    property MaxClickPos: Integer read FMaxClickPos write FMaxClickPos;
    property FacesRight: Boolean read FFacesRight write FFacesRight;
  end;

implementation

{ TLoadedLife }

constructor TLoadedLife.Create(const LifeID: Integer);
begin
  FID := LifeID;
end;

{ TMapleMapObject }

function TMapleMapObject.GetObjectID: Integer;
begin
  Result := FObjectID;
end;

function TMapleMapObject.GetType: TObjectType;
begin
  Result := otUnknown;   // to be overriden
end;

end.
