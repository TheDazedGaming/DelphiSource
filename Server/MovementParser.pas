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

unit MovementParser;

interface

uses Types, SysUtils, Generics.Collections, MapleStream, MapleMapObject;

type
  {$IFNDEF VERSION88_UP}
  TMovementCommand = (mcMove, mcJump, mcKnockback, mcBeforeTeleport, mcTeleport,
                      mcFlashJump = 6, mcAssaulter, mcAssassinate, mcRush,
                      mcChangeEquip, mcChair, mcShotJumpBack = 13,
                      mcBeforeJumpDown, mcJumpDown, mcFloat = 17, mcCombatStep = 20,
                      mcAranCombo);
  {$ELSE}
  TMovementCommand = (mcMove, mcJump, mcKnockback, mcAfterTeleport, mcTeleport,
                      mcAssaulter = 6, mcAssassinate, mcRush, mcCharUpdate {$IFDEF CHAOS} = 10{$ENDIF},
                      mcChair, mcBeforeJumpDown, mcJumpDown, mcWingsJump,
                      mcWingsFalling, mcFlashJump = 20, mcRecoilShot = 22,
                      mcTrampoline = 24, mcCombatStep = 26, mcMonsterKnockback);
  {$ENDIF}

  TLifeMovement = class
  protected
    FType: Byte;
    FPosition, FPixelsPerSec: TPoint;
    FDuration, FNewState, FFoothold, FUnk: Integer;
  public
    class function ParseMovement(var Data: TMapleStream): TList<TLifeMovement>;
    class procedure UpdatePosition(Moves: TList<TLifeMovement>; Target: TAnimatedMapObject; YOffset: Integer);

    constructor Create(MType: Byte; Position: TPoint; Duration, NewState: Integer);

    procedure Serialize(var Stream: TMapleStream); virtual; abstract;

    property NewState: Integer read FNewState;
    property Position: TPoint read  FPosition write FPosition;
    property PixelsPerSec: TPoint read FPixelsPerSec write FPixelsPerSec;
    property Foothold: Integer read FFoothold write FFoothold;
    {$IFDEF VERSION88_UP}
    property Unk: Integer read FUnk write FUnk;
    {$ENDIF}
  end;

  TAbsoluteLifeMovement = class(TLifeMovement)
  public
    procedure Serialize(var Stream: TMapleStream); override;
  end;

  TRelativeLifeMovement = class(TLifeMovement)
  public
    procedure Serialize(var Stream: TMapleStream); override;
  end;

  TCharUpdateMovement = class(TLifeMovement)
  public
    procedure Serialize(var Stream: TMapleStream); override;
  end;

  TChairMovement = class(TLifeMovement)
  public
    procedure Serialize(var Stream: TMapleStream); override;
  end;

  TTeleportMovement = class(TLifeMovement)
  public
    procedure Serialize(var Stream: TMapleStream); override;
  end;

  TJumpDownMovement = class(TLifeMovement)
  public
    procedure Serialize(var Stream: TMapleStream); override;
  end;

  TSkillMovement = class(TLifeMovement)
  public
    procedure Serialize(var Stream: TMapleStream); override;
  end;

implementation

uses Main, MapleNPC, MapleServerHandler;

{ TLifeMovement }

constructor TLifeMovement.Create(MType: Byte; Position: TPoint; Duration,
  NewState: Integer);
begin
  FType := MType;
  FPosition := Position;
  FDuration := Duration;
  FNewState := NewState;
end;

class function TLifeMovement.ParseMovement(var Data: TMapleStream): TList<TLifeMovement>;
var
  Command: TMovementCommand;
  CommandCount: Byte;
  i: Integer;
  XPos, YPos, XWobble, YWobble, NewState, Duration, Foothold: Integer;
  XMod, YMod, Unk: Integer;
  LM: TLifeMovement;
  Show: Boolean;
begin
  Show := False;
  Result := TList<TLifeMovement>.Create;
  CommandCount := Data.ReadByte;
  for i := 0 to CommandCount - 1 do
  begin
    Command := TMovementCommand(Data.ReadByte);
    {$IFNDEF VERSION88_UP}
    // OLD
    if not (Command in [mcMove..mcTeleport, mcFlashJump, mcShotJumpBack..mcJumpDown,
                        mcChair, mcChangeEquip, mcAranCombo, mcFloat, mcCombatStep]) then
      Log('[UNKNOWN MOVE NAME] ' + IntToStr(Byte(Command)));
    case Command of
      mcMove, TMovementCommand(5):
      begin
				XPos := Data.ReadShort;
				YPos := Data.ReadShort;
				XWobble := Data.ReadShort;
				YWobble := Data.ReadShort;
				Foothold := Data.ReadShort;
				NewState := Data.ReadByte;
				Duration := Data.ReadShort;

        LM := TAbsoluteLifeMovement.Create(Byte(Command), Point(XPos, YPos), Duration, NewState);
        LM.Foothold := Foothold;
        LM.PixelsPerSec := Point(XWobble, YWobble);

        Result.Add(LM);
      end;

      mcJump, mcKnockback, mcFlashJump, TMovementCommand(12), mcShotJumpBack, TMovementCommand(16):
      begin
        XMod := Data.ReadShort;
        YMod := Data.ReadShort;
        NewState := Data.ReadByte;
        Duration := Data.ReadShort;

        LM := TRelativeLifeMovement.Create(Byte(Command), Point(XMod, YMod), Duration, NewState);

        Result.Add(LM);
      end;

      mcBeforeTeleport, mcTeleport, mcAssaulter..mcRush, mcBeforeJumpDown:
      begin
				XPos := Data.ReadShort;
				YPos := Data.ReadShort;
				XWobble := Data.ReadShort;
				YWobble := Data.ReadShort;
        NewState := Data.ReadByte;

        LM := TTeleportMovement.Create(Byte(Command), Point(XPos, YPos), 0, NewState);
        LM.FPixelsPerSec := Point(XWobble, YWobble);

        Result.Add(LM);
      end;

      mcChangeEquip:
      begin
        LM := TCharUpdateMovement.Create(Byte(Command), Point(0, 0), 0, Data.ReadByte);

        Result.Add(LM);
      end;

      mcChair:
      begin
        XPos := Data.ReadShort;
        YPos := Data.ReadShort;
        Foothold := Data.ReadShort;
        NewState := Data.ReadByte;
        Duration := Data.ReadShort;

        LM := TChairMovement.Create(Byte(Command), Point(XPos, YPos), Duration, NewState);
        LM.Foothold := Foothold;

        Result.Add(LM);
      end;

      mcJumpDown:
      begin
				XPos := Data.ReadShort;
				YPos := Data.ReadShort;
				XWobble := Data.ReadShort;
				YWobble := Data.ReadShort;
        Foothold := Data.ReadInt;  // first 2 bytes are something like an OriginFh
        NewState := Data.ReadByte;
        Duration := Data.ReadShort;

        LM := TJumpDownMovement.Create(Byte(Command), Point(XPos, YPos), Duration, NewState);
        LM.Foothold := Foothold;
        LM.PixelsPerSec := Point(XWobble, YWobble);

        Result.Add(LM);
      end;

      mcAranCombo, mcCombatStep:
      begin
        NewState := Data.ReadByte;
        Unk := Data.ReadShort;

        LM := TSkillMovement.Create(Byte(Command), Point(0, 0), Unk, NewState);
        Result.Add(LM);
      end;

      else
      begin
        Log('[WARNING] Movement type unknown (Command: %d)!', [Byte(Command)]);
        Show := True;
      end;
    end;
    {$ELSE}
    // NEW
    case Command of
      mcMove, mcWingsFalling:
      begin
				XPos := Data.ReadShort;
				YPos := Data.ReadShort;
				XWobble := Data.ReadShort;
				YWobble := Data.ReadShort;
				Foothold := Data.ReadShort;
        Unk := Data.ReadInt;
				NewState := Data.ReadByte;
				Duration := Data.ReadShort;

        LM := TAbsoluteLifeMovement.Create(Byte(Command), Point(XPos, YPos), Duration, NewState);
        LM.Foothold := Foothold;
        LM.PixelsPerSec := Point(XWobble, YWobble);
        LM.Unk := Unk;

        Result.Add(LM);
      end;

      mcJump, mcKnockback, mcWingsJump:
      begin
        XMod := Data.ReadShort;
        YMod := Data.ReadShort;
        NewState := Data.ReadByte;
        Duration := Data.ReadShort;

        LM := TRelativeLifeMovement.Create(Byte(Command), Point(XMod, YMod), Duration, NewState);

        Result.Add(LM);
      end;

      mcAfterTeleport, mcTeleport, mcAssaulter..mcRush, mcBeforeJumpDown:
      begin
				XPos := Data.ReadShort;
				YPos := Data.ReadShort;
				XWobble := Data.ReadShort;
				YWobble := Data.ReadShort;
        NewState := Data.ReadByte;

        LM := TTeleportMovement.Create(Byte(Command), Point(XPos, YPos), 0, NewState);
        LM.FPixelsPerSec := Point(XWobble, YWobble);

        Result.Add(LM);
      end;

      mcCharUpdate:
      begin
        // Byte: 0 = Movement Buff? (at least Nimble Feet); 1 = Unequip; 2 = Equip
        LM := TCharUpdateMovement.Create(Byte(Command), Point(0, 0), 0, Data.ReadByte);
        Result.Add(LM);
      end;

      mcChair:
      begin
        XPos := Data.ReadShort;
        YPos := Data.ReadShort;
        Foothold := Data.ReadShort;
        NewState := Data.ReadByte;
        Duration := Data.ReadShort;

        LM := TChairMovement.Create(Byte(Command), Point(XPos, YPos), Duration, NewState);
        LM.Foothold := Foothold;

        Result.Add(LM);
      end;

      mcJumpDown:
      begin
				XPos := Data.ReadShort;
				YPos := Data.ReadShort;
				XWobble := Data.ReadShort;
				YWobble := Data.ReadShort;
        Foothold := Data.ReadInt;    // first 2 bytes are something like an OriginFh
        Unk := Data.ReadInt;
        NewState := Data.ReadByte;
        Duration := Data.ReadShort;

        LM := TJumpDownMovement.Create(Byte(Command), Point(XPos, YPos), Duration, NewState);
        LM.Foothold := Foothold;
        LM.PixelsPerSec := Point(XWobble, YWobble);
        LM.Unk := Unk;

        Result.Add(LM);
      end;

      mcFlashJump, mcRecoilShot, mcTrampoline, mcCombatStep, mcMonsterKnockback:
      begin
        NewState := Data.ReadByte;
        Unk := Data.ReadShort;

        LM := TSkillMovement.Create(Byte(Command), Point(0, 0), Unk, NewState);
        Result.Add(LM);
      end;

 (*  
      mcFloat:
      begin
        XPos := Data.ReadShort;
        YPos := Data.ReadShort;
        Unk := Data.ReadShort;
        NewState := Data.ReadByte;

        LM := TFloatMovement.Create(Byte(Command), Point(XPos, YPos), Unk, NewState);
        SetLength(Buf, 6);
        Data.Read(Buf[0], 6);
        TFloatMovement(LM).Buffer := Buf;

        Result.Add(LM);
      end;     *)

      else
      begin
        Log('[WARNING] Movement type unknown (Command: %d)!', [Byte(Command)]);
        Show := True;
      end;
    end;
    {$ENDIF}
  end;

  if Show then
    Log(Data.ToString);
end;

class procedure TLifeMovement.UpdatePosition(Moves: TList<TLifeMovement>;
  Target: TAnimatedMapObject; YOffset: Integer);
var
  Move: TLifeMovement;
begin
  for Move in Moves do
  begin
    if Move is TAbsoluteLifeMovement then
    begin
      Move.Position := Point(Move.Position.X, Move.Position.Y + YOffset);
      Target.Position := Point(Move.Position.X, Move.Position.Y);
    end;

    Target.Stance := Move.NewState;
  end;
end;

{ TAbsoluteLifeMovement }

procedure TAbsoluteLifeMovement.Serialize(var Stream: TMapleStream);
begin
  with Stream do
  begin
    WriteByte(FType);
    WritePos(FPosition);
    WritePos(FPixelsPerSec);
    WriteShort(FFoothold);
    {$IFDEF VERSION88_UP}
    WriteInt(FUnk);
    {$ENDIF}
    WriteByte(FNewState);
    WriteShort(FDuration);
  end;
end;

{ TRelativeLifeMovement }

procedure TRelativeLifeMovement.Serialize(var Stream: TMapleStream);
begin
  with Stream do
  begin
    WriteByte(FType);
    WritePos(FPosition);
    WriteByte(FNewState);
    WriteShort(FDuration);
  end;
end;

{ TSingleByteMovement }

procedure TCharUpdateMovement.Serialize(var Stream: TMapleStream);
begin
  Stream.WriteByte(Byte(FType));
  Stream.WriteByte(FNewState);
end;

{ TChairMovement }

procedure TChairMovement.Serialize(var Stream: TMapleStream);
begin
  with Stream do
  begin
    WriteByte(FType);
    WritePos(FPosition);
    WriteShort(FFoothold);
    WriteByte(FNewState);
    WriteShort(FDuration);
  end;
end;

{ TTeleportMovement }

procedure TTeleportMovement.Serialize(var Stream: TMapleStream);
begin
  with Stream do
  begin
    WriteByte(FType);
    WritePos(FPosition);
    WritePos(FPixelsPerSec);
    WriteByte(FNewState);
  end;
end;

{ TJumpDownMovement }

procedure TJumpDownMovement.Serialize(var Stream: TMapleStream);
begin
  with Stream do
  begin
    WriteByte(FType);
    WritePos(FPosition);
    WritePos(FPixelsPerSec);
    WriteInt(FFoothold);
    {$IFDEF VERSION88_UP}
    WriteInt(FUnk);
    {$ENDIF}
    WriteByte(FNewState);
    WriteShort(FDuration);
  end;
end;

{ TSkillMovement }

procedure TSkillMovement.Serialize(var Stream: TMapleStream);
begin
  with Stream do
  begin
    WriteByte(FType);
    WriteByte(FNewState);
    WriteShort(FDuration);  // varname is wrong
  end;
end;

end.
