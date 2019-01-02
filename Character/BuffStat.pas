unit BuffStat;

interface

uses Classes;

const
  L_BITS = {$IF DEFINED(CHAOS)} 256 {$ELSEIF DEFINED(VERSION97_UP)} 160 {$ELSE} 128 {$IFEND};
  L_BYTES = L_BITS div 8;
  H_BYTES = L_BYTES - 1;

type
  TBuffStat = record
    Bytes: array[0..H_BYTES] of Byte;

    constructor Create64(Index: Byte; i: UInt64);

    class operator BitwiseAnd(const a, b: TBuffStat): TBuffStat;
    class operator BitwiseOr(const a, b: TBuffStat): TBuffStat;
    class operator BitwiseXor(const a, b: TBuffStat): TBuffStat;
    class operator LogicalNot(const a: TBuffStat): Boolean;

    class operator Equal(const a, b: TBuffStat): Boolean;
    class operator Implicit(const a: TBuffStat): Boolean;

    procedure Serialize(S: TStream);
  end;

var
  MORPH, RECOVERY, MAPLE_WARRIOR, STANCE, SHARP_EYES, SHADOW_CLAW, WATK, WDEF, MATK, MDEF,
  ACC, AVOID, HANDS, SPEED, JUMP, MAGIC_GUARD, DARK_SIGHT, BOOSTER, POWER_GUARD,
  HYPERBODY_HP, HYPERBODY_MP, INVINCIBLE, SOUL_ARROW,
  COMBO, WK_CHARGE, SHADOW_PARTNER: TBuffStat;

  INFILTRATE: TBuffStat;

  ENERGY_CHARGE, DASH_X, DASH_Y, MONSTER_RIDING, SPEED_INFUSION, HOMING_BEACON: TBuffStat;

implementation

procedure Init;
begin
  MORPH := TBuffStat.Create64(1, $02);
  RECOVERY := TBuffStat.Create64(1, $04);
  MAPLE_WARRIOR := TBuffStat.Create64(1, $08);
  STANCE := TBuffStat.Create64(1, $10);
  SHARP_EYES := TBuffStat.Create64(1, $20);
  SHADOW_CLAW := TBuffStat.Create64(1, $100);

  WATK := TBuffStat.Create64(1, $100000000);
  WDEF := TBuffStat.Create64(1, $200000000);
  MATK := TBuffStat.Create64(1, $400000000);
  MDEF := TBuffStat.Create64(1, $800000000);
  ACC := TBuffStat.Create64(1, $1000000000);
  AVOID := TBuffStat.Create64(1, $2000000000);
  HANDS := TBuffStat.Create64(1, $4000000000);
  SPEED := TBuffStat.Create64(1, $8000000000);
  JUMP := TBuffStat.Create64(1, $10000000000);

  MAGIC_GUARD := TBuffStat.Create64(1, $20000000000);
  DARK_SIGHT := TBuffStat.Create64(1, $40000000000);
  BOOSTER := TBuffStat.Create64(1, $80000000000);
  POWER_GUARD := TBuffStat.Create64(1, $100000000000);
  HYPERBODY_HP := TBuffStat.Create64(1, $200000000000);
  HYPERBODY_MP := TBuffStat.Create64(1, $400000000000);
  INVINCIBLE := TBuffStat.Create64(1, $800000000000);
  SOUL_ARROW := TBuffStat.Create64(1, $1000000000000);

  COMBO := TBuffStat.Create64(1, $20000000000000);
  WK_CHARGE := TBuffStat.Create64(1, $40000000000000);

  SHADOW_PARTNER := TBuffStat.Create64(1, $400000000000000);

  INFILTRATE := TBuffStat.Create64(2, $1000);

  {$IF DEFINED(CHAOS)}
  ENERGY_CHARGE := TBuffStat.Create64(4, $2000000);
  DASH_X := TBuffStat.Create64(4, $4000000);
  DASH_Y := TBuffStat.Create64(4, $8000000);
  MONSTER_RIDING := TBuffStat.Create64(4, $10000000);
  SPEED_INFUSION := TBuffStat.Create64(4, $20000000);
  HOMING_BEACON := TBuffStat.Create64(4, $40000000);
  {$ELSEIF DEFINED(VERSION97)}
  ENERGY_CHARGE := TBuffStat.Create64(2, $8000000);
  DASH_X := TBuffStat.Create64(2, $10000000);
  DASH_Y := TBuffStat.Create64(2, $20000000);
  MONSTER_RIDING := TBuffStat.Create64(2, $40000000);
  SPEED_INFUSION := TBuffStat.Create64(2, $80000000);
  FillChar(HOMING_BEACON.Bytes, L_BYTES, 0);
  HOMING_BEACON.Bytes[0] := $01;
  {$ELSEIF DEFINED(VERSION90)}
  ENERGY_CHARGE := TBuffStat.Create64(2, $80000);
  DASH_X := TBuffStat.Create64(2, $100000);
  DASH_Y := TBuffStat.Create64(2, $200000);
  MONSTER_RIDING := TBuffStat.Create64(2, $400000);
  SPEED_INFUSION := TBuffStat.Create64(2, $800000);
  HOMING_BEACON := TBuffStat.Create64(2, $1000000);
  {$ELSE}
  ENERGY_CHARGE := TBuffStat.Create64(2, $4000000000000);
  DASH_X := TBuffStat.Create64(2, $8000000000000);
  DASH_Y := TBuffStat.Create64(2, $10000000000000);
  MONSTER_RIDING := TBuffStat.Create64(2, $20000000000000);
  SPEED_INFUSION := TBuffStat.Create64(2, $40000000000000);
  HOMING_BEACON := TBuffStat.Create64(2, $80000000000000);
  {$IFEND}
end;

{ UINT256 }

constructor TBuffStat.Create64(Index: Byte; i: UInt64);
begin
  FillChar(Bytes, L_BYTES, 0);
  PUInt64(NativeUInt(@Bytes) + L_BYTES - Index * 8)^ := i;
end;

class operator TBuffStat.Equal(const a, b: TBuffStat): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := H_BYTES downto 0 do
    if a.Bytes[i] <> b.Bytes[i] then
      Exit(False);
end;

class operator TBuffStat.BitwiseAnd(const a, b: TBuffStat): TBuffStat;
var
  i: Integer;
begin
  for i := 0 to H_BYTES do
    Result.Bytes[i] := a.Bytes[i] and b.Bytes[i];
end;

class operator TBuffStat.BitwiseOr(const a, b: TBuffStat): TBuffStat;
var
  i: Integer;
begin
  for i := 0 to H_BYTES do
    Result.Bytes[i] := a.Bytes[i] or b.Bytes[i];
end;

class operator TBuffStat.BitwiseXor(const a, b: TBuffStat): TBuffStat;
var
  i: Integer;
begin
  for i := 0 to H_BYTES do
    Result.Bytes[i] := a.Bytes[i] xor b.Bytes[i];
end;

class operator TBuffStat.LogicalNot(const a: TBuffStat): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := H_BYTES downto 0 do
    if a.Bytes[i] <> 0 then
      Exit(False);
end;

class operator TBuffStat.Implicit(const a: TBuffStat): Boolean;
begin
  Result := not (not a);
end;

procedure TBuffStat.Serialize(S: TStream);
begin
  S.Write(Bytes, L_BYTES);
end;

initialization Init;

end.
