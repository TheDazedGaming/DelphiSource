unit LifeDataProvider;

interface

uses Windows, SysUtils, ZDataset, Generics.Collections;

type
  TMonsterStats = record
    ID, EXP: Integer;
    HP, MP, Level: Integer;
    RemoveAfter: Integer;
    Boss, Undead, PublicReward, CanFreeze, CanPoison: Boolean;
    Summons: TList<Integer>;
  end;
  PMonsterStats = ^TMonsterStats;

  TLifeDataProvider = class
  private
    FMonsterStats: TDictionary<Integer, PMonsterStats>;
  public
    constructor Create;
    destructor Destroy; override;

    function GetMonsterStats(ID: Integer): PMonsterStats;
  end;

var
  LifeDataProv: TLifeDataProvider;

implementation

uses MapleServerHandler;

{ TLifeDataProvider }

constructor TLifeDataProvider.Create;
begin
  FMonsterStats := TDictionary<Integer, PMonsterStats>.Create;
end;

destructor TLifeDataProvider.Destroy;
var
  S: PMonsterStats;
begin
  for S in FMonsterStats.Values do
  begin
    if Assigned(S^.Summons) then
      S^.Summons.Free;
    Dispose(S);
  end;

  FMonsterStats.Free;

  inherited;
end;

function TLifeDataProvider.GetMonsterStats(ID: Integer): PMonsterStats;
var
  Q: TZQuery;
  Attr: string;
begin
  if FMonsterStats.ContainsKey(ID) then
    Exit(FMonsterStats[ID]);

  Q := MCDB.GetQuery;
  Q.SQL.Text := 'SELECT * FROM mob_data WHERE mobid = ' + IntToStr(ID);
  Q.Open;

  if not Q.EOF then
  begin
    New(Result);

    with Q, Result^ do
    begin
      Result^.ID := Q.FieldByName('mobid').AsInteger;
      Level := Q.FieldByName('mob_level').AsInteger;
      HP := Q.FieldByName('hp').AsInteger;
      MP := Q.FieldByName('mp').AsInteger;
      Exp := Q.FieldByName('experience').AsInteger;
      RemoveAfter := Q.FieldByName('death_after').AsInteger;
      Boss := Pos('boss', Q.FieldByName('flags').AsString) > 0;
      Undead := Pos('undead', Q.FieldByName('flags').AsString) > 0;
      PublicReward := Pos('public_reward', Q.FieldByName('flags').AsString) > 0;
      Attr := Q.FieldByName('ice_modifier').AsString;
      CanFreeze := (not Boss) and (Attr <> 'immune') and (Attr <> 'strong');
      Attr := Q.FieldByName('poison_modifier').AsString;
      CanPoison := (not Boss) and (Attr <> 'immune') and (Attr <> 'strong');
      Summons := nil;  // std

      FMonsterStats.Add(ID, Result);
    end;
  end
  else
    Result := nil;

  Q.Close;

  try
    Q.SQL.Text := 'SELECT summonid FROM mob_summons WHERE mobid = ' + IntToStr(ID);
    Q.Open;

    if (Result = nil) or (Q.Eof) then
      Exit;

    Result^.Summons := TList<Integer>.Create;
    while not Q.EOF do
    begin
      Result^.Summons.Add(Q.FieldByName('summonid').AsInteger);

      Q.Next;
    end;
  finally
    Q.Close;
    Q.Free;
  end;
end;

end.
