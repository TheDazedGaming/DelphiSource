unit MapleParty;

interface

uses Types, SysUtils, Generics.Collections, GameLogic;

type
  TMaplePartyCharacter = class
  private
    FID, FLevel: Integer;
    FName: string;
    FChannel, FMapID: Integer;
    FJob: TMapleJob;
    FDoorTown, FDoorTarget: Integer;
    FDoorPosition: TPoint;
  public
    constructor Create; overload;
    constructor Create(Char: TObject); overload;
    procedure Init(Char: TObject);

    property Channel: Integer read FChannel write FChannel;
    property ID: Integer read FID;
    property Job: TMapleJob read FJob;
    property Level: Integer read FLevel;
    property MapID: Integer read FMapID;
    property Name: string read FName;
    property DoorTown: Integer read FDoorTown;
    property DoorTarget: Integer read FDoorTarget;
    property DoorPosition: TPoint read FDoorPosition;
  end;

  TPartyOperation = (poJoin, poExpel, poLeave, poDisband, poUpdate, poChangeLeader);
  TMapleParty = class
  private
    FID: Integer;
    FLeader: TMaplePartyCharacter;
    FMembers: TList<TMaplePartyCharacter>;

    function GetMember(ID: Integer): TMaplePartyCharacter; overload;
    {$HINTS OFF}
    function GetMember(Name: string): TMaplePartyCharacter; overload;
    {$HINTS ON}
  public
    constructor Create(const AID: Integer; ALeader: TMaplePartyCharacter);
    destructor Destroy; override;

    function GetEnumerator: TList<TMaplePartyCharacter>.TEnumerator;

    property ID: Integer read FID;
    property Leader: TMaplePartyCharacter read FLeader write FLeader;
    property Member[ID: Integer]: TMaplePartyCharacter read GetMember; default;  // default so it can be overloaded
    property Member[Name: string]: TMaplePartyCharacter read GetMember; default;
    property Members: TList<TMaplePartyCharacter> read FMembers;
  end;

implementation

uses MapleClient, MapleCharacter;

{ TMaplePartyCharacter }

constructor TMaplePartyCharacter.Create(Char: TObject);
begin
  Init(Char);
end;

constructor TMaplePartyCharacter.Create;
begin
  // Used to fill up the packet to 6 members
  FID := 0;
  FName := '';
  FChannel := -2;
  FJob := TMapleJob(0);
  FLevel := 0;
  FDoorTown := NO_MAP;
  FDoorTarget := NO_MAP;
  FDoorPosition := Point(0, 0);
end;

procedure TMaplePartyCharacter.Init(Char: TObject);
var
  Chr: TMapleCharacter;
begin
  Chr := TMapleCharacter(Char);
  FID := Chr.ID;
  FLevel := Chr.Level;
  FName := Chr.Name;
  FChannel := TMapleClient(Chr.Client).Channel.Index;
  FMapID := Chr.Map.ID;
  FJob := Chr.Job;
  FDoorTown := NO_MAP;
  FDoorTarget := NO_MAP;
  FDoorPosition := Point(0, 0);
end;

{ TMapleParty }

constructor TMapleParty.Create(const AID: Integer; ALeader: TMaplePartyCharacter);
begin
  FID := AID;
  FLeader := ALeader;
  FMembers := TList<TMaplePartyCharacter>.Create;
  FMembers.Add(FLeader);
end;

destructor TMapleParty.Destroy;
begin
  // xxx free chars?
  FMembers.Free;

  inherited;
end;

function TMapleParty.GetEnumerator: TList<TMaplePartyCharacter>.TEnumerator;
begin
  Result := FMembers.GetEnumerator;
end;

function TMapleParty.GetMember(ID: Integer): TMaplePartyCharacter;
begin
  for Result in FMembers do
    if Result.ID = ID then
      Exit;

  Result := nil;
end;

function TMapleParty.GetMember(Name: string): TMaplePartyCharacter;
begin
  for Result in FMembers do
    if SameText(Result.Name, Name) then
      Exit;

  Result := nil;
end;

end.
