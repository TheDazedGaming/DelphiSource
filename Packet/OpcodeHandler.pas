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

unit OpcodeHandler;

interface

uses SysUtils, Classes, IniFiles, Generics.Collections, Utils;

type
  TOpcodeHandler = class
  private
    FIniFile: string;
    FReceiveOps: TDictionary<Integer, string>;     // Opcode: Integer; Handlername: string
    FSendOps: TDictionary<string, Integer>;

    procedure ReadIni;

    function GetSendOp(Name: string): Integer;
  public
    constructor Create(IniFileName: string);
    destructor Destroy; override;

    property ReceiveOps: TDictionary<Integer, string> read FReceiveOps;
    property SendOps[Name: string]: Integer read GetSendOp;
  end;

implementation

{ TOpcodeHandler }

constructor TOpcodeHandler.Create(IniFileName: string);
begin
  if not FileExists(IniFileName) then
    raise Exception.Create('Opcode.ini file is missing!');

  FIniFile := IniFileName;
  FReceiveOps := TDictionary<Integer, string>.Create;
  FSendOps := TDictionary<string, Integer>.Create;

  ReadIni;
end;

destructor TOpcodeHandler.Destroy;
begin
  FreeAndNil(FReceiveOps);
  FreeAndNil(FSendOps);

  inherited;
end;

procedure TOpcodeHandler.ReadIni;
var
  Ini: TIniFile;
  SL: TStringList;
  i: Integer;
  Split: TArrayofstring;
begin
  Ini := TIniFile.Create(FIniFile);
  SL := TStringList.Create;
  try
    Ini.ReadSectionValues('Receive', SL);

    for i := 0 to SL.Count - 1 do
    begin
      Split := Explode('=', SL[i]);
      FReceiveOps.Add(StrToInt(Split[1]), Split[0]);
      Split := nil;
    end;

    SL.Clear;
    Ini.ReadSectionValues('Send', SL);
    for i := 0 to SL.Count - 1 do
    begin
      Split := Explode('=', SL[i]);
      FSendOps.Add(Split[0], StrToInt(Split[1]));
      Split := nil;
    end;
  finally
    FreeAndNil(Ini);
    FreeAndNil(SL);
  end;
end;

function TOpcodeHandler.GetSendOp(Name: string): Integer;
begin
  // Just so that you don't get the normal exception "Entry not found", but exactly know which opcode is missing
  if not FSendOps.ContainsKey(Name) then
    raise Exception.CreateFmt('No such Send-Opcode: %s', [Name]);

  Result := FSendOps[Name];
end;

end.
