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

unit DatabaseConnection;

interface

uses SysUtils, ZConnection, ZAbstractRODataset, ZAbstractDataset, ZDataset, DB;

type
  TDatabaseConnection = class
  private
    FCon: TZConnection;

    procedure Init(DBName: string);
  public
    constructor Create(DBName: string);
    destructor Destroy; override;

    function GetQuery: TZQuery;
    function GetLastInsertedID: Integer;

    property Con: TZConnection read FCon write FCon;
  end;

implementation

uses Settings;

{ TDatabaseConnection }

constructor TDatabaseConnection.Create(DBName: string);
begin
  Init(DBName);
end;

destructor TDatabaseConnection.Destroy;
begin
  // Close the connection
  FCon.Connected := False;
  FreeAndNil(FCon);

  inherited;
end;

procedure TDatabaseConnection.Init(DBName: string);
begin
  // Initialize the conenction with values from the settings
  FCon := TZConnection.Create(nil);
  FCon.Protocol := 'mysql-5';
  FCon.HostName := frmSettings.edtSQLHost.Text;
  FCon.Port := frmSettings.seSQLPort.Value;
  FCon.Catalog := DBName;
  FCon.Database := DBName;
  FCon.User := frmSettings.edtSQLUser.Text;
  FCon.Password := frmSettings.edtSQLPW.Text;

  FCon.Connected := True;
end;

function TDatabaseConnection.GetQuery: TZQuery;
begin
  Result := TZQuery.Create(nil);
  Result.Connection := FCon;
end;

function TDatabaseConnection.GetLastInsertedID: Integer;
var
  IDQuery: TZQuery;
begin
  try
    IDQuery := GetQuery;
    try
      IDQuery.SQL.Text := 'SELECT LAST_INSERT_ID()';
      IDQuery.Open;
      Result := IDQuery.FieldByName('LAST_INSERT_ID()').AsInteger;
      IDQuery.Close;
    finally
      FreeAndNil(IDQuery);
    end;
  except
    Result := -1;
  end;
end;

end.
