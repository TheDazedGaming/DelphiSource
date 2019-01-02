unit MapDebug;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, MapleMap, MapleMapObject;

type
  TfrmMapDebug = class(TForm)
    LVInfo: TListView;
    TmrUpdate: TTimer;
    procedure TmrUpdateTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
  public
    Map: TMapleMap;
  end;

var
  frmMapDebug: TfrmMapDebug;

implementation

uses MapleMonster;

{$R *.dfm}

procedure TfrmMapDebug.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TmrUpdate.Enabled := False;
end;

procedure TfrmMapDebug.FormShow(Sender: TObject);
begin
  TmrUpdate.Enabled := True;
end;

procedure TfrmMapDebug.TmrUpdateTimer(Sender: TObject);
var
  O: TMapleMapObject;
begin
  if Map = nil then
    Exit;

  with LVInfo.Items do
  begin
    BeginUpdate;
    try
      Clear;
      for O in Map.GetMapObjects do
        with Add do
        begin
          Caption := IntToStr(O.ObjectID);
          SubItems.Add(O.ClassName);
          if O is TLoadedLife then
            SubItems.Add(IntToStr(TLoadedLife(O).ID))
          else
            SubItems.Add('nil');
          SubItems.Add(IntToStr(O.Position.X));
          SubItems.Add(IntToStr(O.Position.Y));
          if O is TMapleMonster then
            if TMapleMonster(O).Controller = nil then
              SubItems.Add('nil')
            else
              SubItems.Add(TMapleMonster(O).Controller.Name)
        end;
    finally
      EndUpdate;
    end;
  end;
end;

end.
