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

unit Scheduler;

interface

uses Windows, SysUtils, Generics.Collections;

type
  TCallback = procedure of object;
  TRefCallback = reference to procedure;

  TTimerInfo = record
    Proc: TCallback;
    RefProc: TRefCallback;
    Timer, Queue: THandle;
  end;
  PTimerInfo = ^TTimerInfo;

  TScheduler = class
  private
    FQueue: THandle;
    FTimers: TDictionary<THandle, PTimerInfo>;
  public
    constructor Create;
    destructor Destroy; override;

    function AddSchedule(Milliseconds: Cardinal; Proc: TCallback): THandle; overload;
    function AddSchedule(Milliseconds: Cardinal; Proc: TRefCallback): THandle; overload;
    function AddRepeatedJob(Interval: Cardinal; Proc: TCallback): THandle;

    procedure CancelSchedule(Handle: THandle);
    procedure RemoveRepeatedJob(Handle: THandle);

    procedure TimerDone(Handle: THandle);
  end;

var
  Sched: TScheduler;    // singleton

implementation

uses Main;

{ TScheduler }

constructor TScheduler.Create;
begin
  FQueue := CreateTimerQueue;
  FTimers := TDictionary<THandle, PTimerInfo>.Create;
end;

destructor TScheduler.Destroy;
begin
  DeleteTimerQueueEx(FQueue, INVALID_HANDLE_VALUE);
  FTimers.Free;

  inherited;
end;

procedure OnSchedule(Context: PTimerInfo; Fired: Boolean); stdcall;
var
  E: Cardinal;
begin
  try
    if @Context^.Proc <> nil then
      Context^.Proc()
    else
      Context^.RefProc();

    if not DeleteTimerQueueTimer(Context^.Queue, Context^.Timer, 0) then
    begin
      E := GetLastError;
      if E <> ERROR_IO_PENDING then
        Log('Deleting a timer failed with %d', [E]);
    end;
  finally
    Sched.TimerDone(Context^.Timer);
  end;
end;

procedure OnRepeat(Context: PTimerInfo; Fired: Boolean); stdcall;
begin
  Context^.Proc();
end;

function TScheduler.AddSchedule(Milliseconds: Cardinal; Proc: TCallback): THandle;
var
  Timer: THandle;
  Info: PTimerInfo;
begin
  New(Info);
  Info^.Proc := Proc;
  Info^.RefProc := nil;
  Info^.Queue := FQueue;

  if not CreateTimerQueueTimer(Timer, FQueue, @OnSchedule, Info, Milliseconds, 0, WT_EXECUTEONLYONCE) then
    raise Exception.Create('Creating a timer (schedule) failed!');

  Info^.Timer := Timer;
  Result := Timer;
  FTimers.Add(Result, Info);
end;

function TScheduler.AddSchedule(Milliseconds: Cardinal; Proc: TRefCallback): THandle;
var
  Timer: THandle;
  Info: PTimerInfo;
begin
  New(Info);
  Info^.Proc := nil;
  Info^.RefProc := Proc;
  Info^.Queue := FQueue;

  if not CreateTimerQueueTimer(Timer, FQueue, @OnSchedule, Info, Milliseconds, 0, WT_EXECUTEONLYONCE) then
    raise Exception.Create('Creating a timer (schedule) failed!');

  Info^.Timer := Timer;
  Result := Timer;
  FTimers.Add(Result, Info);
end;

procedure TScheduler.CancelSchedule(Handle: THandle);
begin
  try
    if not DeleteTimerQueueTimer(FQueue, Handle, 0) then
    begin
      if GetLastError <> ERROR_IO_PENDING then
        raise Exception.Create('Cancelling a timer failed!')
    end
    else
      Log('Timer cancelled!');
  finally
    TimerDone(Handle);
  end;
end;

function TScheduler.AddRepeatedJob(Interval: Cardinal; Proc: TCallback): THandle;
var
  Info: PTimerInfo;
begin
  New(Info);
  Info^.Proc := Proc;

  if not CreateTimerQueueTimer(Result, FQueue, @OnRepeat, Info, Interval, Interval, WT_EXECUTEDEFAULT) then
    raise Exception.Create('Creating a timer (repeat) failed!');

  FTimers.AddOrSetValue(Result, Info);
end;

procedure TScheduler.RemoveRepeatedJob(Handle: THandle);
begin
  if not DeleteTimerQueueTimer(FQueue, Handle, 0) then
    raise Exception.Create('Deleting a timer (repeat) failed!');

  TimerDone(Handle);
end;

procedure TScheduler.TimerDone(Handle: THandle);
begin
  if not FTimers.ContainsKey(Handle) then
  begin
    Log('Could not dispose timer info');
    Exit;
  end;

  Dispose(FTimers[Handle]);   // Also free the record we allocated
  FTimers.Remove(Handle);
end;

// Singleton handling
initialization
  Sched := TScheduler.Create;

finalization
  Sched.Free;

end.
