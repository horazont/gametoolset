(*******************************************************************************
** File Name: gtthreadfifo.pas
This file is part of the Game Toolset Package.

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms
of the GNU General Public license (the  "GPL License"), in which case the
provisions of GPL License are applicable instead of those
above.

For feedback and questions about Game Toolset please mail me,
Jonas Wielicki:
  j.wielicki@sotecware.net
*******************************************************************************)
unit GTThreadFIFO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils{$ifdef Linux}, ctypes{$endif};

type
  PGTThreadFIFOElement = ^TGTThreadFIFOElement;
  TGTThreadFIFOElement = record
    Up: PGTThreadFIFOElement;
    Data: Pointer;
  end;

  TGTAsyncMethod = procedure (Data: Pointer) of object;
  TGTAsyncMethodElement = record
    Method: TGTAsyncMethod;
    Data: Pointer;
  end;
  PGTAsyncMethodElement = ^TGTAsyncMethodElement;

  { TGTCustomThreadFIFO }

  TGTCustomThreadFIFO = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FAvailableSemaphore: Pointer;
    FBottomElement: PGTThreadFIFOElement;
    FEmpty: Boolean;
    FLock: TRTLCriticalSection;
    FTag: Integer;
    FTM: TThreadManager;
    FTopElement: PGTThreadFIFOElement;
    FOnPush: TNotifyEvent;
  private
    procedure DebugMsg(const S: String);
    procedure DebugSemaphore;
  protected
    property OnPush: TNotifyEvent read FOnPush write FOnPush;
    property Tag: Integer read FTag write FTag;
  protected
    procedure DoOnPush; virtual;
    procedure FreeItem(AData: Pointer); virtual;
  public
    procedure Clear;
    function Empty: Boolean;
    function Pop: Pointer;
    function PopBlocking: Pointer;
    procedure Push(AData: Pointer);
    procedure WaitData;
  public
    property AvailableSemaphore: Pointer read FAvailableSemaphore;
  end;

  TGTThreadFIFO = class (TGTCustomThreadFIFO)
  public
    property Tag;
  end;

  { TGTAsyncMethods }

  TGTAsyncMethods = class (TGTCustomThreadFIFO)
  protected
    procedure FreeItem(AData: Pointer); override;
  public
    procedure ProcessAll;
    procedure Push(AMethod: TGTAsyncMethod; Data: Pointer);
  end;

implementation

{$ifdef Linux}
function sem_trywait(sem: pointer): cint; external 'rt';
{$endif}

{ TGTCustomThreadFIFO }

constructor TGTCustomThreadFIFO.Create;
begin
  GetThreadManager(FTM);
  FTag := 0;
  FEmpty := True;
  FBottomElement := nil;
  FTopElement := nil;
  FAvailableSemaphore := FTM.SemaphoreInit();
  DebugMsg('create');
  DebugSemaphore;
  InitCriticalSection(FLock);
end;

destructor TGTCustomThreadFIFO.Destroy;
var
  Element, NewElement: PGTThreadFIFOElement;
begin
  EnterCriticalsection(FLock);
  try
    Element := FBottomElement;
    while (Element <> nil) do
    begin
      NewElement := Element^.Up;
      FreeItem(Element^.Data);
      FreeMem(Element);
      Element := NewElement;
    end;
    FBottomElement := nil;
    FTopElement := nil;
    WriteLn('Semaphore deleted');
    FTM.SemaphoreDestroy(FAvailableSemaphore);
    DebugSemaphore;
  finally
    inherited Destroy;
    DoneCriticalsection(FLock);
  end;
end;

procedure TGTCustomThreadFIFO.DebugMsg(const S: String);
begin
  //WriteLn(Format('[tid=0x%16.16x] [0x%16.16x] [%s] %s', [ptrint(GetThreadID), Ptrint(Self), ClassName, S]));
end;

procedure TGTCustomThreadFIFO.DebugSemaphore;
begin
  //DebugMsg(Format('FAvailableSemaphore = @$%16.16x', [ptrint(FAvailableSemaphore)]));
end;

procedure TGTCustomThreadFIFO.DoOnPush;
begin
  if FOnPush <> nil then
    FOnPush(Self);
end;

procedure TGTCustomThreadFIFO.FreeItem(AData: Pointer);
begin

end;

procedure TGTCustomThreadFIFO.Clear;
var
  Element, NewElement: PGTThreadFIFOElement;
begin
  EnterCriticalSection(FLock);
  try
    Element := FBottomElement;
    while (Element <> nil) do
    begin
      NewElement := Element^.Up;
      FreeItem(Element^.Data);
      FreeMem(Element);
      Element := NewElement;
    end;
    FBottomElement := nil;
    FTopElement := nil;
    FEmpty := True;
    DebugMsg('clear');
    DebugSemaphore;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TGTCustomThreadFIFO.Empty: Boolean;
begin
  Result := FEmpty;
end;

function TGTCustomThreadFIFO.Pop: Pointer;
var
  Element: PGTThreadFIFOElement;
begin
  {$ifdef Linux}
  if sem_trywait(FAvailableSemaphore) <> 0 then
    Exit(nil);
  {$else}
  {$WARNING This will deadlock if a concurrent thread using PopBlocking gets the last element in the fifo}
  {On linux platforms, this is avoided by the usage of sem_trywait. Suggestions
   to solve this on other platforms are appreciated.}
  {make sure not to use PopBlocking and Pop on the same fifo on non-linux
   platforms for now.}
  {$endif}
  EnterCriticalSection(FLock);
  if FBottomElement = nil then
  begin
    Result := nil;
    LeaveCriticalSection(FLock);
    Exit;
  end;
  try
    Result := FBottomElement^.Data;
    {$ifndef Linux}
    FTM.SemaphoreWait(FAvailableSemaphore);
    {$endif}
    // we need to acquire the top lock here as FBottomElement is actually
    // FTopElement if the following if evaluates to true
    EnterCriticalSection(FLock);
    if FBottomElement^.Up = nil then
    begin
      FTopElement := nil;
      FEmpty := True;
    end;
    LeaveCriticalSection(FLock);
    Element := FBottomElement;
    FBottomElement := FBottomElement^.Up;
    FreeMem(Element);
    DebugMsg('pop');
    DebugSemaphore;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TGTCustomThreadFIFO.PopBlocking: Pointer;
var
  Element: PGTThreadFIFOElement;
begin
  FTM.SemaphoreWait(FAvailableSemaphore);
  EnterCriticalSection(FLock);
  if FBottomElement = nil then
  begin
    Result := nil;
    LeaveCriticalSection(FLock);
    Exit;
  end;
  try
    Result := FBottomElement^.Data;
    if FBottomElement^.Up = nil then
    begin
      FTopElement := nil;
      FEmpty := True;
    end;
    Element := FBottomElement;
    FBottomElement := FBottomElement^.Up;
    FreeMem(Element);
    DebugMsg('pop blocking');
    DebugSemaphore;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TGTCustomThreadFIFO.Push(AData: Pointer);
var
  NewElement: PGTThreadFIFOElement;
begin
  NewElement := GetMem(SizeOf(TGTThreadFIFOElement));
  NewElement^.Up := nil;
  NewElement^.Data := AData;
  EnterCriticalSection(FLock);
  try
    if FTopElement = nil then
    begin
      FBottomElement := NewElement;
      FEmpty := False;
    end
    else
      FTopElement^.Up := NewElement;
    FTopElement := NewElement;
    DebugMsg('push');
    DebugSemaphore;
    FTM.SemaphorePost(FAvailableSemaphore);
  finally
    LeaveCriticalSection(FLock);
  end;
  DoOnPush;
end;

procedure TGTCustomThreadFIFO.WaitData;
begin
  // wait for it and repost to allow popping
  FTM.SemaphoreWait(FAvailableSemaphore);
  FTM.SemaphorePost(FAvailableSemaphore);
end;

{ TGTAsyncMethods }

procedure TGTAsyncMethods.FreeItem(AData: Pointer);
begin
  FreeMem(AData);
end;

procedure TGTAsyncMethods.ProcessAll;
var
  List: TFPList;
  I: Integer;
  CurrElement, NextElement: PGTThreadFIFOElement;
  CurrCall: PGTAsyncMethodElement;
begin
  List := TFPList.Create;
  try
    EnterCriticalSection(FLock);
    try
      // First, save all entries to a list so we do not have to wait for some
      // expensive calls while we have the list locked.
      CurrElement := FBottomElement;
      if CurrElement = nil then
        Exit;
      while CurrElement <> nil do
      begin
        List.Add(CurrElement^.Data);
        NextElement := CurrElement^.Up;
        FreeMem(CurrElement);
        CurrElement := NextElement;
      end;
      FTopElement := nil;
      FBottomElement := nil;
    finally
      LeaveCriticalSection(FLock);
    end;
    for I := 0 to List.Count - 1 do
    begin
      CurrCall := PGTAsyncMethodElement(List[I]);
      CurrCall^.Method(CurrCall^.Data);
      FreeMem(CurrCall);
    end;
  finally
    List.Free;
  end;
end;

procedure TGTAsyncMethods.Push(AMethod: TGTAsyncMethod; Data: Pointer);
var
  Element: PGTAsyncMethodElement;
begin
  GetMem(Element, SizeOf(TGTAsyncMethodElement));
  Element^.Data := Data;
  Element^.Method := AMethod;
  inherited Push(Element);
end;

end.

