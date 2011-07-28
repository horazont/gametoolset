(*******************************************************************************
** File Name: gtringbuffer.pas
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
unit GTRingBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs;

type

  { TGTRingBuffer }

  TGTRingBuffer = class (TStream)
  public
    constructor Create(const ASize: SizeUInt);
    destructor Destroy; override;
  private
    FAvailable: SizeUInt;
    FDataBuffer: Pointer;
    FDebug: Boolean;
    FLock: TCriticalSection;
    FReadOffset: SizeUInt;
    FSize: SizeUInt;
    FWriteOffset: SizeUInt;
    function GetAvailable: SizeUInt;
  protected
    function GetSize: Int64; override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    procedure Reset;
    function Write(const Buffer; Count: Longint): Longint; override;
  public
    property Available: SizeUInt read GetAvailable;
    property Debug: Boolean read FDebug write FDebug;
  end;

implementation

var
  TM: TThreadManager;
  TMInitialized: Boolean;

{ TGTRingBuffer }

constructor TGTRingBuffer.Create(const ASize: SizeUInt);
begin
  if not TMInitialized then
    GetThreadManager(TM);
  inherited Create;
  FLock := TCriticalSection.Create;
  FReadOffset := 0;
  FWriteOffset := 0;
  FSize := ASize;
  FDataBuffer := GetMem(ASize + 1);
  PByte(FDataBuffer)[ASize] := $FF;
  FillByte(FDataBuffer^, FSize, $00);
  FDebug := False;
end;

destructor TGTRingBuffer.Destroy;
begin
  FreeMem(FDataBuffer);
  FLock.Free;
  inherited Destroy;
end;

function TGTRingBuffer.GetAvailable: SizeUInt;
begin
  FLock.Acquire;
  Result := FAvailable;
  FLock.Release;
end;

function TGTRingBuffer.GetSize: Int64;
begin
  Result := FSize;
end;

function TGTRingBuffer.Read(var Buffer; Count: Longint): Longint;
var
  AWriteOffset, AReadOffset: SizeUInt;
  Target: Pointer;

  function ReadStep(ACount: Longint): Longint;
  begin
    if AReadOffset > AWriteOffset then
    begin
      if (FSize - AReadOffset) > ACount then
      begin
        Move((FDataBuffer + AReadOffset)^, Target^, ACount);
        Result := ACount;
        AReadOffset += ACount;
        if AReadOffset = FSize then
          AReadOffset := 0;
      end
      else
      begin
        Move((FDataBuffer + AReadOffset)^, Target^, (FSize - AReadOffset));
        Result := FSize - AReadOffset;
        AReadOffset := 0;
      end;
    end
    else if AWriteOffset > AReadOffset then
    begin
      if (AWriteOffset - AReadOffset) > ACount then
      begin
        Move((FDataBuffer + AReadOffset)^, Target^, ACount);
        Result := ACount;
        AReadOffset += ACount;
      end
      else
      begin
        Move((FDataBuffer + AReadOffset)^, Target^, (AWriteOffset - AReadOffset));
        Result := AWriteOffset - AReadOffset;
        AReadOffset := AWriteOffset;
      end;
    end
    else
      Result := 0;
    Target += Result;
  end;

var
  ReadThisTime: SizeUInt;
  I: SizeUInt;
begin
  {$ifdef DEBUG}
  if FDebug then
    WriteLn(Format('Attempt to read %d bytes.', [Count]));
  {$endif}
  FLock.Acquire;
  try
    AWriteOffset := FWriteOffset;
    AReadOffset := FReadOffset;
  finally
    FLock.Release;
  end;

  Target := @Buffer;
  FillByte(Target^, Count, $AA);
  Result := 0;
  while Result < Count do
  begin
    ReadThisTime := ReadStep(Count - Result);
    {$ifdef DEBUG}
    if FDebug and (ReadThisTime > 0) then
    begin
      WriteLn(Format('  Read %d bytes. checking for consistency.', [ReadThisTime]));
      Sleep(1);
      {for I := -ReadThisTime to -1 do
        if PByte(Target)[I] <> $00 then
        begin
          WriteLn(Format('  Byte %d of target buffer (at current offset %d, buffer size is %d) is tainted (%2.2x). Read was from %d to %d, size is %d.', [I, Result - ReadThisTime, Count, PByte(Target)[I], AReadOffset - ReadThisTime, AReadOffset, FSize]));
          ReadLn;
        end;}
    end;
    {$endif}
    Result += ReadThisTime;
    FLock.Acquire;
    AWriteOffset := FWriteOffset;
    FReadOffset := AReadOffset;
    FAvailable -= ReadThisTime;
    FLock.Release;
    ThreadSwitch;
  end;
  {$ifdef DEBUG}
  if FDebug then
    WriteLn(Format('Read done (%d bytes successfully read)', [Result]));
  {$endif}
end;

procedure TGTRingBuffer.Reset;
begin
  FLock.Acquire;
  try
    FReadOffset := 0;
    FWriteOffset := 0;
  finally
    FLock.Release;
  end;
end;

function TGTRingBuffer.Write(const Buffer; Count: Longint): Longint;
var
  AWriteOffset, AReadOffset: SizeUInt;
  Source: Pointer;

  function WriteStep(ACount: Longint): Longint;
  var
    MaxCount: SizeUInt;
  begin
    if (AWriteOffset = FSize) then
    begin
      if (AReadOffset = 0) then
        Exit(0)
      else
        AWriteOffset := 0;
    end;
    if AWriteOffset >= AReadOffset then
    begin
      if (FSize - AWriteOffset) > ACount then
      begin
        Move(Source^, (FDataBuffer + AWriteOffset)^, ACount);
        Result := ACount;
        AWriteOffset += ACount;
      end
      else
      begin
        Move(Source^, (FDataBuffer + AWriteOffset)^, (FSize - AWriteOffset));
        Result := (FSize - AWriteOffset);
        AWriteOffset := FSize;
      end;
    end
    else// if AReadOffset > AWriteOffset then
    begin
      MaxCount := (AReadOffset - AWriteOffset) - 1;
      if MaxCount > ACount then
      begin
        Move(Source^, (FDataBuffer + AWriteOffset)^, ACount);
        Result := ACount;
        AWriteOffset += ACount;
      end
      else
      begin
        Move(Source^, (FDataBuffer + AWriteOffset)^, MaxCount);
        Result := MaxCount;
        AWriteOffset += MaxCount;
      end;
    end;
    Source += Result;
  end;

var
  WroteThisTime: SizeUInt;
begin
  {$ifdef DEBUG}
  if FDebug then
    WriteLn(Format('Attempt to write %d bytes.', [Count]));
  {$endif}
  FLock.Acquire;
  try
    AWriteOffset := FWriteOffset;
    AReadOffset := FReadOffset;
  finally
    FLock.Release;
  end;

  Source := @Buffer;
  Result := 0;

  while Result < Count do
  begin
    WroteThisTime := WriteStep(Count - Result);
    Result += WroteThisTime;
    FLock.Acquire;
    {$ifdef DEBUG}
    if FDebug and (WroteThisTime > 0) then
    begin
      WriteLn(Format('  Wrote %d bytes. Old/New write ptr: %d/%d; Old/New read ptr: %d/%d', [WroteThisTime, FWriteOffset, AWriteOffset, FReadOffset, AReadOffset]));
//      Sleep(1);
    end;
    {$endif}
    FWriteOffset := AWriteOffset;
    AReadOffset := FReadOffset;
    FAvailable += WroteThisTime;
    FLock.Release;
    ThreadSwitch;
  end;
  {$ifdef DEBUG}
  if FDebug then
    WriteLn(Format('Write done (%d bytes successfully written)', [Result]));
  if FAvailable > FSize then
  begin
    WriteLn(Format('Overfull ringbuffer: %d/%d.', [FAvailable, FSize]));
  end;
  {$endif}
end;

end.

