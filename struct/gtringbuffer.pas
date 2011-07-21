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
    FDataBuffer: Pointer;
    FLock: TCriticalSection;
    FReadOffset: SizeUInt;
    FSize: SizeUInt;
    FWriteOffset: SizeUInt;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
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
  FDataBuffer := GetMem(ASize);
  FReadOffset := 0;
  FWriteOffset := 0;
  FSize := ASize;
end;

destructor TGTRingBuffer.Destroy;
begin
  FreeMem(FDataBuffer);
  FLock.Free;
  inherited Destroy;
end;

function TGTRingBuffer.Read(var Buffer; Count: Longint): Longint;
var
  AWriteOffset, AReadOffset: SizeUInt;
  Target: Pointer;

  function ReadStep(Count: Longint): Longint;
  begin
    if AReadOffset > AWriteOffset then
    begin
      if (FSize - AReadOffset) >= Count then
      begin
        Move((FDataBuffer + AReadOffset)^, Target^, Count);
        Result := Count;
        AReadOffset += Count;
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
      if (AWriteOffset - AReadOffset) >= Count then
      begin
        Move((FDataBuffer + AReadOffset)^, Target^, Count);
        Result := Count;
        AReadOffset += Count;
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

begin
  FLock.Acquire;
  try
    AWriteOffset := FWriteOffset;
    AReadOffset := FReadOffset;
  finally
    FLock.Release;
  end;

  Target := @Buffer;
  Result := 0;
  while Result < Count do
  begin
    Result += ReadStep(Count - Result);
    FLock.Acquire;
    AWriteOffset := FWriteOffset;
    FReadOffset := AReadOffset;
    FLock.Release;
    ThreadSwitch;
  end;
end;

function TGTRingBuffer.Write(const Buffer; Count: Longint): Longint;
var
  AWriteOffset, AReadOffset: SizeUInt;
  Source: Pointer;

  function WriteStep(Count: Longint): Longint;
  var
    MaxCount: SizeUInt;
  begin
    if AWriteOffset >= AReadOffset then
    begin
      if (FSize - AWriteOffset) >= Count then
      begin
        Move(Source^, (FDataBuffer + AWriteOffset)^, Count);
        Result := Count;
        AWriteOffset += Count;
        if AWriteOffset = FSize then
          AWriteOffset := 0;
      end
      else
      begin
        Move(Source^, (FDataBuffer + AWriteOffset)^, (FSize - AWriteOffset));
        Result := (FSize - AWriteOffset);
        AWriteOffset := 0;
      end;
    end
    else
    begin
      MaxCount := (AReadOffset - AWriteOffset) - 1;
      if MaxCount >= Count then
      begin
        Move(Source^, (FDataBuffer + AWriteOffset)^, Count);
        Result := Count;
        AWriteOffset += Count;
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

begin
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
    Result += WriteStep(Count - Result);
    FLock.Acquire;
    FWriteOffset := AWriteOffset;
    AReadOffset := FReadOffset;
    FLock.Release;
    ThreadSwitch;
  end;
end;

end.

