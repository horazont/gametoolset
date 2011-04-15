(*******************************************************************************
** File Name: gtsynapsestreamwrapper.pas
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
unit GTSynapseStreamWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock;

type

  { TSynapseStream }

  TSynapseStream = class (TStream)
  public
    constructor Create(const ASocket: TBlockSocket; AOwnsSocket: Boolean);
    destructor Destroy; override;
  private
    FOwnsSocket: Boolean;
    FSocket: TBlockSocket;
  protected
    function GetPosition: Int64; override;
    function GetSize: Int64; override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  public
    property Socket: TBlockSocket read FSocket;
  end;

implementation

{ TSynapseStream }

constructor TSynapseStream.Create(const ASocket: TBlockSocket;
  AOwnsSocket: Boolean);
begin
  inherited Create;
  FSocket := ASocket;
  FOwnsSocket := AOwnsSocket;
end;

destructor TSynapseStream.Destroy;
begin
  if FOwnsSocket then
  begin
    FSocket.CloseSocket;
    FSocket.Free;
  end;
  inherited Destroy;
end;

function TSynapseStream.GetPosition: Int64;
begin
  Exit(-1);
end;

function TSynapseStream.GetSize: Int64;
begin
  Exit(-1);
end;

function TSynapseStream.Read(var Buffer; Count: Longint): Longint;
begin
  Exit(FSocket.RecvBufferEx(@Buffer, Count, -1));
end;

function TSynapseStream.Write(const Buffer; Count: Longint): Longint;
begin
  Exit(FSocket.SendBuffer(@Buffer, Count));
end;

end.

