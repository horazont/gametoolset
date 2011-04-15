(*******************************************************************************
** File Name: gtunixsockstream.pas
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
unit GTUnixSockStream;

{$mode objfpc}{$H+}
{$IFNDEF UNIX}
interface

implementation

end.
{$ELSE}

interface

uses
  Classes, SysUtils, ctypes, Sockets, BaseUnix;

const
  UNIX_PATH_MAX = 108;

type
  EGTUnixSockError = class (Exception);

  { TGTUnixSockStream }

  TGTUnixSockStream = class (TStream)
  public
    constructor Create(const ASocket: cint; AOwnsSocket: Boolean);
    destructor Destroy; override;
  private
    FOwnsSocket: Boolean;
  protected
    FSocket: cint;
  protected
    function GetPosition: Int64; override;
    function GetSize: Int64; override;
  public
    procedure Flush; virtual;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
       overload;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  { TGTUnixSockStreamFilesystem }

  TGTUnixSockStreamFilesystem = class (TGTUnixSockStream)
  public
    constructor Create(const AName: String; const Connect: Boolean);
  end;

  { TGTUnixSockStreamNamed }

  TGTUnixSockStreamNamed = class (TGTUnixSockStreamFilesystem)
  public
    constructor Create(const AName: String; const Connect: Boolean);
  end;

implementation

{ TGTUnixSockStream }

constructor TGTUnixSockStream.Create(const ASocket: cint; AOwnsSocket: Boolean);
begin
  inherited Create;
  FSocket := ASocket;
  FOwnsSocket := AOwnsSocket;
end;

destructor TGTUnixSockStream.Destroy;
begin
  if FOwnsSocket then
    fpshutdown(FSocket, SHUT_RDWR);
  inherited Destroy;
end;

function TGTUnixSockStream.GetPosition: Int64;
begin
  Exit(-1);
end;

function TGTUnixSockStream.GetSize: Int64;
begin
  Exit(-1);
end;

procedure TGTUnixSockStream.Flush;
begin
  fpsend(FSocket, nil, 0, MSG_EOR);
end;

function TGTUnixSockStream.Read(var Buffer; Count: Longint): Longint;
begin
  Exit(fprecv(FSocket, @Buffer, Count, MSG_WAITALL or MSG_NOSIGNAL));
end;

function TGTUnixSockStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := inherited Seek(Offset, Origin);
end;

function TGTUnixSockStream.Write(const Buffer; Count: Longint): Longint;
begin
  Exit(fpsend(FSocket, @Buffer, Count, MSG_NOSIGNAL));
end;

{ TGTUnixSockStreamFilesystem }

constructor TGTUnixSockStreamFilesystem.Create(const AName: String;
  const Connect: Boolean);
var
  sock, connection: cint;
  addr: sockaddr_un;
begin
  if Length(AName) > UNIX_PATH_MAX-1 then
    raise EGTUnixSockError.CreateFmt('Named socket name is too long (has %d, maximum is %d chars).', [Length(AName), UNIX_PATH_MAX-1]);
  sock := fpsocket(AF_UNIX, SOCK_STREAM, AF_UNIX);
  addr.sun_family := AF_UNIX;
  FillByte(addr.sun_path[0], UNIX_PATH_MAX, 0);
  Move(AName[1], addr.sun_path[0], Length(AName));
  if Connect then
  begin
    if fpconnect(sock, @addr, SizeOf(sockaddr_un)) <> 0 then
      raise EGTUnixSockError.CreateFmt('Could not connect to named unix socket ''%s''.', [AName]);
    inherited Create(sock, True);
  end
  else
  begin
    if fpbind(sock, @addr, SizeOf(sockaddr_un)) <> 0 then
      raise EGTUnixSockError.CreateFmt('Could not bind to named unix socket ''%s''.', [AName]);
    if fplisten(sock, 1) <> 0 then
      raise EGTUnixSockError.CreateFmt('Could not listen on named unix socket ''%s''.', [AName]);
    connection := fpaccept(sock, nil, nil);
    fpshutdown(sock, SHUT_RDWR);
    inherited Create(connection, True);
  end;
end;

{ TGTUnixSockStreamNamed }

constructor TGTUnixSockStreamNamed.Create(const AName: String;
  const Connect: Boolean);
var
  NulledName: String;
  L: Integer;
begin
  L := Length(AName);
  SetLength(NulledName, L + 1);
  NulledName[1] := #0;
  Move(AName[1], NulledName[2], L);
  inherited Create(NulledName, Connect);
end;

end.
{$ENDIF}
