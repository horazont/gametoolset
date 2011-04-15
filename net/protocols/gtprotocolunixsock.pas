(*******************************************************************************
** File Name: gtprotocolunixsock.pas
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
unit GTProtocolUnixSock;

{$mode objfpc}{$H+}

{$IFNDEF UNIX}

interface

implementation

end.
{$ELSE}

interface

uses
  Classes, SysUtils, GTURI, URIParser, Sockets, GTUnixSockStream,
  GTProtocolFile;

type

  { TGTProtocolUnixSock }

  TGTProtocolUnixSock = class (TGTProtocol)
  public
    class function CreateBidirectionalStream(const AURI: TURI;
       const WriteMode: TGTStreamWriteMode = wmOverwrite;
       const ShareMode: TGTStreamShareMode = smDontCare): TStream; override;
    class function GetCapabilities: TGTProtocolCapabilities; override;
    class function GetProtocolName: String; override;
  end;

implementation

{ TGTProtocolUnixSock }

class function TGTProtocolUnixSock.CreateBidirectionalStream(const AURI: TURI;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
begin
  if ShareMode <> smDontCare then
    NoShareModesSupported;
  Result := TGTUnixSockStreamFilesystem.Create(TGTProtocolFile.URIToFileName(AURI), True);
end;

class function TGTProtocolUnixSock.GetCapabilities: TGTProtocolCapabilities;
begin
  Result := [pcRead, pcWrite, pcBidirectional];
end;

class function TGTProtocolUnixSock.GetProtocolName: String;
begin
  Result := 'unix';
end;

initialization
AutoRegisterProtocol(TGTProtocolUnixSock);

end.
{$ENDIF}

