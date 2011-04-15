(*******************************************************************************
** File Name: gtprotocoltcpssl.pas
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

For feedback and questions about Thorium Scripting Language please mail me,
Jonas Wielicki:
  j.wielicki@sotecware.net
*******************************************************************************)
unit GTProtocolTCPSSL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTSynapseStreamWrapper, GTURI, URIParser, blcksock,
  ssl_openssl, GTProtocolTCP;

type
  { TGTProtocolTCPSSL }

  TGTProtocolTCPSSL = class (TGTProtocolTCP)
  public
    class function CreateBidirectionalStream(const AURI: TURI;
       const WriteMode: TGTStreamWriteMode = wmOverwrite;
       const ShareMode: TGTStreamShareMode = smDontCare): TStream; override;
    class function GetProtocolName: String; override;
  end;

implementation

{ TGTProtocolTCPSSL }

class function TGTProtocolTCPSSL.CreateBidirectionalStream(const AURI: TURI;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
var
  Socket: TTCPBlockSocket;
begin
  if ShareMode <> smDontCare then
    NoShareModesSupported;
  if not (WriteMode in [wmAppend, wmIgnore]) then
    raise EGTURIProtocolError.Create('tcp protocol implementation does only support wmAppend write mode.');
  Socket := TTCPBlockSocket.CreateWithSSL(TSSLOpenSSL);
  Socket.RaiseExcept := True;
  Socket.Connect(AURI.Host, IntToStr(AURI.Port));
  Result := TSynapseStream.Create(Socket, True);
end;

class function TGTProtocolTCPSSL.GetProtocolName: String;
begin
  Result := 'tcp+ssl';
end;

initialization
AutoRegisterProtocol(TGTProtocolTCPSSL);

end.

