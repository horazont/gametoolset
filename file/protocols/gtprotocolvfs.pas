(*******************************************************************************
** File Name: gtprotocolvfs.pas
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
unit GTProtocolVFS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTURI, URIParser, GTVFS;

type

  { TGTProtocolVFS }

  TGTProtocolVFS = class (TGTProtocol)
  protected
    class procedure HostNotSupported;
  public
    VFS: TGTVFS; static;
  public
    class function CreateBidirectionalStream(const AURI: TURI;
       const WriteMode: TGTStreamWriteMode = wmOverwrite;
       const ShareMode: TGTStreamShareMode = smDontCare): TStream; override;
    class function CreateReadOnlyStream(const AURI: TURI;
       const ShareMode: TGTStreamShareMode = smDontCare): TStream; override;
    class function CreateWriteOnlyStream(const AURI: TURI;
       const WriteMode: TGTStreamWriteMode = wmOverwrite;
       const ShareMode: TGTStreamShareMode = smDontCare): TStream; override;
    class function GetCapabilities: TGTProtocolCapabilities; override;
    class function GetProtocolName: String; override;
  end;

implementation

{ TGTProtocolVFS }

class procedure TGTProtocolVFS.HostNotSupported;
begin
  raise EGTURIProtocolError.CreateFmt('Hosts are not supported for %s protocol.', [GetProtocolName]);
end;

class function TGTProtocolVFS.CreateBidirectionalStream(const AURI: TURI;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
begin
  if AURI.Host <> '' then
    HostNotSupported;
  Exit(VFS.CreateBidirectionalStream(AURI.Path + AURI.Document, WriteMode, ShareMode));
end;

class function TGTProtocolVFS.CreateReadOnlyStream(const AURI: TURI;
  const ShareMode: TGTStreamShareMode): TStream;
begin
  if AURI.Host <> '' then
    HostNotSupported;
  Exit(VFS.CreateReadOnlyStream(AURI.Path + AURI.Document, ShareMode));
end;

class function TGTProtocolVFS.CreateWriteOnlyStream(const AURI: TURI;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
begin
  if AURI.Host <> '' then
    HostNotSupported;
  Exit(VFS.CreateWriteOnlyStream(AURI.Path + AURI.Document, WriteMode, ShareMode));
end;

class function TGTProtocolVFS.GetCapabilities: TGTProtocolCapabilities;
begin
  Exit([pcRead, pcWrite, pcBidirectional, pcShareModes]);
end;

class function TGTProtocolVFS.GetProtocolName: String;
begin
  Exit('vfs');
end;

initialization
TGTProtocolVFS.VFS := nil;
AutoRegisterProtocol(TGTProtocolVFS);

finalization
FreeAndNil(TGTProtocolVFS.VFS);

end.

