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

