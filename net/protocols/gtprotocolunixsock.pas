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

