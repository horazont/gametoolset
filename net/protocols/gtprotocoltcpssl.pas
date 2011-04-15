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

