unit GTProtocolTCP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTSynapseStreamWrapper, GTURI, URIParser, blcksock;

type

  { TGTProtocolTCP }

  TGTProtocolTCP = class (TGTProtocol)
  public
    class function CreateBidirectionalStream(const AURI: TURI;
       const WriteMode: TGTStreamWriteMode = wmOverwrite;
       const ShareMode: TGTStreamShareMode = smDontCare): TStream; override;
    class function GetCapabilities: TGTProtocolCapabilities; override;
    class function GetProtocolName: String; override;
  end;

implementation

{ TGTProtocolTCP }

class function TGTProtocolTCP.CreateBidirectionalStream(const AURI: TURI;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
var
  Socket: TTCPBlockSocket;
begin
  if ShareMode <> smDontCare then
    NoShareModesSupported;
  if not (WriteMode in [wmAppend, wmIgnore]) then
    raise EGTURIProtocolError.Create('tcp protocol implementation does only support wmAppend write mode.');
  Socket := TTCPBlockSocket.Create;
  Socket.RaiseExcept := True;
  Socket.Connect(AURI.Host, IntToStr(AURI.Port));
  Result := TSynapseStream.Create(Socket, True);
end;

class function TGTProtocolTCP.GetCapabilities: TGTProtocolCapabilities;
begin
  Result := [pcRead, pcWrite, pcBidirectional];
end;

class function TGTProtocolTCP.GetProtocolName: String;
begin
  Result := 'tcp';
end;

initialization
AutoRegisterProtocol(TGTProtocolTCP);

end.

