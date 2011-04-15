unit GTProtocolFD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTURI, URIParser;

type
  { TGTProtocolFD }

  TGTProtocolFD = class (TGTProtocol)
  public
    class function CreateReadOnlyStream(const AURI: TURI;
       const ShareMode: TGTStreamShareMode = smDontCare): TStream; override;
    class function CreateWriteOnlyStream(const AURI: TURI;
       const WriteMode: TGTStreamWriteMode = wmOverwrite;
       const ShareMode: TGTStreamShareMode = smDontCare): TStream; override;
    class function GetCapabilities: TGTProtocolCapabilities; override;
    class function GetProtocolName: String; override;
  end;

implementation

{ TGTProtocolFD }

class function TGTProtocolFD.CreateReadOnlyStream(const AURI: TURI;
  const ShareMode: TGTStreamShareMode): TStream;
begin
  if ShareMode <> smDontCare then
    NoShareModesSupported;
  if AURI.Host = 'stdin' then
    Result := THandleStream.Create(GetFileHandle(Input))
  else
    raise EGTURIProtocolError.CreateFmt('fd protocol implementation does not support opening ''%s'' for reading.', [AURI.Host]);
end;

class function TGTProtocolFD.CreateWriteOnlyStream(const AURI: TURI;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
begin
  if ShareMode <> smDontCare then
    NoShareModesSupported;
  if AURI.Host = 'stdout' then
    Result := THandleStream.Create(GetFileHandle(Output))
  else if AURI.Host = 'stderr' then
    Result := THandleStream.Create(GetFileHandle(ErrOutput))
  else
    raise EGTURIProtocolError.CreateFmt('fd protocol implementation does not support opening ''%s'' for writing.', [AURI.Host]);
end;

class function TGTProtocolFD.GetCapabilities: TGTProtocolCapabilities;
begin
  Result := [pcRead, pcWrite];
end;

class function TGTProtocolFD.GetProtocolName: String;
begin
  Result := 'fd';
end;

initialization
AutoRegisterProtocol(TGTProtocolFD);

end.

