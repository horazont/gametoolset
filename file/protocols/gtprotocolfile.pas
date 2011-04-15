unit GTProtocolFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTURI, URIParser;

type

  { TGTProtocolFile }

  TGTProtocolFile = class (TGTProtocol)
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
    class function URIToFileName(const AURI: TURI): String;
  end;

implementation

{ TGTProtocolFile }

class function TGTProtocolFile.CreateBidirectionalStream(const AURI: TURI;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
begin
  if not (WriteMode in [wmIgnore, wmAppend]) then
    raise EGTURIProtocolError.Create('Bidirectional file streams must use wmAppend write mode.');
  Result := TFileStream.Create(URIToFileName(AURI), fmOpenReadWrite or ShareModeToFlags(ShareMode));
end;

class function TGTProtocolFile.CreateReadOnlyStream(const AURI: TURI;
  const ShareMode: TGTStreamShareMode): TStream;
begin
  Result := TFileStream.Create(URIToFileName(AURI), fmOpenRead or ShareModeToFlags(ShareMode));
end;

class function TGTProtocolFile.CreateWriteOnlyStream(const AURI: TURI;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
begin
  case WriteMode of
    wmOverwrite, wmIgnore:
      Result := TFileStream.Create(URIToFileName(AURI), fmCreate or ShareModeToFlags(ShareMode));
    wmAppend:
      Result := TFileStream.Create(URIToFileName(AURI), fmOpenWrite or ShareModeToFlags(ShareMode));
  end;
end;

class function TGTProtocolFile.GetCapabilities: TGTProtocolCapabilities;
begin
  Exit([pcRead, pcWrite, pcBidirectional]);
end;

class function TGTProtocolFile.GetProtocolName: String;
begin
  Result := 'file';
end;

class function TGTProtocolFile.URIToFileName(const AURI: TURI): String;
begin
  if (Length(AURI.Path) > 2) and (AURI.Path[1] = '/') and (AURI.Path[3] = ':') then
    Result := Copy(AURI.Path, 2, Length(AURI.Path)-1)
  else
    Result := AURI.Path;
  Result := Result + AURI.Document;
end;

initialization
AutoRegisterProtocol(TGTProtocolFile);

end.

