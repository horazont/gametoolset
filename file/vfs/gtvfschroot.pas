unit GTVFSChRoot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTVFS, GTURI;

type

  { TGTVFSChRoot }

  TGTVFSChRoot = class (TGTMount)
  public
    constructor Create(const AMount: TGTMount; const AChRoot: String);
  private
    FChRoot: String;
    FMount: TGTMount;
    FRootLen: Integer;
  protected
    function GetCapabilities: TGTProtocolCapabilities; override;
  public
    function CreateBidirectionalStream(const APath: String;
       const WriteMode: TGTStreamWriteMode = wmOverwrite;
       const ShareMode: TGTStreamShareMode = smDontCare): TStream; override;
    function CreateReadOnlyStream(const APath: String;
       const ShareMode: TGTStreamShareMode = smDontCare): TStream; override;
    function CreateWriteOnlyStream(const APath: String;
       const WriteMode: TGTStreamWriteMode = wmOverwrite;
       const ShareMode: TGTStreamShareMode = smDontCare): TStream; override;
    function FileExists(const AFileName: String): Boolean; override;
    function ToString: ansistring; override;
  end;

implementation

{ TGTVFSChRoot }

constructor TGTVFSChRoot.Create(const AMount: TGTMount; const AChRoot: String);
begin
  inherited Create(AMount.VFS);
  FMount := AMount;
  FChRoot := IncludeTrailingPathDelimiter(AChRoot);
  FRootLen := Length(FChRoot);
end;

function TGTVFSChRoot.GetCapabilities: TGTProtocolCapabilities;
begin
  Result := FMount.Capabilities;
end;

function TGTVFSChRoot.CreateBidirectionalStream(const APath: String;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
begin
  Result := FMount.CreateBidirectionalStream(Copy(APath, FRootLen-1, Length(APath)), WriteMode, ShareMode);
end;

function TGTVFSChRoot.CreateReadOnlyStream(const APath: String;
  const ShareMode: TGTStreamShareMode): TStream;
begin
  Result := FMount.CreateReadOnlyStream(Copy(APath, FRootLen-1, Length(APath)), ShareMode);
end;

function TGTVFSChRoot.CreateWriteOnlyStream(const APath: String;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
begin
  Result := FMount.CreateWriteOnlyStream(Copy(APath, FRootLen-1, Length(APath)), WriteMode, ShareMode);
end;

function TGTVFSChRoot.FileExists(const AFileName: String): Boolean;
begin
  if (Copy(AFileName, 1, FRootLen) = FChRoot) then
    Exit(FMount.FileExists(Copy(AFileName, FRootLen-1, Length(AFileName))));
end;

function TGTVFSChRoot.ToString: ansistring;
begin
  Exit('mount '''+FMount.ToString+''' at '+FChRoot);
end;

end.

