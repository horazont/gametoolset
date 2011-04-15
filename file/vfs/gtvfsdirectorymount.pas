(*******************************************************************************
** File Name: gtvfsdirectorymount.pas
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
unit GTVFSDirectoryMount;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTVFS, GTURI, FileUtil;

type

  { TGTMountDirectory }

  TGTMountDirectory = class (TGTMount)
  public
    constructor Create(const AVFS: TGTVFS; const ARoot: String; const AReadOnly: Boolean = True);
  private
    FReadOnly: Boolean;
    FRoot: String;
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

{ TGTMountDirectory }

constructor TGTMountDirectory.Create(const AVFS: TGTVFS; const ARoot: String;
  const AReadOnly: Boolean);
begin
  inherited Create(AVFS);
  FRoot := IncludeTrailingPathDelimiter(ARoot);
  FReadOnly := AReadOnly;
end;

function TGTMountDirectory.GetCapabilities: TGTProtocolCapabilities;
begin
  if FReadOnly then
    Result := [pcRead, pcShareModes]
  else
    Result := [pcBidirectional, pcRead, pcWrite, pcShareModes];
end;

function TGTMountDirectory.CreateBidirectionalStream(const APath: String;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
begin
  if FReadOnly then
    raise EGTVFSMountError.CreateFmt('Cannot open ''%s'' in read-only mount as bidirectional stream.', [APath]);
  if not (WriteMode in [wmIgnore, wmAppend]) then
    raise EGTURIProtocolError.Create('Bidirectional file streams must use wmAppend write mode.');
  Result := TFileStream.Create(UTF8ToSys(FRoot + APath), fmOpenReadWrite or ShareModeToFlags(ShareMode));
end;

function TGTMountDirectory.CreateReadOnlyStream(const APath: String;
  const ShareMode: TGTStreamShareMode): TStream;
begin
  Result := TFileStream.Create(UTF8ToSys(FRoot + APath), fmOpenRead or ShareModeToFlags(ShareMode));
end;

function TGTMountDirectory.CreateWriteOnlyStream(const APath: String;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
begin
  if FReadOnly then
    raise EGTVFSMountError.CreateFmt('Cannot open ''%s'' in read-only mount as write stream.', [APath]);
  case WriteMode of
    wmOverwrite, wmIgnore:
      Result := TFileStream.Create(UTF8ToSys(FRoot + APath), fmCreate or ShareModeToFlags(ShareMode));
    wmAppend:
      Result := TFileStream.Create(UTF8ToSys(FRoot + APath), fmOpenWrite or ShareModeToFlags(ShareMode));
  end;
end;

function TGTMountDirectory.FileExists(const AFileName: String): Boolean;
begin
  Exit(FileExistsUTF8(FRoot + AFileName));
end;

function TGTMountDirectory.ToString: ansistring;
begin
  Exit('directory:'+FRoot);
end;

end.

