(*******************************************************************************
** File Name: gturi.pas
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
unit GTURI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, URIParser, fgl, contnrs;

type
  EGTURIError = class (Exception);
  EGTURIProtocolError = class (EGTURIError);
  EGTURIStreamError = class (EGTURIError);

  TGTProtocolCapability = (pcBidirectional, pcRead, pcWrite, pcShareModes);
  TGTProtocolCapabilities = set of TGTProtocolCapability;

  TGTChainedProtocolCapability = (cpcRead, cpcWrite, cpcBidirectional);
  TGTChainedProtocolCapabilities = set of TGTChainedProtocolCapability;

  TGTStreamWriteMode = (wmAppend, wmOverwrite, wmIgnore);
  TGTStreamOpenMode = (omRead, omWrite, omBoth);
  TGTStreamShareMode = (smExclusive, smAllowRead, smAllowWrite, smAllowBoth, smDontCare);

  { TGTProtocol }

  TGTProtocol = class (TObject)
  private
    constructor Create;
  protected
    class procedure NoShareModesSupported;
  public
    class function CreateBidirectionalStream(const AURI: TURI;
      const WriteMode: TGTStreamWriteMode = wmOverwrite;
      const ShareMode: TGTStreamShareMode = smDontCare): TStream; virtual; abstract;
    class function CreateReadOnlyStream(const AURI: TURI;
      const ShareMode: TGTStreamShareMode = smDontCare): TStream; virtual;
    class function CreateWriteOnlyStream(const AURI: TURI;
      const WriteMode: TGTStreamWriteMode = wmOverwrite;
      const ShareMode: TGTStreamShareMode = smDontCare): TStream; virtual;
    class function GetCapabilities: TGTProtocolCapabilities; virtual;
    class function GetProtocolName: String; virtual; abstract;
  end;
  TGTProtocolClass = class of TGTProtocol;
  TGTProtocolClassList = specialize TFPGList<TGTProtocolClass>;

  { TGTURIStream }

  TGTURIStream = class (TStream)
  public
    constructor Create(const AURI: TURI; const OpenMode: TGTStreamOpenMode;
      const WriteMode: TGTStreamWriteMode = wmOverwrite;
      const ShareMode: TGTStreamShareMode = smDontCare);
    constructor Create(const AURI: String; const OpenMode: TGTStreamOpenMode;
      const WriteMode: TGTStreamWriteMode = wmOverwrite;
      const ShareMode: TGTStreamShareMode = smDontCare);
    destructor Destroy; override;
  private
    FChildStream: TStream;
  protected
    function GetPosition: Int64; override;
    function GetSize: Int64; override;
  public
    class function ActualStream(const AURI: TURI; const OpenMode: TGTStreamOpenMode;
      const WriteMode: TGTStreamWriteMode = wmOverwrite;
      const ShareMode: TGTStreamShareMode = smDontCare): TStream;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
       overload;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

var
  ProtocolAutoRegister: Boolean = False;

procedure AutoRegisterProtocol(const AProtocol: TGTProtocolClass);
procedure RegisterProtocol(const AProtocol: TGTProtocolClass);
function GetProtocol(const AProtocolName: String; const AFilter: TGTProtocolCapabilities = []): TGTProtocolClass;
function ShareModeToFlags(const AShareMode: TGTStreamShareMode): Integer;

implementation

var
  Protocols: TFPDataHashTable = nil;

procedure AutoRegisterProtocol(const AProtocol: TGTProtocolClass);
begin
  if not ProtocolAutoRegister then
    Exit;
  RegisterProtocol(AProtocol);
end;

procedure RegisterProtocol(const AProtocol: TGTProtocolClass);
begin
  Protocols.Add(AProtocol.GetProtocolName, AProtocol);
end;

function GetProtocol(const AProtocolName: String;
  const AFilter: TGTProtocolCapabilities): TGTProtocolClass;
var
  I: TGTProtocolCapability;
  Caps: TGTProtocolCapabilities;
begin
  Result := TGTProtocolClass(Protocols.Items[AProtocolName]);
  if AFilter <> [] then
  begin
    Caps := Result.GetCapabilities;
    for I := Low(TGTProtocolCapability) to High(TGTProtocolCapability) do
    begin
      if (I in AFilter) and (I in Caps) then
        Exit;
    end;
    Exit(nil);
  end;
end;

function ShareModeToFlags(const AShareMode: TGTStreamShareMode): Integer;
begin
  case AShareMode of
    smDontCare: Result := 0;
    smExclusive: Result := fmShareExclusive;
    smAllowBoth: Result := fmShareDenyNone;
    smAllowRead: Result := fmShareDenyWrite;
    smAllowWrite: Result := fmShareDenyRead;
  else
    Result := 0;
  end;
end;

{ TGTProtocol }

constructor TGTProtocol.Create;
begin

end;

class procedure TGTProtocol.NoShareModesSupported;
begin
  raise EGTURIProtocolError.CreateFmt('%s protocol implementation does not support share modes.', [GetProtocolName]);
end;

class function TGTProtocol.CreateReadOnlyStream(const AURI: TURI;
  const ShareMode: TGTStreamShareMode): TStream;
begin
  Exit(CreateBidirectionalStream(AURI, wmIgnore, ShareMode));
end;

class function TGTProtocol.CreateWriteOnlyStream(const AURI: TURI;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
begin
  Exit(CreateBidirectionalStream(AURI, WriteMode, ShareMode));
end;

class function TGTProtocol.GetCapabilities: TGTProtocolCapabilities;
begin
  Exit([]);
end;

{ TGTURIStream }

constructor TGTURIStream.Create(const AURI: TURI;
  const OpenMode: TGTStreamOpenMode; const WriteMode: TGTStreamWriteMode;
  const ShareMode: TGTStreamShareMode);
begin
  FChildStream := ActualStream(AURI, OpenMode, WriteMode, ShareMode);
  inherited Create;
end;

constructor TGTURIStream.Create(const AURI: String;
  const OpenMode: TGTStreamOpenMode; const WriteMode: TGTStreamWriteMode;
  const ShareMode: TGTStreamShareMode);
begin
  Create(ParseURI(AURI), OpenMode, WriteMode, ShareMode);
end;

destructor TGTURIStream.Destroy;
begin
  FChildStream.Free;
  inherited Destroy;
end;

function TGTURIStream.GetPosition: Int64;
begin
  Exit(FChildStream.Position);
end;

function TGTURIStream.GetSize: Int64;
begin
  Exit(FChildStream.Size);
end;

class function TGTURIStream.ActualStream(const AURI: TURI;
  const OpenMode: TGTStreamOpenMode; const WriteMode: TGTStreamWriteMode;
  const ShareMode: TGTStreamShareMode): TStream;
var
  Protocol: TGTProtocolClass;
  Caps: TGTProtocolCapabilities;
begin
  Protocol := GetProtocol(AURI.Protocol);
  if Protocol = nil then
    raise EGTURIProtocolError.CreateFmt('Protocol ''%s'' is not implemented.', [AURI.Protocol]);
  Caps := Protocol.GetCapabilities;
  case OpenMode of
    omRead:
    begin
      if not (pcRead in Caps) then
        raise EGTURIProtocolError.CreateFmt('%s protocol implementation does not support read streams.', [AURI.Protocol]);
      Result := Protocol.CreateReadOnlyStream(AURI, ShareMode);
    end;
    omWrite:
    begin
      if not (pcWrite in Caps) then
        raise EGTURIProtocolError.CreateFmt('%s protocol implementation does not support write streams.', [AURI.Protocol]);
      Result := Protocol.CreateWriteOnlyStream(AURI, WriteMode, ShareMode);
    end;
    omBoth:
    begin
      if not (pcBidirectional in Caps) then
        raise EGTURIProtocolError.CreateFmt('%s protocol implementation does not support bidirectional streams.', [AURI.Protocol]);
      Result := Protocol.CreateBidirectionalStream(AURI, WriteMode, ShareMode);
    end;
  else
    raise EGTURIError.CreateFmt('Invalid ordinal value for OpenMode: ''%d''.', [Ord(OpenMode)]);
  end;
end;

function TGTURIStream.Read(var Buffer; Count: Longint): Longint;
begin
  Exit(FChildStream.Read(Buffer, Count));
end;

function TGTURIStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Exit(FChildStream.Seek(Offset, Origin));
end;

function TGTURIStream.Write(const Buffer; Count: Longint): Longint;
begin
  Exit(FChildStream.Write(Buffer, Count));
end;

initialization
Protocols := TFPDataHashTable.CreateWith(97, @RSHash);

finalization
Protocols.Free;

end.

