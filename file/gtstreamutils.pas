(*******************************************************************************
** File Name: gtstreamutils.pas
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
unit GTStreamUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  EGTStreamError = class (Exception);

procedure CheckRead(const AStream: TStream; var Buffer; Count: Longint);

implementation

procedure CheckRead(const AStream: TStream; var Buffer; Count: Longint);
begin
  if AStream.Read(Buffer, Count) < Count then
    raise EGTStreamError.Create('Stream read error.');
end;

end.

