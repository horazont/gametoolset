(*******************************************************************************
** File Name: gtdebug.pas
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
unit GTDebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure DebugMsg(const Fmt: String; Args: array of const; Obj: TObject = nil);

implementation

var
  NullTime: TDateTime;

function GetMSecs: QWord;
begin
  Result := Round((Now - NullTime) * 24 * 60 * 60 * 1000);
end;

procedure DebugMsg(const Fmt: String; Args: array of const; Obj: TObject);
var
  Fmtd: String;
begin
  Fmtd := Format(Fmt, Args);
  if Obj <> nil then
    WriteLn(Format('[0x%12.12x] [tid=0x%16.16x] [0x%16.16x (%s)] %s', [GetMSecs, int64(GetThreadID), int64(Obj), Obj.ToString, Fmtd]))
  else
    WriteLn(Format('[0x%12.12x] [tid=0x%16.16x] [nil] %s', [GetMSecs, int64(GetThreadID), Fmtd]));
end;

initialization
NullTime := Now;

end.

