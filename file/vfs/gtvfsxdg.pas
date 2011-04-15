(*******************************************************************************
** File Name: gtvfsxdg.pas
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

For feedback and questions about Thorium Scripting Language please mail me,
Jonas Wielicki:
  j.wielicki@sotecware.net
*******************************************************************************)
unit GTVFSXDG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTVFS, GTURI, GTPaths, GTVFSDirectoryMount, GTVFSChRoot;

type

  { TGTXDGVFS }

  TGTXDGVFS = class (TGTVFS)
  public
    constructor Create;
  end;

implementation

{ TGTXDGVFS }

constructor TGTXDGVFS.Create;
var
  PathList: TStringList;
  Path: String;
begin
  inherited Create;
  PathList := TStringList.Create;
  try
    Paths.GetXDGReadOnlyDataPaths(PathList, False);
    for Path in PathList do
    begin
      AddMount(TGTMountDirectory.Create(Self, Path, True), '/data/', fpFallback);
    end;
    AddMount(TGTMountDirectory.Create(Self, Paths.GetXDGDataPath, False), '/data/', fpFileSystem);

    PathList.Clear;
    Paths.GetXDGReadOnlyConfigPaths(PathList, False);
    for Path in PathList do
    begin
      AddMount(TGTMountDirectory.Create(Self, Path, True), '/config/', fpFallback);
    end;
    AddMount(TGTMountDirectory.Create(Self, Paths.GetXDGConfigPath, False), '/config/', fpFileSystem);
  finally
    PathList.Free;
  end;
end;

end.

