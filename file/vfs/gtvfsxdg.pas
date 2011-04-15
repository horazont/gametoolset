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

