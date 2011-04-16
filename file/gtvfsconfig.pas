unit GTVFSConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTConfig, GTURI, GTProtocolVFS;

type

  { TGTVFSConfig }

  TGTVFSConfig = class (TGTConfig)
  public
    class function GetConfigPath: String; override;
    procedure Load; override;
    procedure Save; override;
  end;

implementation

{ TGTVFSConfig }

class function TGTVFSConfig.GetConfigPath: String;
begin
  Exit('vfs:///config/' + GetConfigFileName);
end;

procedure TGTVFSConfig.Load;
var
  URL: String;
begin
  URL := GetConfigPath;
  if TGTProtocolVFS.VFS.FileExists(URL) then
    LoadFromURL(URL);
end;

procedure TGTVFSConfig.Save;
begin
  SaveToURL(GetConfigPath);
end;

end.

