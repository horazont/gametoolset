unit GTConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, GTBase, FileUtil, GTPaths;

type
  EGTConfigError = class (EGTCoreError);

  { TGTConfig }

  TGTConfig = class (TGTBaseObject)
  public
    constructor Create; override;
  private
    FAutoLoadForbidden: Boolean;
  protected
    class function GetConfigFileName: String; virtual;
    procedure ForbidAutoLoad(Relieve: Boolean = False);
  public
    procedure AfterConstruction; override;
    class function GetConfigPath: String;
    procedure Load;
    procedure Save;
  public
    property ConfigPath: String read GetConfigPath;
  end;

implementation

{ TGTConfig }

constructor TGTConfig.Create;
begin
  inherited Create;
  FAutoLoadForbidden := False;
end;

class function TGTConfig.GetConfigFileName: String;
begin
  Result := 'config.xml';
end;

procedure TGTConfig.ForbidAutoLoad(Relieve: Boolean);
begin
  FAutoLoadForbidden := not Relieve;
end;

class function TGTConfig.GetConfigPath: String;
begin
  Result := Paths.GetXDGConfigPath + GetConfigFileName;
end;

procedure TGTConfig.AfterConstruction;
begin
  inherited AfterConstruction;
  if not FAutoLoadForbidden then
    Load;
end;

procedure TGTConfig.Load;
var
  FileName: String;
begin
  FileName := GetConfigPath;
  if FileExistsUTF8(FileName) then
    LoadFromFile(FileName);
end;

procedure TGTConfig.Save;
var
  FileName: String;
begin
  FileName := GetConfigPath;
  if FileExistsUTF8(FileName) then
    CopyFile(FileName, FileName + '~');
  SaveToFile(FileName);
end;

end.

