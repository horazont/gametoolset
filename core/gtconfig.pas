(*******************************************************************************
** File Name: gtconfig.pas
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
    class function GetConfigPath: String; virtual;
    procedure Load; virtual;
    procedure Save; virtual;
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

