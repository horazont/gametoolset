(*******************************************************************************
** File Name: gtpaths.pas
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
unit GTPaths;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

type
  EGTPathError = class (Exception);

  { TGTPaths }

  TGTPaths = class (TObject)
  private
    constructor Create;
  private
    FConfigPath: String; static;
    FDataPath: String; static;
    FReadOnlyDataPath: String; static;
  protected
    class function CreateXDGPath(const MiddlePart: String; const FallbackDotName: String; const EnvName: String): String;
    class procedure ListXDGPaths(const AppNamePart: String; const FallbackGlobal,
      FallbackDotName: String; const EnvNameGlobal, EnvNameLocal: String; const AList: TStrings;
      const IncludeHome: Boolean = True);
  public
    class function GetAppName: String; virtual; abstract;
    class function GetXDGConfigDirName: String; virtual;
    class function GetXDGDataDirName: String; virtual;
    class function GetXDGConfigPath: String;
    class function GetXDGDataPath: String;
    class function GetXDGReadOnlyDataPath: String;
    class procedure GetXDGReadOnlyDataPaths(const AList: TStrings; const IncludeHome: Boolean = True);
    class procedure GetXDGReadOnlyConfigPaths(const AList: TStrings; const IncludeHome: Boolean = True);
  end;
  TGTPathsClass = class of TGTPaths;

var
  Paths: TGTPathsClass;

implementation

{ TGTPaths }

constructor TGTPaths.Create;
begin

end;

class function TGTPaths.CreateXDGPath(const MiddlePart: String;
  const FallbackDotName: String; const EnvName: String): String;
begin
  Result := GetEnvironmentVariableUTF8('XDG_CONFIG_HOME');
  if Result = '' then
  begin
    Result := GetEnvironmentVariableUTF8('HOME');
    if Result = '' then
      Result := ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))
    else
      Result += PathDelim+FallbackDotName+PathDelim+MiddlePart;
  end
  else
    Result += PathDelim+MiddlePart;
  if not ForceDirectoriesUTF8(Result) then
  begin
    // We cannot write there or something else went wrong. No plan B, we'll
    // raise an exception for now.
    raise EGTPathError.CreateFmt('Could not create config directory: ''%s''.', [Result]);
  end;
  Result += PathDelim;
end;

class procedure TGTPaths.ListXDGPaths(const AppNamePart: String;
  const FallbackGlobal, FallbackDotName: String; const EnvNameGlobal,
  EnvNameLocal: String; const AList: TStrings; const IncludeHome: Boolean);
var
  Paths, Path: String;
  P: Integer;
begin
  if ClassType = TGTPaths then
    raise EGTPathError.Create('You must subclass TGTXDGPaths and assign the class to the global XDG variable to use XDG paths.');
  Paths := GetEnvironmentVariableUTF8(EnvNameGlobal);
  if Paths = '' then
    Paths := FallbackGlobal;
  repeat
    P := Pos(':', Paths);
    if P = 0 then
      Path := IncludeTrailingPathDelimiter(Paths)
    else
    begin
      Path := IncludeTrailingPathDelimiter(Copy(Paths, 1, P-1));
      Delete(Paths, 1, P);
    end;
    Path := Path + AppNamePart + PathDelim;
    if DirectoryExistsUTF8(Path) then
    begin
      AList.Insert(0, Path);
    end;
  until P = 0;
  if IncludeHome then
  begin
    AList.Insert(0, CreateXDGPath(AppNamePart, FallbackDotName, EnvNameLocal));
  end;
end;

class function TGTPaths.GetXDGConfigDirName: String;
begin
  Result := GetAppName;
end;

class function TGTPaths.GetXDGDataDirName: String;
begin
  Result := GetAppName;
end;

class function TGTPaths.GetXDGConfigPath: String;
begin
  if ClassType = TGTPaths then
    raise EGTPathError.Create('You must subclass TGTXDGPaths and assign the class to the global XDG variable to use XDG paths.');
  if FConfigPath <> '' then
    Exit(FConfigPath);
  FConfigPath := CreateXDGPath(GetXDGConfigDirName, '.config', 'XDG_CONFIG_HOME');
  Result := FConfigPath;
end;

class function TGTPaths.GetXDGDataPath: String;
begin
  if ClassType = TGTPaths then
    raise EGTPathError.Create('You must subclass TGTXDGPaths and assign the class to the global XDG variable to use XDG paths.');
  if FDataPath <> '' then
    Exit(FDataPath);
  FDataPath := CreateXDGPath('share/'+GetXDGDataDirName, '.local', 'XDG_DATA_HOME');
  Result := FDataPath;
end;

class function TGTPaths.GetXDGReadOnlyDataPath: String;
var
  Paths, Path: String;
  P: Integer;
  Found: Boolean;
begin
  if ClassType = TGTPaths then
    raise EGTPathError.Create('You must subclass TGTXDGPaths and assign the class to the global XDG variable to use XDG paths.');
  if FReadOnlyDataPath <> '' then
    Exit(FReadOnlyDataPath);

  Paths := GetEnvironmentVariableUTF8('XDG_DATA_DIRS');
  if Paths = '' then
    Paths := '/usr/local/share/:/usr/share/';
  Path := GetEnvironmentVariableUTF8('XDG_DATA_HOME');
  if Path = '' then
  begin
    Path := GetEnvironmentVariableUTF8('HOME');
    if Path <> '' then
      Paths += ':'+IncludeTrailingPathDelimiter(Path)+'.local'+PathDelim+'share'+PathDelim;
  end
  else
    Paths += ':'+Path;
  Found := False;
  repeat
    P := Pos(':', Paths);
    if P = 0 then
      Path := IncludeTrailingPathDelimiter(Paths)
    else
    begin
      Path := IncludeTrailingPathDelimiter(Copy(Paths, 1, P-1));
      Delete(Paths, 1, P);
    end;
    Path := Path + GetXDGDataDirName + PathDelim;
    if DirectoryExistsUTF8(Path) then
    begin
      Found := True;
      Break;
    end;
  until P = 0;
  if Found then
  begin
    FReadOnlyDataPath := Path;
    Exit(Path);
  end;
  FReadOnlyDataPath := './';
  Exit(FReadOnlyDataPath);
end;

class procedure TGTPaths.GetXDGReadOnlyDataPaths(const AList: TStrings; const IncludeHome: Boolean);
begin
  ListXDGPaths(GetXDGDataDirName, '/usr/local/share/:/usr/share/', '.local'+PathDelim+'share', 'XDG_DATA_DIRS', 'XDG_DATA_HOME', AList, IncludeHome);
end;

class procedure TGTPaths.GetXDGReadOnlyConfigPaths(const AList: TStrings;
  const IncludeHome: Boolean);
begin
  ListXDGPaths(GetXDGConfigDirName, '/etc/', '.config', 'XDG_DATA_DIRS', 'XDG_DATA_HOME', AList, IncludeHome);
end;

end.

