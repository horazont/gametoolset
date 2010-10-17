unit GTXDG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

type
  EGTXDGPathError = class (Exception);

  { TGTXDGPaths }

  TGTXDGPaths = class (TObject)
  private
    constructor Create;
  private
    FConfigPath: String; static;
    FDataPath: String; static;
  protected
    class function CreateXDGPath(const MiddlePart: String; const FallbackDotName: String): String;
  public
    class function GetAppName: String; virtual; abstract;
    class function GetXDGConfigDirName: String; virtual;
    class function GetXDGDataDirName: String; virtual;
    class function GetXDGConfigPath: String;
    class function GetXDGDataPath: String;
  end;
  TGTXDGPathsClass = class of TGTXDGPaths;

var
  XDG: TGTXDGPathsClass;

implementation

{ TGTXDGPaths }

constructor TGTXDGPaths.Create;
begin

end;

class function TGTXDGPaths.CreateXDGPath(const MiddlePart: String;
  const FallbackDotName: String): String;
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
    raise EGTXDGPathError.CreateFmt('Could not create config directory: ''%s''.', [Result]);
  end;
  Result += PathDelim;
end;

class function TGTXDGPaths.GetXDGConfigDirName: String;
begin
  Result := GetAppName;
end;

class function TGTXDGPaths.GetXDGDataDirName: String;
begin
  Result := GetAppName;
end;

class function TGTXDGPaths.GetXDGConfigPath: String;
begin
  if ClassType = TGTXDGPaths then
    raise EGTXDGPathError.Create('You must subclass TGTXDGPaths and assign the class to the global XDG variable to use XDG paths.');
  if FConfigPath <> '' then
    Exit(FConfigPath);
  FConfigPath := CreateXDGPath(GetXDGConfigDirName, '.config');
  Result := FConfigPath;
end;

class function TGTXDGPaths.GetXDGDataPath: String;
begin
  if ClassType = TGTXDGPaths then
    raise EGTXDGPathError.Create('You must subclass TGTXDGPaths and assign the class to the global XDG variable to use XDG paths.');
  if FDataPath <> '' then
    Exit(FDataPath);
  FDataPath := CreateXDGPath('share/'+GetXDGDataDirName, '.local');
  Result := FDataPath;
end;

end.

