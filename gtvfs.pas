unit GTVFS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, FileUtil;

type
  TGTFilePriority = (
    fpPracticallyInexsitant,
    fpDiscriminated,
    fpFallback,
    fpFileSystem,
    fpImportant,
    fpOverride,
    fpPenetrant
  );

  TGTVFS = class;

  { TGTMount }

  TGTMount = class (TObject)
  public
    constructor Create(const AVFS: TGTVFS);
  private
    FPriority: TGTFilePriority;
    FVFS: TGTVFS;
  protected
    procedure EnableMount; virtual;
  public
    property Priority: TGTFilePriority read FPriority;
    property VFS: TGTVFS read FVFS;
  public
    function FileExists(const AFileName: String): Boolean; virtual; abstract;
    function OpenFile(const AFileName: String; Mode: Word = fmOpenRead): TStream; virtual; abstract;
  end;

  TGTMountList = specialize TFPGList<TGTMount>;

  TGTMountLibrary = array [TGTFilePriority] of TGTMountList;

  { TGTMountDirectory }

  TGTMountDirectory = class (TGTMount)
  public
    constructor Create(const AVFS: TGTVFS; const ARootDirectory: String);
  private
    FDirectory: String;
  protected
    function AllowFileName(const AFileName: String): Boolean;
    function ConvertFileName(const AFileName: String): String;
  public
    function FileExists(const AFileName: String): Boolean; override;
    function OpenFile(const AFileName: String; Mode: Word=fmOpenRead
       ): TStream; override;
  end;

  { TGTVFS }

  TGTVFS = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FFindCache: TStringList;
    FMountLibrary: TGTMountLibrary;
    FMountList: TGTMountList;
    function GetCount: Integer;
    function GetCountByPriority(APriority: TGTFilePriority): Integer;
    function GetMount(Index: Integer): TGTMount;
    function GetMountByPriority(APriority: TGTFilePriority; Index: Integer
      ): TGTMount;
  protected
    function GetFindCache(const AFileName: String): TGTMount;
    function RawFindFileMount(const AFileName: String): TGTMount;
    procedure SetFindCache(const AFileName: String; AMount: TGTMount);
  public
    property Count: Integer read GetCount;
    property CountByPriority[APriority: TGTFilePriority]: Integer read GetCountByPriority;
    property Mount[Index: Integer]: TGTMount read GetMount;
    property MountByPriority[APriority: TGTFilePriority; Index: Integer]: TGTMount read GetMountByPriority;
  public
    procedure AddMount(const AMount: TGTMount;
      const APriority: TGTFilePriority);
    function FileExists(const AFileName: String): Boolean; virtual;
    function FindFileMount(const AFileName: String): TGTMount; virtual;
    function OpenFile(const AFileName: String; Mode: Word = fmOpenRead): TStream; virtual;
    procedure Unmount(const AMount: TGTMount);
    procedure UnmountAll;
  end;

implementation

{ TGTMount }

constructor TGTMount.Create(const AVFS: TGTVFS);
begin
  FVFS := AVFS;
end;

procedure TGTMount.EnableMount;
begin

end;

{ TGTMountDirectory }

constructor TGTMountDirectory.Create(const AVFS: TGTVFS;
  const ARootDirectory: String);
begin
  inherited Create(AVFS);
  FDirectory := ARootDirectory;
end;

function TGTMountDirectory.AllowFileName(const AFileName: String): Boolean;
var
  Level: Integer;
  I: Integer;
  Dots: Integer;
begin
  Level := 0;
  Dots := 0;
  for I := 1 to Length(AFileName) do
  begin
    case AFileName[I] of
      '\', '/':
      begin
        if Dots = 2 then
        begin
          Level-=1;
          if Level < 0 then
          begin
            Result := False;
            Exit;
          end;
        end
        else if Dots <> 1 then
          Level+=1;
      end;
      '.':
      begin
        Dots += 1;
      end;
    else
      Dots := 0;
    end;
  end;
  Result := True;
end;

function TGTMountDirectory.ConvertFileName(const AFileName: String): String;
begin
  {$ifdef WINDOWS}
    Result := StringReplace(AFileName, '/', PathDelim, [rfReplaceAll]);
  {$else}
    Result := AFileName;
  {$endif}
end;

function TGTMountDirectory.FileExists(const AFileName: String): Boolean;
var
  FullPath: String;
begin
  if not AllowFileName(AFileName) then
    Result := False
  else
  begin
    FullPath := FDirectory + PathDelim + ConvertFileName(AFileName);
    Result := FileExistsUTF8(FullPath);
  end;
end;

function TGTMountDirectory.OpenFile(const AFileName: String; Mode: Word
  ): TStream;
begin
  if not AllowFileName(AFileName) then
    raise EFOpenError.CreateFmt('Could not open file ''%s''. Path leaves scope.', [AFileName]);
  Result := TFileStream.Create(UTF8ToSys(ConvertFileName(AFileName)), Mode);
end;

{ TGTVFS }

constructor TGTVFS.Create;
var
  Priority: TGTFilePriority;
begin
  FFindCache := TStringList.Create;
  FMountList := TGTMountList.Create;
  for Priority := Low(TGTFilePriority) to High(TGTFilePriority) do
    FMountLibrary[Priority] := TGTMountList.Create;
end;

destructor TGTVFS.Destroy;
var
  Priority: TGTFilePriority;
begin
  UnmountAll;
  FMountList.Free;
  for Priority := Low(TGTFilePriority) to High(TGTFilePriority) do
    FMountLibrary[Priority].Free;
  FFindCache.Free;
  inherited Destroy;
end;

function TGTVFS.GetCount: Integer;
begin
  Result := FMountList.Count;
end;

function TGTVFS.GetCountByPriority(APriority: TGTFilePriority): Integer;
begin
  Result := FMountLibrary[APriority].Count;
end;

function TGTVFS.GetMount(Index: Integer): TGTMount;
begin
  Result := FMountList[Index];
end;

function TGTVFS.GetMountByPriority(APriority: TGTFilePriority; Index: Integer
  ): TGTMount;
begin
  Result := FMountLibrary[APriority][Index];
end;

function TGTVFS.GetFindCache(const AFileName: String): TGTMount;
var
  Idx: Integer;
begin
  Idx := FFindCache.IndexOf(AFileName);
  if Idx >= 0 then
    Result := TGTMount(FFindCache.Objects[Idx])
  else
    Result := nil;
end;

function TGTVFS.RawFindFileMount(const AFileName: String): TGTMount;
var
  I: Integer;
  Priority: TGTFilePriority;
begin
  for Priority := High(TGTFilePriority) downto Low(TGTFilePriority) do
    for I := FMountLibrary[Priority].Count - 1 downto 0 do
    begin
      Result := FMountLibrary[Priority][I];
      if Result.FileExists(AFileName) then
        Exit;
    end;
  Result := nil;
end;

procedure TGTVFS.SetFindCache(const AFileName: String; AMount: TGTMount);
var
  I: Integer;
begin
  FFindCache.InsertObject(0, AFileName, AMount);
  I := FFindCache.Count;
  if I > FMountList.Count then
    FFindCache.Delete(I - 1);
end;

procedure TGTVFS.AddMount(const AMount: TGTMount; const APriority: TGTFilePriority
  );
begin
  FMountList.Add(AMount);
  FMountLibrary[APriority].Add(AMount);
  AMount.FPriority := APriority;
  AMount.EnableMount;
end;

function TGTVFS.FileExists(const AFileName: String): Boolean;
begin
  Result := FindFileMount(AFileName) <> nil;
end;

function TGTVFS.FindFileMount(const AFileName: String): TGTMount;
begin
  Result := GetFindCache(AFileName);
  if Result <> nil then
    Exit;
  Result := RawFindFileMount(AFileName);
  if Result <> nil then
    SetFindCache(AFileName, Result);
end;

function TGTVFS.OpenFile(const AFileName: String; Mode: Word): TStream;
var
  AMount: TGTMount;
begin
  AMount := FindFileMount(AFileName);
  if AMount = nil then
    raise EFOpenError.CreateFmt('Cannot open file ''%s''. File not found.', [AFileName]);
  Result := AMount.OpenFile(AFileName, Mode);
end;

procedure TGTVFS.Unmount(const AMount: TGTMount);
var
  I: Integer;
begin
  I := FMountList.IndexOf(AMount);
  if I < 0 then
    Exit;
  FMountList.Delete(I);
  I := FMountLibrary[AMount.FPriority].IndexOf(AMount);
  if I >= 0 then
    FMountLibrary[AMount.FPriority].Delete(I);
  AMount.Free;
end;

procedure TGTVFS.UnmountAll;
var
  I: Integer;
  Priority: TGTFilePriority;
begin
  for I := FMountList.Count - 1 downto 0 do
    FMountList[I].Free;
  FMountList.Clear;
  for Priority := Low(TGTFilePriority) to High(TGTFilePriority) do
    FMountLibrary[Priority].Clear;
end;

end.
