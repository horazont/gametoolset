unit GTVFS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTBase, GTURI, fgl, GTPaths, contnrs, typinfo;

type
  TGTFilePriority = (
    fpPracticallyInexistant,
    fpDiscriminated,
    fpFallback,
    fpFileSystem,
    fpImportant,
    fpOverride,
    fpPenetrant
  );

  EGTVFSError = class (EGTError);
  EGTVFSMountError = class (EGTError);

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
    function GetCapabilities: TGTProtocolCapabilities; virtual;
  public
    property Capabilities: TGTProtocolCapabilities read GetCapabilities;
    property Priority: TGTFilePriority read FPriority;
    property VFS: TGTVFS read FVFS;
  public
    function CreateBidirectionalStream(const APath: String;
      const WriteMode: TGTStreamWriteMode = wmOverwrite;
      const ShareMode: TGTStreamShareMode = smDontCare): TStream; virtual; abstract;
    function CreateReadOnlyStream(const APath: String;
      const ShareMode: TGTStreamShareMode = smDontCare): TStream; virtual;
    function CreateWriteOnlyStream(const APath: String;
      const WriteMode: TGTStreamWriteMode = wmOverwrite;
      const ShareMode: TGTStreamShareMode = smDontCare): TStream; virtual;
    function FileExists(const AFileName: String): Boolean; virtual; abstract;
  end;

  { TGTMountWrapper }

  TGTMountWrapper = class (TObject)
  public
    constructor Create(const AMount: TGTMount; const ALocation: String);
    destructor Destroy; override;
  private
    FLocation: String;
    FMount: TGTMount;
  public
    property Location: String read FLocation;
    property Mount: TGTMount read FMount;
  end;

  TGTMountList = specialize TFPGList<TGTMountWrapper>;

  TGTMountLibrary = array [TGTFilePriority] of TGTMountList;

  { TGTVFS }

  TGTVFS = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FFindCache: TFPObjectHashTable;
    FMountLibrary: TGTMountLibrary;
    FMountList: TGTMountList;
    function GetCount: Integer;
    function GetCountByPriority(APriority: TGTFilePriority): Integer;
    function GetMount(Index: Integer): TGTMountWrapper;
    function GetMountByPriority(APriority: TGTFilePriority; Index: Integer
      ): TGTMountWrapper;
  protected
    function GetFindCache(const AFileName: String): TGTMount;
    procedure MapFileName(const AInputFileName: String; out AOutput: TStrings); virtual;
    function RawFindFileMount(var AFileName: String; MustExist: Boolean): TGTMount;
    procedure SetFindCache(const AFileName: String; AMount: TGTMount);
  public
    property Count: Integer read GetCount;
    property CountByPriority[APriority: TGTFilePriority]: Integer read GetCountByPriority;
    property Mount[Index: Integer]: TGTMountWrapper read GetMount;
    property MountByPriority[APriority: TGTFilePriority; Index: Integer]: TGTMountWrapper read GetMountByPriority;
  public
    procedure AddMount(const AMount: TGTMount;
      const ALocation: String; const APriority: TGTFilePriority);
    function CreateBidirectionalStream(const AFileName: String;
      const WriteMode: TGTStreamWriteMode = wmOverwrite;
      const ShareMode: TGTStreamShareMode = smDontCare): TStream; virtual;
    function CreateReadOnlyStream(const AFileName: String;
      const ShareMode: TGTStreamShareMode = smDontCare): TStream; virtual;
    function CreateWriteOnlyStream(const AFileName: String;
      const WriteMode: TGTStreamWriteMode = wmOverwrite;
      const ShareMode: TGTStreamShareMode = smDontCare): TStream; virtual;
    procedure DumpMounts;
    function FileExists(const AFileName: String): Boolean; virtual;
    function FindFileMount(var AFileName: String; MustExist: Boolean): TGTMount; virtual;
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

function TGTMount.GetCapabilities: TGTProtocolCapabilities;
begin
  Exit([]);
end;

function TGTMount.CreateReadOnlyStream(const APath: String;
  const ShareMode: TGTStreamShareMode): TStream;
begin
  if not (pcRead in GetCapabilities) then
    raise EFOpenError.CreateFmt('Cannot open file ''%s''. Read mode not supported.', [APath]);
  Exit(CreateBidirectionalStream(APath, wmIgnore, ShareMode));
end;

function TGTMount.CreateWriteOnlyStream(const APath: String;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
begin
  if not (pcWrite in GetCapabilities) then
    raise EFOpenError.CreateFmt('Cannot open file ''%s''. Write mode not supported.', [APath]);
  Exit(CreateBidirectionalStream(APath, WriteMode, ShareMode));
end;

{ TGTMountWrapper }

constructor TGTMountWrapper.Create(const AMount: TGTMount;
  const ALocation: String);
begin
  FMount := AMount;
  FLocation := ALocation;
end;

destructor TGTMountWrapper.Destroy;
begin
  FMount.Free;
  inherited Destroy;
end;

{ TGTVFS }

constructor TGTVFS.Create;
var
  Priority: TGTFilePriority;
begin
  FFindCache := TFPObjectHashTable.Create(False);
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

function TGTVFS.GetMount(Index: Integer): TGTMountWrapper;
begin
  Result := FMountList[Index];
end;

function TGTVFS.GetMountByPriority(APriority: TGTFilePriority; Index: Integer
  ): TGTMountWrapper;
begin
  Result := FMountLibrary[APriority][Index];
end;

function TGTVFS.GetFindCache(const AFileName: String): TGTMount;
begin
  Exit(TGTMount(FFindCache[AFileName]));
end;

procedure TGTVFS.MapFileName(const AInputFileName: String; out AOutput: TStrings
  );
begin
  AOutput := nil;
end;

function TGTVFS.RawFindFileMount(var AFileName: String; MustExist: Boolean): TGTMount;
var
  I: Integer;
  Priority: TGTFilePriority;
  Wrapper: TGTMountWrapper;
  StrippedFileName: String;
begin
  for Priority := High(TGTFilePriority) downto Low(TGTFilePriority) do
  begin
    for Wrapper in FMountLibrary[Priority] do
    begin
      if Copy(AFileName, 1, Length(Wrapper.FLocation)) = Wrapper.FLocation then
      begin
        StrippedFileName := AFileName;
        Delete(StrippedFileName, 1, Length(Wrapper.FLocation));
        if (not MustExist) or Wrapper.Mount.FileExists(StrippedFileName) then
        begin
          AFileName := StrippedFileName;
          Exit(Wrapper.Mount);
        end;
      end;
    end;
  end;
  Result := nil;
end;

procedure TGTVFS.SetFindCache(const AFileName: String; AMount: TGTMount);
begin
  FFindCache.Add(AFileName, AMount);
end;

procedure TGTVFS.AddMount(const AMount: TGTMount; const ALocation: String;
  const APriority: TGTFilePriority);
var
  Wrapper: TGTMountWrapper;
begin
  Wrapper := TGTMountWrapper.Create(AMount, ALocation);
  FMountList.Add(Wrapper);
  FMountLibrary[APriority].Add(Wrapper);
  AMount.FPriority := APriority;
  AMount.EnableMount;
end;

function TGTVFS.CreateBidirectionalStream(const AFileName: String;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
var
  AMount: TGTMount;
  TmpFileName: String;
begin
  TmpFileName := AFileName;
  AMount := FindFileMount(TmpFileName, True);
  if AMount = nil then
    raise EFOpenError.CreateFmt('Cannot open file ''%s''. File not found.', [AFileName]);
  if not (pcBidirectional in AMount.Capabilities) then
    raise EFOpenError.CreateFmt('Cannot open file ''%s''. Bidirectional mode not supported.', [AFileName]);
  Result := AMount.CreateBidirectionalStream(TmpFileName, WriteMode, ShareMode);
end;

function TGTVFS.CreateReadOnlyStream(const AFileName: String;
  const ShareMode: TGTStreamShareMode): TStream;
var
  AMount: TGTMount;
  TmpFileName: String;
begin
  TmpFileName := AFileName;
  AMount := FindFileMount(TmpFileName, True);
  if AMount = nil then
    raise EFOpenError.CreateFmt('Cannot open file ''%s''. File not found.', [AFileName]);
  Result := AMount.CreateReadOnlyStream(TmpFileName, ShareMode);
end;

function TGTVFS.CreateWriteOnlyStream(const AFileName: String;
  const WriteMode: TGTStreamWriteMode; const ShareMode: TGTStreamShareMode
  ): TStream;
var
  AMount: TGTMount;
  TmpFileName: String;
begin
  TmpFileName := AFileName;
  AMount := FindFileMount(TmpFileName, WriteMode = wmAppend);
  if AMount = nil then
    raise EFOpenError.CreateFmt('Cannot open file ''%s''. File not found.', [AFileName]);
  Result := AMount.CreateWriteOnlyStream(TmpFileName, WriteMode, ShareMode);
end;

procedure TGTVFS.DumpMounts;
var
  Priority: TGTFilePriority;
  Wrapper: TGTMountWrapper;
begin
  WriteLn('Mounts: ');
  for Priority := Low(TGTFilePriority) to High(TGTFilePriority) do
  begin
    WriteLn('  priority: ', Copy(GetEnumName(TypeInfo(TGTFilePriority), Ord(Priority)), 3, 1024));
    for Wrapper in FMountLibrary[Priority] do
      WriteLn('    ', Wrapper.Mount.ToString, ' on ', Wrapper.Location);
  end;
end;

function TGTVFS.FileExists(const AFileName: String): Boolean;
var
  TmpFileName: String;
begin
  TmpFileName := AFileName;
  Result := FindFileMount(TmpFileName, True) <> nil;
end;

function TGTVFS.FindFileMount(var AFileName: String; MustExist: Boolean
  ): TGTMount;
begin
  {Result := GetFindCache(AFileName);
  if Result <> nil then
    Exit;}
  Result := RawFindFileMount(AFileName, MustExist);
  {if Result <> nil then
    SetFindCache(AFileName, Result);}
end;

procedure TGTVFS.Unmount(const AMount: TGTMount);
var
  I, J: Integer;
  Wrapper: TGTMountWrapper;
begin
  for I := 0 to FMountList.Count - 1 do
  begin
    Wrapper := FMountList[I];
    if Wrapper.Mount = AMount then
    begin
      FMountList.Delete(I);
      J := FMountLibrary[AMount.FPriority].IndexOf(Wrapper);
      if J >= 0 then
        FMountLibrary[AMount.FPriority].Delete(J);
      Wrapper.Free;
    end;
  end;
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

