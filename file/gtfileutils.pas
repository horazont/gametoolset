unit GTFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zstream;

const
  gfFileCompressed = 1;

  gfDefaultFlags = gfFileCompressed;

const
  {$ifdef ENDIAN_LITTLE}
  GT_FILE_HEADER_MAGIC : Cardinal = $5447776A;
  {$else}
  GT_FILE_HEADER_MAGIC : Cardinal = $6A774754;
  {$endif}

type
  EGTFileError = class (Exception);
  EGTStreamExtractError = class (Exception);

  TGTFileMode = (fmRead, fmWrite);

  TGTFileHeader = record
    MagicConstant: Cardinal;
    Flags: Cardinal;
  end;

  TGTFileReadFlag = (rfBuffer, rfAutoBuffer);
  TGTFileReadFlags = set of TGTFileReadFlag;

  { TGTStreamCapsule }

  TGTStreamCapsule = class (TOwnerStream)
  public
    constructor Create(ASource: TStream);
    destructor Destroy; override;
  private
    FOffset: Int64;
  protected
    function GetOffset: Int64; virtual;
    function GetPosition: Int64; override;
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); override; overload;
  public
    procedure AfterConstruction; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
       overload;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  { TGTStreamExtract }

  TGTStreamExtract = class (TOwnerStream)
  public
    constructor Create(ASource: TStream; AStart, ALength: Int64);
    destructor Destroy; override;
  private
    FLength: Int64;
    FPosition: Int64;
    FStart: Int64;
  protected
    function GetPosition: Int64; override;
    function GetSize: Int64; override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
       overload;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  { TGTFileStream }

  TGTFileStream = class (TGTStreamCapsule)
  public
    constructor CreateReader(AInput: TStream; ReadFlags: TGTFileReadFlags = [
      rfAutoBuffer]; SkipHeader: Boolean = False;
      AFlags: Cardinal = gfDefaultFlags); virtual;
    constructor CreateWriter(AOutput: TStream; AFlags: Cardinal = gfDefaultFlags;
      SkipHeader: Boolean = False); virtual;
    destructor Destroy; override;
  private
    FFlags: Cardinal;
    FMode: TGTFileMode;
    FSkipHeader: Boolean;
  protected
    FSecondarySource: TStream;
  protected
    property Flags: Cardinal read FFlags;
    property Mode: TGTFileMode read FMode;
  protected
    procedure BuildReadChain(var ASource: TStream; const AFlags: Cardinal;
      out ASecondarySource: TStream); virtual;
    procedure BuildWriteChain(var ATarget: TStream; const AFlags: Cardinal;
      out ASecondaryTarget: TStream); virtual;
    constructor Create(ATarget: TStream; AMode: TGTFileMode;
      SkipHeader: Boolean = False); virtual;
    function GetHeaderSize: Int64; virtual;
    function GetOffset: Int64; override;
    function VerifyHeader(ATarget: TStream; var AFlags: Cardinal): Boolean; virtual;
    function WriteHeader(ATarget: TStream): Boolean; virtual;
  end;

  TGTFileStreamClass = class of TGTFileStream;

function BufferedCopyFrom(Self: TStream; Source: TStream; Size: Int64 = -1): Int64;
procedure DeheadFile(const AFileName: String; const AStreamClass: TGTFileStreamClass; OutputFileName: String = '');
procedure HeadFile(const AFileName: String; const AStreamClass: TGTFileStreamClass; Flags: Cardinal = gfDefaultFlags; OutputFileName: String = '');

implementation

function BufferedCopyFrom(Self: TStream; Source: TStream; Size: Int64 = -1): Int64;
const
  BUFFER_SIZE = 1048576;
var
  Read: LongInt;
  Buffer: Pointer;
begin
  Result := 0;
  GetMem(Buffer, BUFFER_SIZE);
  try
    if Size < 0 then
    begin
      repeat
        Read := Source.Read(Buffer^, BUFFER_SIZE);
        Result += Read;
        Self.Write(Buffer^, Read);
      until Read = 0;
    end
    else
    begin
      repeat
        if Size < BUFFER_SIZE then
          Read := Source.Read(Buffer^, Size)
        else
          Read := Source.Read(Buffer^, BUFFER_SIZE);
        Result += Read;
        Size -= Read;
        Self.Write(Buffer^, Read);
      until Read = 0;
    end;
  finally
    FreeMem(Buffer);
  end;
end;

procedure DeheadFile(const AFileName: String;
  const AStreamClass: TGTFileStreamClass; OutputFileName: String);
const
  BUFFER_SIZE = 1048576;
var
  InStream, OutStream: TStream;
  Buffer: Pointer;
  Read: Int64;

begin
  if OutputFileName = '' then
    OutputFileName := AFileName;
  if AFileName = OutputFileName then
  begin
    InStream := TMemoryStream.Create;
    try
      TMemoryStream(InStream).LoadFromFile(AFileName);
    except
      InStream.Free;
      raise;
    end;
  end
  else
  begin
    InStream := TFileStream.Create(AFileName, fmOpenRead);
  end;
  InStream := AStreamClass.CreateReader(InStream);
  try
    OutStream := TFileStream.Create(OutputFileName, fmCreate);
    Buffer := GetMem(BUFFER_SIZE);
    try
      repeat
        Read := InStream.Read(Buffer^, BUFFER_SIZE);
        OutStream.Write(Buffer^, Read);
      until Read = 0;
    finally
      FreeMem(Buffer);
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

procedure HeadFile(const AFileName: String;
  const AStreamClass: TGTFileStreamClass; Flags: Cardinal;
  OutputFileName: String);
var
  InStream, OutStream: TStream;
begin
  if OutputFileName = '' then
    OutputFileName := AFileName;
  if AFileName = OutputFileName then
  begin
    InStream := TMemoryStream.Create;
    try
      TMemoryStream(InStream).LoadFromFile(AFileName);
    except
      InStream.Free;
      raise;
    end;
  end
  else
  begin
    InStream := TFileStream.Create(AFileName, fmOpenRead);
  end;
  try
    OutStream := AStreamClass.CreateWriter(TFileStream.Create(OutputFileName, fmCreate), Flags);
    try
      OutStream.CopyFrom(InStream, InStream.Size);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

{ TGTStreamCapsule }

constructor TGTStreamCapsule.Create(ASource: TStream);
begin
  if ASource = nil then
    raise EStreamError.CreateFmt('%s must not be created with a nil source.', [ClassName]);
  inherited;
  FOwner := True;
end;

destructor TGTStreamCapsule.Destroy;
begin
  inherited Destroy;
end;

function TGTStreamCapsule.GetOffset: Int64;
begin
  Result := 0;
end;

function TGTStreamCapsule.GetPosition: Int64;
begin
  Result := FSource.Position - FOffset;
end;

function TGTStreamCapsule.GetSize: Int64;
begin
  Result := FSource.Size - FOffset;
end;

procedure TGTStreamCapsule.SetSize(const NewSize: Int64);
begin
  FSource.Size := NewSize + FOffset;
end;

procedure TGTStreamCapsule.AfterConstruction;
begin
  inherited AfterConstruction;
  FOffset := GetOffset;
end;

function TGTStreamCapsule.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FSource.Read(Buffer, Count);
end;

function TGTStreamCapsule.Seek(const Offset: Int64; Origin: TSeekOrigin
  ): Int64;
begin
  Result := FSource.Seek(Offset + FOffset, Origin);
end;

function TGTStreamCapsule.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FSource.Write(Buffer, Count);
end;

{ TGTStreamExtract }

constructor TGTStreamExtract.Create(ASource: TStream; AStart, ALength: Int64);
begin
  inherited Create(ASource);
  FStart := AStart;
  FLength := ALength;
  FPosition := 0;
end;

destructor TGTStreamExtract.Destroy;
begin
  inherited Destroy;
end;

function TGTStreamExtract.GetPosition: Int64;
begin
  Result := FPosition;
end;

function TGTStreamExtract.GetSize: Int64;
begin
  Result := FLength;
end;

function TGTStreamExtract.Read(var Buffer; Count: Longint): Longint;
var
  Max: Int64;
  PosStack: Int64;
begin
  Max := FLength - FPosition;
  if Count > Max then
    Count := Max;
  PosStack := FSource.Position;
  FSource.Position := FStart + FPosition;
  Result := FSource.Read(Buffer, Count);
  FSource.Position := PosStack;
  FPosition += Result;
end;

function TGTStreamExtract.Seek(const Offset: Int64; Origin: TSeekOrigin
  ): Int64;
begin
  Result := FPosition;
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: FPosition += Offset;
    soEnd: FPosition := FLength - (Offset+1);
  end;
  if FPosition >= FLength then
    FPosition := FLength - 1;
  if FPosition < 0 then
    FPosition := 0;
end;

function TGTStreamExtract.Write(const Buffer; Count: Longint): Longint;
begin
  // Just to get rid of the warning
  Result := 0;
  raise EGTStreamExtractError.Create('Can only read from stream extracts.');
end;

{ TGTFileStream }

constructor TGTFileStream.Create(ATarget: TStream; AMode: TGTFileMode;
  SkipHeader: Boolean = False);
begin
  inherited Create(ATarget);
  FMode := AMode;
  FSkipHeader := SkipHeader;
  case FMode of
    fmRead:
    begin
      if not (SkipHeader or VerifyHeader(ATarget, FFlags)) then
        raise EGTFileError.Create('File header corrupt or missing.');
      BuildReadChain(FSource, FFlags, FSecondarySource);
    end;
    fmWrite:
    begin
      if not (SkipHeader or WriteHeader(ATarget)) then
        raise EGTFileError.Create('Could not write file header.');
      BuildWriteChain(FSource, FFlags, FSecondarySource);
    end;
  else
    raise EGTFileError.CreateFmt('Invalid mode value (%d).', [Ord(Mode)]);
  end;
end;

constructor TGTFileStream.CreateReader(AInput: TStream;
  ReadFlags: TGTFileReadFlags; SkipHeader: Boolean; AFlags: Cardinal);
const
  DATA_BUFFER_SIZE = 1048576;
var
  Buffer: TMemoryStream;
  DataBuffer: Pointer;
  ReadData: Int64;
begin
  if SkipHeader then
    FFlags := AFlags
  else
    FFlags := 0;
  Create(AInput, fmRead, SkipHeader);
  if ((rfAutoBuffer in ReadFlags) and (FFlags and gfFileCompressed <> 0)) or (rfBuffer in ReadFlags) then
  begin
    Buffer := TMemoryStream.Create;
    try
      DataBuffer := GetMem(DATA_BUFFER_SIZE);
      try
        repeat
          ReadData := Read(DataBuffer^, DATA_BUFFER_SIZE);
          Buffer.Write(DataBuffer^, ReadData);
        until ReadData = 0;
        FSource.Free;
        FSource := Buffer;
        FSource.Position := 0;
      finally
        FreeMem(DataBuffer);
      end;
    except
      Buffer.Free;
      raise;
    end;
  end;
end;

constructor TGTFileStream.CreateWriter(AOutput: TStream; AFlags: Cardinal;
  SkipHeader: Boolean);
begin
  FFlags := AFlags;
  Create(AOutput, fmWrite, SkipHeader);
end;

destructor TGTFileStream.Destroy;
begin
  if FSecondarySource <> nil then
  begin
    FSource.Free;
    FSource := FSecondarySource;
    FSecondarySource := nil;
  end;
  inherited Destroy;
end;

procedure TGTFileStream.BuildReadChain(var ASource: TStream;
  const AFlags: Cardinal; out ASecondarySource: TStream);
begin
  if AFlags and gfFileCompressed <> 0 then
  begin
    ASecondarySource := ASource;
    ASource := Tdecompressionstream.create(ASecondarySource);
  end;
end;

procedure TGTFileStream.BuildWriteChain(var ATarget: TStream;
  const AFlags: Cardinal; out ASecondaryTarget: TStream);
begin
  if AFlags and gfFileCompressed <> 0 then
  begin
    ASecondaryTarget := ATarget;
    ATarget := Tcompressionstream.create(cldefault, ASecondaryTarget);
  end;
end;

function TGTFileStream.GetHeaderSize: Int64;
begin
  Result := SizeOf(TGTFileHeader);
end;

function TGTFileStream.GetOffset: Int64;
begin
  if FSkipHeader or (FSecondarySource <> nil) then
    Result := 0
  else
    Result := GetHeaderSize;
end;

function TGTFileStream.VerifyHeader(ATarget: TStream; var AFlags: Cardinal): Boolean;
var
  Header: TGTFileHeader;
begin
  if FMode <> fmRead then
    raise EGTFileError.Create('Attempt to read an header from a write-only file.');
  FillByte(Header, SizeOf(TGTFileHeader), 0);
  Result := ATarget.Read(Header, SizeOf(TGTFileHeader)) = SizeOf(TGTFileHeader);
  if not Result then
    Exit;
  Result := LEToN(Header.MagicConstant) = GT_FILE_HEADER_MAGIC;
  if not Result then
    Exit;
  AFlags := LEToN(Header.Flags);
end;

function TGTFileStream.WriteHeader(ATarget: TStream): Boolean;
var
  Header: TGTFileHeader;
begin
  if FMode <> fmWrite then
    raise EGTFileError.Create('Attempt to write an header to a read-only file.');
  Header.MagicConstant := NToLE(GT_FILE_HEADER_MAGIC);
  Header.Flags := NToLE(FFlags);
  Result := ATarget.Write(Header, SizeOf(TGTFileHeader)) = SizeOf(TGTFileHeader);
end;

end.

