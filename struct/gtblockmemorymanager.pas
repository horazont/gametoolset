unit GTBlockMemoryManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTThreadFIFO;

type
  TGTResizeStackEntry = record
    RemainingToResize: Integer;
    NewSize: sizeint;
  end;

  TGTBlockStack = TGTThreadFIFO;

  TGTBlockGroup = PPointer;

  { TGTStaticBlockMemoryManager }

  TGTStaticBlockMemoryManager = class (TObject)
  public
    constructor Create(const BlockSize, BlocksPerGroup: SizeUInt;
      const AllowExpansion: Boolean);
    destructor Destroy; override;
  private
    FAllowExpansion: Boolean;
    FBlockSize, FBlocksPerGroup: SizeUInt;
    FFreeBlocks: TGTBlockStack;
    FBlockLock: TRTLCriticalSection;
    FBlocks: array of TGTBlockGroup;
    FBlockCapacity, FBlockCount: Integer;
  protected
    procedure Expand;
  public
    function GetFreeBlock: Pointer;
    procedure ReleaseBlock(const ABlock: Pointer);
  end;

implementation

constructor TGTStaticBlockMemoryManager.Create(const BlockSize,
  BlocksPerGroup: SizeUInt; const AllowExpansion: Boolean);
begin
  FBlockSize := BlockSize;
  FBlocksPerGroup := BlocksPerGroup;
  FFreeBlocks := TGTBlockStack.Create;
  FBlockCapacity := 0;
  FBlockCount := 0;
  InitCriticalSection(FBlockLock);
  FAllowExpansion := True;
  Expand;
  FAllowExpansion := AllowExpansion;
end;

destructor TGTStaticBlockMemoryManager.Destroy;
var
  I, J: Integer;
begin
  DoneCriticalsection(FBlockLock);
  for I := 0 to High(FBlocks) do
  begin
    if FBlocks[I] <> nil then
    begin
      for J := 0 to FBlocksPerGroup-1 do
        FreeMem(FBlocks[I][J]);
      FreeMem(FBlocks[I]);
    end;
  end;
  FFreeBlocks.Free;
  inherited Destroy;
end;

procedure TGTStaticBlockMemoryManager.Expand;
var
  I, J: Integer;
  CurrBlock: PPointer;
begin
  if not FAllowExpansion then
  begin
    FFreeBlocks.WaitData;
    Exit;
  end;
  EnterCriticalSection(FBlockLock);
  try
    if FBlockCount = FBlockCapacity then
    begin
      FBlockCapacity += 8;
      SetLength(FBlocks, FBlockCapacity);
      for I := FBlockCount to FBlockCapacity - 1 do
        FBlocks[I] := nil;
    end;
    I := FBlockCount;
    FBlocks[I] := GetMem(FBlocksPerGroup*SizeOf(Pointer));
    CurrBlock := FBlocks[I];
    for J := 1 to FBlocksPerGroup do
    begin
      CurrBlock^ := GetMem(FBlockSize);
      FFreeBlocks.Push(CurrBlock^);
      Inc(CurrBlock);
    end;
    Inc(FBlockCount);
  finally
    LeaveCriticalSection(FBlockLock);
  end;
end;

function TGTStaticBlockMemoryManager.GetFreeBlock: Pointer;
begin
  if not FAllowExpansion then
  begin
    Result := FFreeBlocks.PopBlocking;
  end
  else
  begin
    while FFreeBlocks.Empty do
      Expand;
    Result := FFreeBlocks.PopBlocking;
  end;
end;

procedure TGTStaticBlockMemoryManager.ReleaseBlock(const ABlock: Pointer);
begin
  FFreeBlocks.Push(ABlock);
end;

end.

