unit GTNodeDataTypeBlockMemory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTBlockMemoryManager, GTNodes;

type

  { TGTNodeDataTypeBlockMemory }

  TGTNodeDataTypeBlockMemory = class (TGTNodeDataType)
  public
    constructor Create(const AOvermind: TGTNodeOvermind);
    destructor Destroy; override;
  private
    FAllowExpansion: Boolean;
    FMemoryManager: TGTStaticBlockMemoryManager;
    FSize: SizeInt;
    procedure SetSize(const AValue: SizeInt);
  public
    procedure Burn; override;
    function GetItem: Pointer; override;
    procedure FreeItem(var AItem: Pointer); override;
    procedure Init; override;
  published
    property AllowExpansion: Boolean read FAllowExpansion write FAllowExpansion;
    property Size: SizeInt read FSize write SetSize;
  end;

implementation

{ TGTNodeDataTypeBlockMemory }

constructor TGTNodeDataTypeBlockMemory.Create(const AOvermind: TGTNodeOvermind
  );
begin
  inherited Create(AOvermind);
  FAllowExpansion := False;
  FSize := 0;
  FMemoryManager := nil;
end;

destructor TGTNodeDataTypeBlockMemory.Destroy;
begin
  Assert(FMemoryManager = nil);
  inherited Destroy;
end;

procedure TGTNodeDataTypeBlockMemory.SetSize(const AValue: SizeInt);
begin
  if FSize = AValue then exit;
  if FMemoryManager <> nil then
    raise EGTNodeLockError.CreateFmt('%s must not be initialized to change block size.', [ClassName]);
  FSize := AValue;
end;

procedure TGTNodeDataTypeBlockMemory.Burn;
begin
  if FMemoryManager <> nil then
  begin
    FreeAndNil(FMemoryManager);
  end;
  inherited Burn;
end;

function TGTNodeDataTypeBlockMemory.GetItem: Pointer;
begin
  Result := FMemoryManager.GetFreeBlock;
end;

procedure TGTNodeDataTypeBlockMemory.FreeItem(var AItem: Pointer);
begin
  FMemoryManager.ReleaseBlock(AItem);
end;

procedure TGTNodeDataTypeBlockMemory.Init;
begin
  inherited Init;
  if FMemoryManager <> nil then
    FMemoryManager.Free;
  FMemoryManager := TGTStaticBlockMemoryManager.Create(FSize, 128, FAllowExpansion);
end;

end.

