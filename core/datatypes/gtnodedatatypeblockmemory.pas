unit GTNodeDataTypeBlockMemory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTBlockMemoryManager, GTNodes;

type

  { TGTNodeDataTypeCustomBlockMemory }

  TGTNodeDataTypeCustomBlockMemory = class (TGTNodeDataType)
  public
    constructor Create(const AOvermind: TGTNodeOvermind);
    destructor Destroy; override;
  private
    FAllowExpansion: Boolean;
    FMemoryManager: TGTStaticBlockMemoryManager;
    FSize: SizeInt;
    procedure SetSize(const AValue: SizeInt);
  protected
    property AllowExpansion: Boolean read FAllowExpansion write FAllowExpansion;
    property Size: SizeInt read FSize write SetSize;
  public
    procedure Burn; override;
    function GetItem: Pointer; override;
    function GetSize: SizeUInt; override;
    procedure FreeItem(var AItem: Pointer); override;
    procedure Init; override;
  end;

  TGTNodeDataTypeBlockMemory = class (TGTNodeDataTypeCustomBlockMemory)
  published
    property AllowExpansion;
    property Size;
  end;


implementation

{ TGTNodeDataTypeCustomBlockMemory }

constructor TGTNodeDataTypeCustomBlockMemory.Create(const AOvermind: TGTNodeOvermind
  );
begin
  inherited Create(AOvermind);
  FAllowExpansion := False;
  FSize := 0;
  FMemoryManager := nil;
end;

destructor TGTNodeDataTypeCustomBlockMemory.Destroy;
begin
  Assert(FMemoryManager = nil);
  inherited Destroy;
end;

procedure TGTNodeDataTypeCustomBlockMemory.SetSize(const AValue: SizeInt);
begin
  if FSize = AValue then exit;
  if FMemoryManager <> nil then
    raise EGTNodeLockError.CreateFmt('%s must not be initialized to change block size.', [ClassName]);
  FSize := AValue;
end;

procedure TGTNodeDataTypeCustomBlockMemory.Burn;
begin
  inherited Burn;
  WriteLn(Format('[0x%16.16x] [%s] Burned', [ptrint(Self), ClassName]));
  if FMemoryManager <> nil then
  begin
    FreeAndNil(FMemoryManager);
  end;
end;

function TGTNodeDataTypeCustomBlockMemory.GetItem: Pointer;
begin
  WriteLn(Format('[0x%16.16x] [%s] GetItem (memman=$%16.16x)', [ptrint(Self), ClassName, ptrint(FMemoryManager)]));
  Result := FMemoryManager.GetFreeBlock;
end;

function TGTNodeDataTypeCustomBlockMemory.GetSize: SizeUInt;
begin
  Exit(FSize);
end;

procedure TGTNodeDataTypeCustomBlockMemory.FreeItem(var AItem: Pointer);
begin
  FMemoryManager.ReleaseBlock(AItem);
end;

procedure TGTNodeDataTypeCustomBlockMemory.Init;
begin
  inherited Init;
  if FMemoryManager <> nil then
    FMemoryManager.Free;
  FMemoryManager := TGTStaticBlockMemoryManager.Create(FSize, 128, FAllowExpansion);
  WriteLn(Format('[0x%16.16x] [%s] Initialized (memman=$%16.16x)', [ptrint(Self), ClassName, ptrint(FMemoryManager)]));
end;

end.

