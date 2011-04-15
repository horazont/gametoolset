unit GTThreadFIFO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PGTThreadFIFOElement = ^TGTThreadFIFOElement;
  TGTThreadFIFOElement = record
    Up: PGTThreadFIFOElement;
    Data: Pointer;
  end;

  TGTAsyncMethod = procedure (Data: Pointer) of object;
  TGTAsyncMethodElement = record
    Method: TGTAsyncMethod;
    Data: Pointer;
  end;
  PGTAsyncMethodElement = ^TGTAsyncMethodElement;

  { TGTCustomThreadFIFO }

  TGTCustomThreadFIFO = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FAvailableSemaphore: Pointer;
    FBottomElement: PGTThreadFIFOElement;
    FBottomLock: TRTLCriticalSection;
    FEmpty: Boolean;
    FTag: Integer;
    FTopElement: PGTThreadFIFOElement;
    FTopLock: TRTLCriticalSection;
  protected
    property Tag: Integer read FTag write FTag;

    procedure FreeItem(AData: Pointer); virtual;
  public
    procedure Clear;
    function Empty: Boolean;
    function Pop: Pointer;
    procedure Push(AData: Pointer);
    procedure WaitData;
  public
    property AvailableSemaphore: Pointer read FAvailableSemaphore;
  end;

  TGTThreadFIFO = class (TGTCustomThreadFIFO)
  public
    property Tag;
  end;

  { TGTAsyncMethods }

  TGTAsyncMethods = class (TGTCustomThreadFIFO)
  protected
    procedure FreeItem(AData: Pointer); override;
  public
    procedure ProcessAll;
    procedure Push(AMethod: TGTAsyncMethod; Data: Pointer);
  end;

implementation

var
  TM: TThreadManager;

{ TGTCustomThreadFIFO }

constructor TGTCustomThreadFIFO.Create;
begin
  FTag := 0;
  FEmpty := True;
  FBottomElement := nil;
  FTopElement := nil;
  //FAvailableSemaphore := TM.SemaphoreInit;
  InitCriticalSection(FTopLock);
  InitCriticalSection(FBottomLock);
end;

destructor TGTCustomThreadFIFO.Destroy;
var
  Element, NewElement: PGTThreadFIFOElement;
begin
  EnterCriticalsection(FTopLock);
  EnterCriticalsection(FBottomLock);
  try
    Element := FBottomElement;
    while (Element <> nil) do
    begin
      NewElement := Element^.Up;
      FreeItem(Element^.Data);
      FreeMem(Element);
      Element := NewElement;
    end;
    FBottomElement := nil;
    FTopElement := nil;
    //TM.SemaphoreDestroy(FAvailableSemaphore);
  finally
    inherited Destroy;
    DoneCriticalsection(FTopLock);
    DoneCriticalsection(FBottomLock);
  end;
end;

procedure TGTCustomThreadFIFO.FreeItem(AData: Pointer);
begin

end;

procedure TGTCustomThreadFIFO.Clear;
var
  Element, NewElement: PGTThreadFIFOElement;
begin
  EnterCriticalSection(FTopLock);
  EnterCriticalSection(FBottomLock);
  try
    Element := FBottomElement;
    while (Element <> nil) do
    begin
      NewElement := Element^.Up;
      FreeItem(Element^.Data);
      FreeMem(Element);
      Element := NewElement;
    end;
    FBottomElement := nil;
    FTopElement := nil;
    FEmpty := True;
  finally
    LeaveCriticalSection(FBottomLock);
    LeaveCriticalSection(FTopLock);
  end;
end;

function TGTCustomThreadFIFO.Empty: Boolean;
begin
  Result := FEmpty;
end;

function TGTCustomThreadFIFO.Pop: Pointer;
var
  Element: PGTThreadFIFOElement;
begin
  EnterCriticalSection(FBottomLock);
  if FBottomElement = nil then
  begin
    Result := nil;
    LeaveCriticalSection(FBottomLock);
    Exit;
  end;
  try
    Result := FBottomElement^.Data;
    if FBottomElement^.Up = nil then
    begin
      EnterCriticalSection(FTopLock);
      FTopElement := nil;
      FEmpty := True;
      LeaveCriticalSection(FTopLock);
    end;
    Element := FBottomElement;
    FBottomElement := FBottomElement^.Up;
    FreeMem(Element);
  finally
    LeaveCriticalSection(FBottomLock);
  end;
end;

procedure TGTCustomThreadFIFO.Push(AData: Pointer);
var
  NewElement: PGTThreadFIFOElement;
begin
  NewElement := GetMem(SizeOf(TGTThreadFIFOElement));
  NewElement^.Up := nil;
  NewElement^.Data := AData;
  EnterCriticalSection(FTopLock);
  try
    if FTopElement = nil then
    begin
      EnterCriticalSection(FBottomLock);
      FBottomElement := NewElement;
      FEmpty := False;
      LeaveCriticalSection(FBottomLock);
    end
    else
      FTopElement^.Up := NewElement;
    FTopElement := NewElement;
    //TM.SemaphorePost(FAvailableSemaphore);
  finally
    LeaveCriticalSection(FTopLock);
  end;
end;

procedure TGTCustomThreadFIFO.WaitData;
begin
  while FEmpty do
    Sleep(1);
  //if FEmpty then
  //  TM.SemaphoreWait(FAvailableSemaphore);
end;

{ TGTAsyncMethods }

procedure TGTAsyncMethods.FreeItem(AData: Pointer);
begin
  FreeMem(AData);
end;

procedure TGTAsyncMethods.ProcessAll;
var
  List: TFPList;
  I: Integer;
  CurrElement, NextElement: PGTThreadFIFOElement;
  CurrCall: PGTAsyncMethodElement;
begin
  List := TFPList.Create;
  try
    EnterCriticalSection(FBottomLock);
    try
      // First, save all entries to a list so we do not have to wait for some
      // expensive calls while we have the list locked.
      CurrElement := FBottomElement;
      if CurrElement = nil then
        Exit;
      EnterCriticalSection(FTopLock);
      while CurrElement <> nil do
      begin
        List.Add(CurrElement^.Data);
        NextElement := CurrElement^.Up;
        FreeMem(CurrElement);
        CurrElement := NextElement;
      end;
      FTopElement := nil;
      FBottomElement := nil;
      LeaveCriticalSection(FTopLock);
    finally
      LeaveCriticalSection(FBottomLock);
    end;
    for I := 0 to List.Count - 1 do
    begin
      CurrCall := PGTAsyncMethodElement(List[I]);
      CurrCall^.Method(CurrCall^.Data);
      FreeMem(CurrCall);
    end;
  finally
    List.Free;
  end;
end;

procedure TGTAsyncMethods.Push(AMethod: TGTAsyncMethod; Data: Pointer);
var
  Element: PGTAsyncMethodElement;
begin
  GetMem(Element, SizeOf(TGTAsyncMethodElement));
  Element^.Data := Data;
  Element^.Method := AMethod;
  inherited Push(Element);
end;

initialization
GetThreadManager(TM);

end.

