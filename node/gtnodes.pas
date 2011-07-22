(*******************************************************************************
** File Name: gtnodes.pas
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
unit GTNodes;

{$mode objfpc}{$H+}
{$inline off}
{.$define DebugMsg}

interface

uses
  Classes, SysUtils, GTBase, fgl, GTThreadFIFO, typinfo, GTDebug, syncobjs;

const
  NODE_THREAD_COMMAND_NONE =        $00;
  NODE_THREAD_COMMAND_SETUP_IO =    $01;
  NODE_THREAD_COMMAND_INIT =        $02;
  NODE_THREAD_COMMAND_BURN =        $03;
  NODE_THREAD_COMMAND_PAUSE =       $10;

type
  EGTNodeError = class(EGTError);
    EGTNodeTypeError = class (EGTNodeError);
    EGTNodePortError = class (EGTNodeError);
    EGTNodeInvalidConstructor = class(EGTNodeError);
    EGTNodeLockError = class (EGTNodeError);

  TGTNodeThread = class;
  TGTNode = class;
  TGTNodePortNumber = SizeUInt;
  TGTNodeOutPort = class;
  TGTNodeDataSet = PPointer;
  TGTNodeOvermind = class;

  TGTNodeOvermindState = (osUnlocked, osInitialized, osLocked);
  TGTNodeTypeState = (tsBurned, tsParametrized, tsInitialized);
  TGTNodeThreadState = (nsBurned, nsIOSetup, nsInitialized);

  { TGTNodeDataType }

  TGTNodeDataType = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FOnParametrize: TNotifyEvent;
    FState: TGTNodeTypeState;
  protected
    procedure ForceState(const AState: TGTNodeTypeState);
  public
    procedure Burn; virtual;
    procedure FreeItem(var AItem: Pointer); virtual; abstract;
    function GetItem: Pointer; virtual; abstract;
    function GetSize: SizeUInt; virtual; abstract;
    procedure Init; virtual;
    procedure Parametrize; virtual;
  published
    property OnParametrize: TNotifyEvent read FOnParametrize write FOnParametrize;
  end;
  TGTNodeDataTypeClass = class of TGTNodeDataType;
  TGTNodeDataTypes = specialize TFPGList<TGTNodeDataType>;

  { TGTNodeOutFIFO }

  TGTNodeOutFIFO = class (TGTThreadFIFO)
  public
    constructor Create(const AThread: TGTNodeThread;
      const ADataType: TGTNodeDataType);
    destructor Destroy; override;
  private
    FDataType: TGTNodeDataType;
    FThread: TGTNodeThread;
  protected
    procedure FreeItem(AData: Pointer); override;
  public
    property DataType: TGTNodeDataType read FDataType;
  end;

  { TGTNodeInFIFO }

  TGTNodeInFIFO = class (TGTNodeOutFIFO)
  public
    constructor Create(const AThread: TGTNodeThread;
      const ADataTypeClass: TGTNodeDataTypeClass);
  private
    FDataTypeClass: TGTNodeDataTypeClass;
    procedure SetDataType(const AValue: TGTNodeDataType);
  public
    property DataType: TGTNodeDataType read FDataType write SetDataType;
    property DataTypeClass: TGTNodeDataTypeClass read FDataTypeClass;
  end;

  { TGTNodePort }

  TGTNodePort = class (TObject)
  public
    constructor Create(const AOwner: TGTNode; const ANumber: TGTNodePortNumber);
  private
    FNumber: TGTNodePortNumber;
    FOwner: TGTNode;
  protected
    procedure ForceState(const AState: TGTNodeOvermindState);
    procedure ForceMaxState(const AState: TGTNodeOvermindState);
    procedure ForceMinState(const AState: TGTNodeOvermindState);
  published
    property Number: TGTNodePortNumber read FNumber;
    property Owner: TGTNode read FOwner;
  end;

  { TGTNodeInPort }

  TGTNodeInPort = class (TGTNodePort)
  public
    constructor Create(const AOwner: TGTNode; const AInPipe: TGTNodeInFIFO;
      const ANumber: TGTNodePortNumber);
    destructor Destroy; override;
  private
    FInPipe: TGTNodeInFIFO;
    FOnChange: TNotifyEvent;
    FSource: TGTNodeOutPort;
    procedure SetSource(const AValue: TGTNodeOutPort);
  published
    property InPipe: TGTNodeInFIFO read FInPipe;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Source: TGTNodeOutPort read FSource write SetSource;
  end;
  TGTNodeInPorts = specialize TFPGList<TGTNodeInPort>;

  { TGTNodeOutPort }

  TGTNodeOutPort = class (TGTNodePort)
  public
    constructor Create(const AOwner: TGTNode; const AOutPipe: TGTNodeOutFIFO;
      const ANumber: TGTNodePortNumber);
    destructor Destroy; override;
  private
    FDestPorts: TGTNodeInPorts;
    FOutPipe: TGTNodeOutFIFO;
  protected
    procedure DataPushed(Sender: TObject);
    procedure DestDeleting(Sender: TObject);
    procedure Link(const APort: TGTNodeInPort);
    procedure Unlink(const APort: TGTNodeInPort);
    procedure Unlink(const AIndex: Integer);
    procedure UnlinkAll;
  public
    procedure Push;
  published
    property OutPipe: TGTNodeOutFIFO read FOutPipe;
  end;

  {$M+}

  { TGTNodeThread }

  TGTNodeThread = class (TThread)
  public
    constructor Create(const AOvermind: TGTNodeOvermind;
      const AOwnerNode: TGTNode); virtual;
    destructor Destroy; override;
  private
    FSuspendAfterCommand: Boolean;
    FState: TGTNodeThreadState;

    FOwner: TGTNode;
    FOvermind: TGTNodeOvermind;
    FInData, FOutData: TGTNodeDataSet;
    FInCount, FOutCount: TGTNodePortNumber;
  protected
    FCommandLock: TCriticalSection;
    FNextCommand: Cardinal;
    FInPorts: array of TGTNodeInFIFO;
    FOutPorts: array of TGTNodeOutFIFO;
  protected
    procedure CheckCommand;
    function GetOwner: TGTNode;
    function GetOvermind: TGTNodeOvermind;
    procedure HandleCommand(const ACommand: Cardinal); virtual;
    procedure CommandSuspend; virtual;
    procedure SetupInPorts(const ATypes: array of TGTNodeDataTypeClass);
    procedure SetupOutPorts(const ATypes: array of TGTNodeDataType);
  protected
    procedure Burn; virtual;
    procedure Init; virtual;
    function ProcessDataSet(const AInputData: TGTNodeDataSet;
      const AOutputData: TGTNodeDataSet): Boolean; virtual; abstract;
    function RequireInput(const AInput: TGTNodePortNumber): Boolean; virtual;
    procedure SetupIO; virtual; // note that SetupIO suspends the thread!
  public
    procedure DoResume; virtual;
    procedure Execute; override;
    procedure PostCommand(const ACommand: Cardinal; const ASuspendAfterCommand: Boolean = False; const ARaiseIfPending: Boolean = True);
    procedure WaitForCommand; virtual;
  public
    class function GetName: String; virtual; abstract;
    class function GetDescription: String; virtual;
  end;
  {$M-}

  TGTNodeThreadClass = class of TGTNodeThread;
  TGTNodeThreads = specialize TFPGList<TGTNodeThread>;

  { TGTTypedNodeThread }

  generic TGTTypedNodeThread<TOvermind> = class (TGTNodeThread)
  public
    constructor Create(const AOvermind: TGTNodeOvermind;
       const AOwnerNode: TGTNode); override;
  protected
    function GetOvermind: TOvermind;
  end;

  { TGTNode }

  TGTNode = class (TGTBaseObject)
  public
    constructor Create(const AOvermind: TGTNodeOvermind;
      const AProcessorThread: TGTNodeThreadClass;
      const AOwnsThread: Boolean = True); virtual;
    destructor Destroy; override;
  private
    FInPorts: array of TGTNodeInPort;
    FProcessorThread: TGTNodeThread;
    FOutPorts: array of TGTNodeOutPort;
    FOvermind: TGTNodeOvermind;
    FOwnsThread: Boolean;
    function GetInPort(AIndex: Integer): TGTNodeInPort;
    function GetInPortCount: TGTNodePortNumber;
    function GetOutPort(AIndex: Integer): TGTNodeOutPort;
    function GetPortCount: TGTNodePortNumber;
  protected
    procedure DoChange; override;
    procedure ForceState(const AState: TGTNodeOvermindState);
    procedure ForceMaxState(const AState: TGTNodeOvermindState);
    procedure ForceMinState(const AState: TGTNodeOvermindState);
    procedure RaiseOutOfBounds(const AIndex: String; const AValue, AMin, AMax: Integer);
    procedure RequireOvermind;
    procedure SetupInPorts(const ACount: Integer);
    procedure SetupOutPorts(const ACount: Integer);
    procedure SourceConnected(Sender: TObject);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    property InPort[AIndex: Integer]: TGTNodeInPort read GetInPort;
    property InPortCount: TGTNodePortNumber read GetInPortCount;
    property Port[AIndex: Integer]: TGTNodeOutPort read GetOutPort;
    property PortCount: TGTNodePortNumber read GetPortCount;
  published
    property ProcessorThread: TGTNodeThread read FProcessorThread;
    property OwnsThread: Boolean read FOwnsThread write FOwnsThread;
  end;
  TGTNodeClass = class of TGTNode;
  TGTNodeList = specialize TFPGList<TGTNode>;

  { TGTNodeOvermind }

  TGTNodeOvermind = class (TGTBaseObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FNodes: TGTNodeList;
    FOnNodeCreated: TGTNodeEventList;
    FOnNodeDeleting: TGTNodeEventList;
    FOnNodeUpdated: TGTNodeEventList;
    FState: TGTNodeOvermindState;
  protected
    procedure DoNodeCreated(const ANode: TGTNode);
    procedure DoNodeDeleting(const ANode: TGTNode);
    procedure DoNodeUpdated(const ANode: TGTNode);
  public
    procedure BeforeDestruction; override;
    procedure DeleteAllNodes;
    procedure ForceState(const AState: TGTNodeOvermindState);
    procedure ForceMaxState(const AState: TGTNodeOvermindState);
    procedure ForceMinState(const AState: TGTNodeOvermindState);
    procedure Init;
    procedure Lock;
    function NewNode(const AThread: TGTNodeThreadClass;
      const AOwnsThread: Boolean = True;
      const ANodeClass: TGTNodeClass = nil): TGTNode;
    procedure PauseAll;
    procedure ResumeAll;

    procedure Unlock;
  public
    property OnNodeCreated: TGTNodeEventList read FOnNodeCreated;
    property OnNodeDeleting: TGTNodeEventList read FOnNodeDeleting;
    property OnNodeUpdated: TGTNodeEventList read FOnNodeUpdated;
  end;

implementation

function InitDataSet(const Len: TGTNodePortNumber): TGTNodeDataSet;
begin
  Result := GetMem(Len * SizeOf(Pointer));
  FillByte(Result^, Len * SizeOf(Pointer), 0);
end;

procedure BurnDataSet(var ADataSet: TGTNodeDataSet);
begin
  if ADataSet = nil then
    Exit;
  FreeMem(ADataSet);
  ADataSet := nil;
end;

var
  TM: TThreadManager;
  TMInitialized: Boolean;

{ TGTNodeDataType }

constructor TGTNodeDataType.Create;
begin
  {$ifdef DebugMsg}
  DebugMsg('Creating', [], Self);
  {$endif}
  inherited Create;
end;

destructor TGTNodeDataType.Destroy;
begin
  inherited Destroy;
  {$ifdef DebugMsg}
  DebugMsg('Destructed', [], Self);
  {$endif}
end;

procedure TGTNodeDataType.ForceState(const AState: TGTNodeTypeState);
begin
  if FState <> AState then
    raise EGTNodeLockError.CreateFmt('Invalid state (%s) for this operation (%s required).', [GetEnumName(TypeInfo(TGTNodeTypeState), Ord(FState)), GetEnumName(TypeInfo(TGTNodeTypeState), Ord(AState))]);
end;

procedure TGTNodeDataType.Burn;
begin
  ForceState(tsInitialized);
  FState := tsBurned;
  {$ifdef DebugMsg}
  DebugMsg('Burned', [], Self);
  {$endif}
end;

procedure TGTNodeDataType.Init;
begin
  if FState = tsBurned then
    Parametrize;
  ForceState(tsParametrized);
  FState := tsInitialized;
  {$ifdef DebugMsg}
  DebugMsg('Initialized', [], Self);
  {$endif}
end;

procedure TGTNodeDataType.Parametrize;
begin
  if FState >= tsParametrized then
    Exit;
  ForceState(tsBurned);
  if FOnParametrize <> nil then
    FOnParametrize(Self);
  FState := tsParametrized;
end;

{ TGTNodeOutFIFO }

constructor TGTNodeOutFIFO.Create(const AThread: TGTNodeThread;
  const ADataType: TGTNodeDataType);
begin
  inherited Create;
  FDataType := ADataType;
  FThread := AThread;
end;

destructor TGTNodeOutFIFO.Destroy;
begin
  {$ifdef DebugMsg}
  GTDebug.DebugMsg('Destructing (FDataType = %16.16x)', [Int64(FDataType)], Self);
  {$endif}
  inherited Destroy;
  {$ifdef DebugMsg}
  GTDebug.DebugMsg('Destructed', [], Self);
  {$endif}
end;

procedure TGTNodeOutFIFO.FreeItem(AData: Pointer);
begin
  {$ifdef DebugMsg}
  GTDebug.DebugMsg('FreeItem (FDataType = %16.16x; AData = %16.16x)', [Int64(FDataType), Int64(AData)], Self);
  {$endif}
  FDataType.FreeItem(AData);
end;

{ TGTNodeInFIFO }

constructor TGTNodeInFIFO.Create(const AThread: TGTNodeThread;
  const ADataTypeClass: TGTNodeDataTypeClass);
begin
  inherited Create(AThread, nil);
  FDataTypeClass := ADataTypeClass;
end;

procedure TGTNodeInFIFO.SetDataType(const AValue: TGTNodeDataType);
begin
  if FDataType = AValue then exit;
  if FDataType <> nil then
  begin
    FDataType := nil;
  end;
  if AValue = nil then
    Exit;
  if not (AValue.ClassType = FDataTypeClass) then
    raise EGTNodeTypeError.CreateFmt('Node requires %s type, got %s.', [FDataTypeClass.ClassName, AValue.ClassName]);
  FDataType := AValue;
end;

{ TGTNodePort }

constructor TGTNodePort.Create(const AOwner: TGTNode;
  const ANumber: TGTNodePortNumber);
begin
  FOwner := AOwner;
  FNumber := ANumber;
end;

procedure TGTNodePort.ForceState(const AState: TGTNodeOvermindState);
begin
  FOwner.ForceState(AState);
end;

procedure TGTNodePort.ForceMaxState(const AState: TGTNodeOvermindState);
begin
  FOwner.ForceMaxState(AState);
end;

procedure TGTNodePort.ForceMinState(const AState: TGTNodeOvermindState);
begin
  FOwner.ForceMinState(AState);
end;

{ TGTNodeInPort }

constructor TGTNodeInPort.Create(const AOwner: TGTNode;
  const AInPipe: TGTNodeInFIFO; const ANumber: TGTNodePortNumber);
begin
  inherited Create(AOwner, ANumber);
  FInPipe := AInPipe;
  FSource := nil;
end;

destructor TGTNodeInPort.Destroy;
begin
  ForceState(osUnlocked);
  SetSource(nil);
  inherited Destroy;
end;

procedure TGTNodeInPort.SetSource(const AValue: TGTNodeOutPort);
begin
  if FSource = AValue then exit;
  if FSource <> nil then
  begin
    FSource.Unlink(Self);
    //UnlinkSource(FSource);
    FSource := nil;
    FInPipe.DataType := nil;
  end;
  if not (AValue.OutPipe.DataType.ClassType = FInPipe.DataTypeClass) then
    raise EGTNodeTypeError.CreateFmt('Incompatible types. Attempt to connect an out port of type %s to an in port of type %s.', [AValue.OutPipe.DataType.ClassName, FInPipe.DataTypeClass.ClassName]);
  FSource := AValue;
  if FSource <> nil then
  begin
    //LinkSource(FSource);
    FInPipe.DataType := FSource.OutPipe.DataType;
    FSource.Link(Self);
  end;
  if FOnChange <> nil then
    FOnChange(Self);
end;

{ TGTNodeOutPort }

constructor TGTNodeOutPort.Create(const AOwner: TGTNode;
  const AOutPipe: TGTNodeOutFIFO; const ANumber: TGTNodePortNumber);
begin
  inherited Create(AOwner, ANumber);
  FDestPorts := TGTNodeInPorts.Create;
  FOutPipe := AOutPipe;
  FOutPipe.OnPush := @DataPushed;
end;

destructor TGTNodeOutPort.Destroy;
begin
  ForceState(osUnlocked);
  UnlinkAll;
  FDestPorts.Free;
  inherited Destroy;
end;

procedure TGTNodeOutPort.DataPushed(Sender: TObject);
begin
  // This may execute in an arbitary thread!
  Push;
end;


procedure TGTNodeOutPort.DestDeleting(Sender: TObject);
begin
  ForceMaxState(osInitialized);
  Unlink(Sender as TGTNodeInPort);
end;

procedure TGTNodeOutPort.Link(const APort: TGTNodeInPort);
var
  I: Integer;
begin
  ForceMaxState(osInitialized);
  I := FDestPorts.IndexOf(APort);
  if I >= 0 then
    Exit;
  if APort.InPipe.DataTypeClass <> FOutPipe.DataType.ClassType then
    raise EGTNodeTypeError.CreateFmt('Incompatible types. Attempt to connect an out port of type %s to an in port of type %s.', [FOutPipe.DataType.ClassName, APort.InPipe.DataTypeClass.ClassName]);
  FDestPorts.Add(APort);
  APort.Source := Self;
end;

procedure TGTNodeOutPort.Unlink(const APort: TGTNodeInPort);
var
  I: Integer;
begin
  ForceMaxState(osInitialized);
  I := FDestPorts.IndexOf(APort);
  if I < 0 then
    Exit;
  Unlink(I);
end;

procedure TGTNodeOutPort.Unlink(const AIndex: Integer);
var
  Port: TGTNodeInPort;
begin
  ForceMaxState(osInitialized);
  Port := FDestPorts[AIndex];
  FDestPorts.Delete(AIndex);
  Port.Source := nil;
end;

procedure TGTNodeOutPort.UnlinkAll;
begin
  ForceMaxState(osInitialized);
  while FDestPorts.Count > 0 do
    Unlink(0);
end;

procedure TGTNodeOutPort.Push;
var
  I: Integer;
  Dest: TGTNodeInPort;
  Size: SizeUInt;
  Duplicate: Pointer;
  Item: Pointer;
  First: Boolean;
begin
  ForceState(osLocked);
  if FDestPorts.Count = 0 then
  begin
    Item := FOutPipe.Pop;
    while Item <> nil do
    begin
      FOutPipe.DataType.FreeItem(Item);
      Item := FOutPipe.Pop;
    end;
    Exit;
  end;
  Item := FOutPipe.Pop;
  while Item <> nil do
  begin
    Size := FOutPipe.DataType.GetSize;
    for I := 1 to FDestPorts.Count - 1 do
    begin
      Duplicate := GetMem(Size);
      Move(Item^, Duplicate^, Size);
      FDestPorts[I].FInPipe.Push(Duplicate);
    end;
    FDestPorts[0].FInPipe.Push(Item);
    Item := FOutPipe.Pop;
  end;
end;

{ TGTNodeThread }

constructor TGTNodeThread.Create(const AOvermind: TGTNodeOvermind;
  const AOwnerNode: TGTNode);
begin
  if not TMInitialized then
    GetThreadManager(TM);
  inherited Create(True);
  FCommandLock := TCriticalSection.Create;
  FNextCommand := NODE_THREAD_COMMAND_NONE;
  FSuspendAfterCommand := False;
  FOvermind := AOvermind;
  FOwner := AOwnerNode;
  {$ifdef DebugMsg}
  DebugMsg('Created (%16.16x)', [Int64(FResetSemaphore)], Self);
  {$endif}
  SetupInPorts([]);
  SetupOutPorts([]);
  FState := nsBurned;
end;

destructor TGTNodeThread.Destroy;
var
  I: Integer;
begin
  {$ifdef DebugMsg}
  DebugMsg('Destructing (%16.16x)', [Int64(FResetSemaphore)], Self);
  {$endif}
  for I := 0 to High(FInPorts) do
  begin
    {$ifdef DebugMsg}
    DebugMsg('Attempt to free InFIFO at 0x%16.16x', [Int64(FInPorts[I])], Self);
    {$endif}
    FInPorts[I].Free;
  end;
  for I := 0 to High(FOutPorts) do
    FOutPorts[I].Free;
  inherited Destroy;
  {$ifdef DebugMsg}
  DebugMsg('Destructed', [], Self);
  {$endif}
end;

procedure TGTNodeThread.CheckCommand;
begin
  FCommandLock.Acquire;
  try
    if FNextCommand <> NODE_THREAD_COMMAND_NONE then
    begin
      case FNextCommand of
        NODE_THREAD_COMMAND_SETUP_IO:
        begin
          if FState <> nsBurned then
            Exit;
          SetupIO;
        end;
        NODE_THREAD_COMMAND_INIT:
        begin
          if FState > nsIOSetup then
            Exit;
          if FState = nsBurned then
            SetupIO;
          Init;
        end;
        NODE_THREAD_COMMAND_BURN:
        begin
          if FState <> nsInitialized then
            Exit;
          Burn;
        end;
        NODE_THREAD_COMMAND_PAUSE:
        begin
          if FState <> nsInitialized then
            Exit;
          FSuspendAfterCommand := True;
        end;
      else
        HandleCommand(FNextCommand);
      end;
    end;
  finally
    FNextCommand := NODE_THREAD_COMMAND_NONE;
    if FSuspendAfterCommand then
    begin
      FSuspendAfterCommand := False;
      FCommandLock.Release;
      CommandSuspend;
    end
    else
      FCommandLock.Release;
  end;
  // Always check for a new command!
  CheckCommand;
end;

function TGTNodeThread.GetOwner: TGTNode;
begin
  Exit(FOwner);
end;

function TGTNodeThread.GetOvermind: TGTNodeOvermind;
begin
  Exit(FOvermind);
end;

procedure TGTNodeThread.HandleCommand(const ACommand: Cardinal);
begin
  raise EGTNodeError.CreateFmt('Unknown command: %d', [ACommand]);
end;

procedure TGTNodeThread.CommandSuspend;
begin
  Suspend;
end;

procedure TGTNodeThread.SetupInPorts(
  const ATypes: array of TGTNodeDataTypeClass);
var
  I, L, Count: Integer;
begin
  Count := Length(ATypes);
  L := Length(FInPorts);
  if L = Count then
    Exit;
  if L > Count then
  begin
    for I := Count to High(FInPorts) do
      FInPorts[I].Free;
  end;
  SetLength(FInPorts, Count);
  if L < Count then
  begin
    for I := L to High(FInPorts) do
      FInPorts[I] := TGTNodeInFIFO.Create(Self, ATypes[I]);
  end;
  for I := 0 to L-1 do
  begin
    if FInPorts[I].DataTypeClass <> ATypes[I] then
    begin
      FInPorts[I].Free;
      FInPorts[I] := TGTNodeInFIFO.Create(Self, ATypes[I]);
    end;
  end;
  FOwner.SetupInPorts(Count);
end;

procedure TGTNodeThread.SetupOutPorts(
  const ATypes: array of TGTNodeDataType);
var
  I, L, Count: Integer;
begin
  Count := Length(ATypes);
  L := Length(FOutPorts);
  if L = Count then
    Exit;
  if L > Count then
  begin
    for I := Count to High(FOutPorts) do
      FOutPorts[I].Free;
  end;
  SetLength(FOutPorts, Count);
  if L < Count then
  begin
    for I := L to High(FOutPorts) do
      FOutPorts[I] := TGTNodeOutFIFO.Create(Self, ATypes[I]);
  end;
  for I := 0 to L-1 do
  begin
    if FOutPorts[I].DataType <> ATypes[I] then
    begin
      FOutPorts[I].Free;
      FOutPorts[I] := TGTNodeOutFIFO.Create(Self, ATypes[I]);
    end;
  end;
  FOwner.SetupOutPorts(Count);
end;

procedure TGTNodeThread.Burn;
var
  I: Integer;
begin
  {$ifdef DebugMsg}
  DebugMsg('Burning', [], Self);
  {$endif}
  for I := 0 to High(FInPorts) do
  begin
    if FInData[I] <> nil then
      FInPorts[I].DataType.FreeItem(FInData[I]);
    FInPorts[I].Clear;
  end;
  for I := 0 to High(FOutPorts) do
    FOutPorts[I].Clear;
  BurnDataSet(FInData);
  BurnDataSet(FOutData);
  FState := nsBurned;
end;

procedure TGTNodeThread.Init;
begin
  FInData := InitDataSet(FInCount);
  FOutData := InitDataSet(FOutCount);
  FState := nsInitialized;
end;

function TGTNodeThread.RequireInput(const AInput: TGTNodePortNumber): Boolean;
begin
  Exit(True);
end;

procedure TGTNodeThread.SetupIO;
begin
  FInCount := Length(FInPorts);
  FOutCount := Length(FOutPorts);
  {$ifdef DebugMsg}
  DebugMsg('SetupIO', [], Self);
  {$endif}
  FState := nsIOSetup;
end;

procedure TGTNodeThread.DoResume;
begin
  Resume;
end;

procedure TGTNodeThread.Execute;
var
  I: Integer;
  Got: TGTNodePortNumber;
begin
  try
    try
      while not Terminated do
      begin
        CheckCommand;
        if Terminated then
          Exit;
        if FState <> nsInitialized then
          Continue;
        Got := 0;
        while Got < FInCount do
        begin
          CheckCommand;
          if Terminated then
            Exit;
          if FState <> nsInitialized then
            Break;
          Got := 0;
          for I := 0 to FInCount - 1 do
          begin
            if (FInData[I] = nil) then
            begin
              if FInPorts[I].DataType <> nil then
                FInData[I] := FInPorts[I].Pop;
              if FInData[I] <> nil then
                Inc(Got)
              else if not RequireInput(I) then
                Inc(Got);
            end
            else
              Inc(Got);
          end;
          ThreadSwitch;
        end;
        if FState <> nsInitialized then
          Continue;
        if ProcessDataSet(FInData, FOutData) then
        begin
          for I := 0 to FOutCount - 1 do
          begin
            FOutPorts[I].Push(FOutData[I]);
          end;
        end;
        for I := 0 to FInCount - 1 do
          FInData[I] := nil;
        ThreadSwitch;
      end;
    finally
      {$ifdef DebugMsg}
      DebugMsg('Thread terminating (FResetTerminated = %d)', [Integer(FResetTerminated)], Self);
      {$endif}
      case FState of
        nsIOSetup:
        begin
          Init;
          Burn;
        end;
        nsInitialized:
        begin
          Burn;
        end;
      end;
    end;
    {$ifdef DebugMsg}
    DebugMsg('Thread terminated gracefully.', [], Self);
    {$endif}
  except
    on E: Exception do
    begin
      WriteLn('Thread ', ToString, ' died with ', E.Message);
    end;
  end;
end;

procedure TGTNodeThread.PostCommand(const ACommand: Cardinal;
  const ASuspendAfterCommand: Boolean; const ARaiseIfPending: Boolean);
begin
  FCommandLock.Acquire;
  try
    if FNextCommand <> NODE_THREAD_COMMAND_NONE then
    begin
      if ARaiseIfPending then
        raise EGTNodeError.CreateFmt('Command queue is not empty (%d waits to be run).', [FNextCommand])
      else
      begin
        FCommandLock.Release;
        WaitForCommand;
        FCommandLock.Acquire;
      end;
    end;
    FNextCommand := ACommand;
    FSuspendAfterCommand := ASuspendAfterCommand;
  finally
    FCommandLock.Release;
  end;
  DoResume;
end;

procedure TGTNodeThread.WaitForCommand;
var
  Done: Boolean;
begin
  Done := False;
  repeat
    FCommandLock.Acquire;
    if FNextCommand <> NODE_THREAD_COMMAND_NONE then
      Done := True;
    FCommandLock.Release;
    if not Done then
      Sleep(1);
  until Done;
end;

class function TGTNodeThread.GetDescription: String;
begin
  Exit('');
end;

{ TGTTypedNodeThread }

constructor TGTTypedNodeThread.Create(const AOvermind: TGTNodeOvermind;
  const AOwnerNode: TGTNode);
begin
  if not (AOvermind.InheritsFrom(TClass(TOvermind))) then
    raise EGTNodeError.CreateFmt('%s requires overmind inheriting from %s (got %s).', [ToString, TOvermind.ClassName, AOvermind.ToString]);
  inherited Create(AOvermind, AOwnerNode);
end;

function TGTTypedNodeThread.GetOvermind: TOvermind;
begin
  Exit(TOvermind(inherited GetOvermind));
end;

{ TGTNode }

constructor TGTNode.Create(const AOvermind: TGTNodeOvermind;
  const AProcessorThread: TGTNodeThreadClass; const AOwnsThread: Boolean);
begin
  if AOvermind = nil then
    RequireOvermind;
  inherited Create;
  if AProcessorThread <> nil then
    FProcessorThread := AProcessorThread.Create(AOvermind, Self);
  FOwnsThread := AOwnsThread;
  FOvermind := AOvermind;
end;

destructor TGTNode.Destroy;
begin
  ForceState(osUnlocked);
  {$ifdef DebugMsg}
  DebugMsg('Destructing', [], Self);
  {$endif}
  if FOwnsThread then
  begin
    {$ifdef DebugMsg}
    DebugMsg('Terminating my thread (0x%16.16x (%s)) gracefully.', [Int64(FProcessorThread), FProcessorThread.ToString], Self);
    {$endif}
    FProcessorThread.Terminate;
    Sleep(1);
    FProcessorThread.Resume;
    FProcessorThread.WaitFor;
    FProcessorThread.Free;
  end
  else
  begin
    {$ifdef DebugMsg}
    DebugMsg('Not owning the thread; Resetting my IO.', [], Self);
    {$endif}
    FProcessorThread := nil;
    SetupInPorts(0);
    SetupOutPorts(0);
  end;
  inherited Destroy;
end;

function TGTNode.GetInPort(AIndex: Integer): TGTNodeInPort;
begin
  if (AIndex < 0) or (AIndex > High(FInPorts)) then
    RaiseOutOfBounds('In port', AIndex, 0, High(FInPorts));
  Exit(FInPorts[AIndex]);
end;

function TGTNode.GetInPortCount: TGTNodePortNumber;
begin
  Exit(Length(FInPorts));
end;

function TGTNode.GetOutPort(AIndex: Integer): TGTNodeOutPort;
begin
  if (AIndex < 0) or (AIndex > High(FOutPorts)) then
    RaiseOutOfBounds('Out port', AIndex, 0, High(FOutPorts));
  Exit(FOutPorts[AIndex]);
end;

function TGTNode.GetPortCount: TGTNodePortNumber;
begin
  Exit(Length(FOutPorts));
end;

procedure TGTNode.DoChange;
begin
  inherited DoChange;
  FOvermind.DoNodeUpdated(Self);
end;

procedure TGTNode.ForceState(const AState: TGTNodeOvermindState);
begin
  FOvermind.ForceState(AState);
end;

procedure TGTNode.ForceMaxState(const AState: TGTNodeOvermindState);
begin
  FOvermind.ForceMaxState(AState);
end;

procedure TGTNode.ForceMinState(const AState: TGTNodeOvermindState);
begin
  FOvermind.ForceMinState(AState);
end;

procedure TGTNode.RaiseOutOfBounds(const AIndex: String; const AValue, AMin,
  AMax: Integer);
begin
  raise EListError.CreateFmt('%s index (%d) out of bounds (%d..%d).', [AIndex, AValue, AMin, AMax]);
end;

procedure TGTNode.RequireOvermind;
begin
  raise EGTNodeError.Create('Overmind required.');
end;

procedure TGTNode.SetupInPorts(const ACount: Integer);
var
  I, L: Integer;
begin
  L := Length(FInPorts);
  if L = ACount then
    Exit;
  ForceState(osUnlocked);
  if L > ACount then
  begin
    for I := ACount to High(FInPorts) do
      FInPorts[I].Free;
  end;
  SetLength(FInPorts, ACount);
  if L < ACount then
  begin
    for I := L to High(FInPorts) do
    begin
      FInPorts[I] := TGTNodeInPort.Create(Self, FProcessorThread.FInPorts[I], I);
      FInPorts[I].OnChange := @SourceConnected;
    end;
  end;
  for I := 0 to L-1 do
  begin
    if FInPorts[I].FInPipe <> FProcessorThread.FInPorts[I] then
    begin
      FInPorts[I].Free;
      FInPorts[I] := TGTNodeInPort.Create(Self, FProcessorThread.FInPorts[I], I);
      FInPorts[I].OnChange := @SourceConnected;
    end;
  end;
  DoChange;
end;

procedure TGTNode.SetupOutPorts(const ACount: Integer);
var
  I, L: Integer;
begin
  L := Length(FOutPorts);
  if L = ACount then
    Exit;
  ForceState(osUnlocked);
  if L > ACount then
  begin
    for I := ACount to High(FOutPorts) do
      FOutPorts[I].Free;
  end;
  SetLength(FOutPorts, ACount);
  if L < ACount then
  begin
    for I := L to High(FOutPorts) do
      FOutPorts[I] := TGTNodeOutPort.Create(Self, FProcessorThread.FOutPorts[I], I);
  end;
  for I := 0 to L-1 do
  begin
    if FOutPorts[I].FOutPipe <> FProcessorThread.FOutPorts[I] then
    begin
      FOutPorts[I].Free;
      FOutPorts[I] := TGTNodeOutPort.Create(Self, FProcessorThread.FOutPorts[I], I);
    end;
  end;
  DoChange;
end;

procedure TGTNode.SourceConnected(Sender: TObject);
begin
  DoChange;
end;

procedure TGTNode.AfterConstruction;
begin
  inherited AfterConstruction;
  FOvermind.DoNodeCreated(Self);
end;

procedure TGTNode.BeforeDestruction;
begin
  FOvermind.DoNodeDeleting(Self);
  inherited BeforeDestruction;
end;

{ TGTNodeOvermind }

constructor TGTNodeOvermind.Create;
begin
  inherited Create;
  FNodes := TGTNodeList.Create;
  FOnNodeCreated := TGTNodeEventList.Create;
  FOnNodeDeleting := TGTNodeEventList.Create;
  FState := osUnlocked;
end;

destructor TGTNodeOvermind.Destroy;
begin
  DeleteAllNodes;
  FOnNodeDeleting.Free;
  FOnNodeCreated.Free;
  FNodes.Free;
  inherited Destroy;
end;

procedure TGTNodeOvermind.DoNodeCreated(const ANode: TGTNode);
begin
  FNodes.Add(ANode);
  FOnNodeCreated.Call(Self, ANode);
end;

procedure TGTNodeOvermind.DoNodeDeleting(const ANode: TGTNode);
begin
  FOnNodeDeleting.Call(Self, ANode);
  FNodes.Remove(ANode);
end;

procedure TGTNodeOvermind.DoNodeUpdated(const ANode: TGTNode);
begin
  FOnNodeUpdated.Call(Self, ANode);
end;

procedure TGTNodeOvermind.BeforeDestruction;
begin
  ForceMaxState(osInitialized);
  inherited BeforeDestruction;
end;

procedure TGTNodeOvermind.DeleteAllNodes;
begin
  ForceState(osUnlocked);
  while FNodes.Count > 0 do
    FNodes[0].Free;
end;

procedure TGTNodeOvermind.ForceState(const AState: TGTNodeOvermindState);
begin
  if FState <> AState then
    raise EGTNodeLockError.CreateFmt('%s state required.', [Copy(GetEnumName(TypeInfo(AState), Ord(AState)), 3, 100)]);
end;

procedure TGTNodeOvermind.ForceMaxState(const AState: TGTNodeOvermindState);
begin
  if FState > AState then
    raise EGTNodeLockError.CreateFmt('State below or equal to %s required.', [Copy(GetEnumName(TypeInfo(AState), Ord(AState)), 2, 100)]);
end;

procedure TGTNodeOvermind.ForceMinState(const AState: TGTNodeOvermindState);
begin
  if FState < AState then
    raise EGTNodeLockError.CreateFmt('State above or equal to %s required.', [Copy(GetEnumName(TypeInfo(AState), Ord(AState)), 2, 100)]);
end;

procedure TGTNodeOvermind.Init;
var
  Node: TGTNode;
begin
  ForceState(osUnlocked);
  for Node in FNodes do
    Node.FProcessorThread.PostCommand(NODE_THREAD_COMMAND_SETUP_IO, True);
  for Node in FNodes do
    Node.FProcessorThread.WaitForCommand;
  Sleep(1);
  FState := osInitialized;
end;

procedure TGTNodeOvermind.Lock;
var
  Node: TGTNode;
  DataType: TGTNodeDataType;
begin
  if FState = osLocked then
    Exit;
  ForceState(osInitialized);
  FState := osLocked;
  for Node in FNodes do
    Node.FProcessorThread.PostCommand(NODE_THREAD_COMMAND_INIT, True);
  for Node in FNodes do
    Node.FProcessorThread.WaitForCommand;
  for Node in FNodes do
    Node.FProcessorThread.DoResume;
  Sleep(1);
end;

function TGTNodeOvermind.NewNode(const AThread: TGTNodeThreadClass;
  const AOwnsThread: Boolean; const ANodeClass: TGTNodeClass): TGTNode;
var
  NodeClass: TGTNodeClass;
begin
  ForceMaxState(osInitialized);
  if ANodeClass = nil then
    NodeClass := TGTNode
  else
    NodeClass := ANodeClass;
  Result := NodeClass.Create(Self, AThread, AOwnsThread);
  Sleep(1);
  if FState = osInitialized then
  begin
    Result.FProcessorThread.PostCommand(NODE_THREAD_COMMAND_SETUP_IO, True);
    Result.FProcessorThread.WaitForCommand;
  end;
end;

procedure TGTNodeOvermind.PauseAll;
var
  Node: TGTNode;
begin
  ForceState(osLocked);
  for Node in FNodes do
    Node.FProcessorThread.PostCommand(NODE_THREAD_COMMAND_PAUSE, True);
  for Node in FNodes do
    Node.FProcessorThread.WaitForCommand;
end;

procedure TGTNodeOvermind.ResumeAll;
var
  Node: TGTNode;
begin
  ForceState(osLocked);
  for Node in FNodes do
  begin
    if Node.FProcessorThread.Suspended then
      Node.FProcessorThread.DoResume;
  end;
  Sleep(1);
end;

procedure TGTNodeOvermind.Unlock;

var
  Node: TGTNode;
  DataType: TGTNodeDataType;
begin
  ForceState(osLocked);
  for Node in FNodes do
    Node.FProcessorThread.PostCommand(NODE_THREAD_COMMAND_PAUSE, True);
  for Node in FNodes do
    Node.FProcessorThread.WaitForCommand;
  for Node in FNodes do
    Node.FProcessorThread.PostCommand(NODE_THREAD_COMMAND_BURN, True);
  for Node in FNodes do
    Node.FProcessorThread.WaitForCommand;
  FState := osUnlocked;
end;

end.

