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

interface

uses
  Classes, SysUtils, GTBase, fgl, GTThreadFIFO, typinfo;

type
  EGTNodeError = class(EGTError);
    EGTNodeTypeError = class (EGTNodeError);
    EGTNodePortError = class (EGTNodeError);
    EGTNodeInvalidConstructor = class(EGTNodeError);
    EGTNodeLockError = class (EGTNodeError);

  TGTNode = class;
  TGTNodePortNumber = SizeUInt;
  TGTNodeOutPort = class;
  TGTNodeDataSet = PPointer;
  TGTNodeOvermind = class;

  TGTNodeOvermindState = (osUnlocked, osInitialized, osLocked);

  { TGTNodeDataType }

  TGTNodeDataType = class (TGTMultiRefObject)
  public
    constructor Create(const AOvermind: TGTNodeOvermind);
  private
    FOvermind: TGTNodeOvermind;
  protected
    procedure DoRefChange; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Burn; virtual;
    procedure FreeItem(var AItem: Pointer); virtual; abstract;
    function GetItem: Pointer; virtual; abstract;
    procedure Init; virtual;
  end;
  TGTNodeDataTypes = specialize TFPGList<TGTNodeDataType>;

  { TGTNodeFIFO }

  TGTNodeFIFO = class (TGTThreadFIFO)
  public
    constructor Create(const ADataType: TGTNodeDataType);
  private
    FDataType: TGTNodeDataType;
  protected
    procedure FreeItem(AData: Pointer); override;
  public
    property DataType: TGTNodeDataType read FDataType;
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
    constructor Create(const AOwner: TGTNode; const AInPipe: TGTNodeFIFO;
      const ANumber: TGTNodePortNumber);
    destructor Destroy; override;
  private
    FInPipe: TGTNodeFIFO;
    FSource: TGTNodeOutPort;
    procedure SetSource(const AValue: TGTNodeOutPort);
  published
    property InPipe: TGTNodeFIFO read FInPipe;
    property Source: TGTNodeOutPort read FSource write SetSource;
  end;
  TGTNodeInPorts = specialize TFPGList<TGTNodeInPort>;

  { TGTNodeOutPort }

  TGTNodeOutPort = class (TGTNodePort)
  public
    constructor Create(const AOwner: TGTNode; const AOutPipe: TGTNodeFIFO;
      const ANumber: TGTNodePortNumber);
    destructor Destroy; override;
  private
    FDestPorts: TGTNodeInPorts;
    FOutPipe: TGTNodeFIFO;
  protected
    procedure DestDeleting(Sender: TObject);
    procedure Link(const APort: TGTNodeInPort);
    procedure Unlink(const APort: TGTNodeInPort);
    procedure Unlink(const AIndex: Integer);
    procedure UnlinkAll;
  public
    procedure Push;
  published
    property OutPipe: TGTNodeFIFO read FOutPipe;
  end;

  {$M+}

  { TGTNodeThread }

  TGTNodeThread = class (TThread)
  public
    constructor Create(const AOwnerNode: TGTNode); virtual;
    destructor Destroy; override;
  private
    FOwner: TGTNode;
    FResetRequested: Boolean;
    FResetSemaphore: Pointer;
    FInData, FOutData: TGTNodeDataSet;
    FInCount, FOutCount: TGTNodePortNumber;
  protected
    FInPorts: array of TGTNodeFIFO;
    FOutPorts: array of TGTNodeFIFO;
  protected
    procedure CheckReset;
    function GetOwner: TGTNode;
    procedure SetupInPorts(const ATypes: array of TGTNodeDataType);
    procedure SetupOutPorts(const ATypes: array of TGTNodeDataType);
  protected
    procedure Burn; virtual;
    procedure Init; virtual; // note that init suspends the thread!
    procedure ProcessDataSet(const AInputData: TGTNodeDataSet;
      const AOutputData: TGTNodeDataSet); virtual; abstract;
    function RequireInput(const AInput: TGTNodePortNumber): Boolean; virtual;
  public
    procedure Execute; override;
    procedure Reset;
    procedure WaitForReset;
  end;
  {$M-}

  TGTNodeThreadClass = class of TGTNodeThread;

  { TGTNode }

  TGTNode = class (TGTBaseObject)
  public
    constructor Create(const AOvermind: TGTNodeOvermind;
      const AProcessorThread: TGTNodeThread;
      const AOwnsThread: Boolean = True); virtual;
    destructor Destroy; override;
  private
    FInPorts: array of TGTNodeInPort;
    FProcessorThread: TGTNodeThread;
    FOutPorts: array of TGTNodeOutPort;
    FOvermind: TGTNodeOvermind;
    FOwnsThread: Boolean;
    function GetInPort(AIndex: Integer): TGTNodeInPort;
    function GetOutPort(AIndex: Integer): TGTNodeOutPort;
  protected
    procedure ForceState(const AState: TGTNodeOvermindState);
    procedure ForceMaxState(const AState: TGTNodeOvermindState);
    procedure ForceMinState(const AState: TGTNodeOvermindState);
    procedure Reset;
    procedure RequireOvermind;
    procedure SetupInPorts(const ACount: Integer);
    procedure SetupOutPorts(const ACount: Integer);
    procedure WaitForReset;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    property InPort[AIndex: Integer]: TGTNodeInPort read GetInPort;
    property Port[AIndex: Integer]: TGTNodeOutPort read GetOutPort;
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
    FTypes: TGTNodeDataTypes;
    FOnNodeCreated: TGTNodeEventList;
    FOnNodeDeleting: TGTNodeEventList;
    FState: TGTNodeOvermindState;
  protected
    procedure DoNodeCreated(const ANode: TGTNode);
    procedure DoNodeDeleting(const ANode: TGTNode);
    procedure DoTypeCreated(const AType: TGTNodeDataType);
    procedure DoTypeDeleting(const AType: TGTNodeDataType);
    procedure ForceState(const AState: TGTNodeOvermindState);
    procedure ForceMaxState(const AState: TGTNodeOvermindState);
    procedure ForceMinState(const AState: TGTNodeOvermindState);
  public
    procedure BeforeDestruction; override;
    procedure DeleteAllNodes;
    procedure Init;
    procedure Lock;
    function NewNode(const AThread: TGTNodeThread;
      const AOwnsThread: Boolean = True;
      const ANodeClass: TGTNodeClass = nil): TGTNode;
    procedure Unlock;
  public
    property OnNodeCreated: TGTNodeEventList read FOnNodeCreated;
    property OnNodeDeleting: TGTNodeEventList read FOnNodeDeleting;
  end;

implementation

function InitDataSet(const Len: TGTNodePortNumber): TGTNodeDataSet;
begin
  Result := GetMem(Len * SizeOf(Pointer));
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

{ TGTNodeDataType }

constructor TGTNodeDataType.Create(const AOvermind: TGTNodeOvermind);
begin
  inherited Create;
  FOvermind := AOvermind;
end;

procedure TGTNodeDataType.DoRefChange;
begin
  inherited DoRefChange;
  if GetReferenceCount = 0 then
    Free;
end;

procedure TGTNodeDataType.AfterConstruction;
begin
  inherited AfterConstruction;
  FOvermind.DoTypeCreated(Self);
end;

procedure TGTNodeDataType.BeforeDestruction;
begin
  FOvermind.DoTypeDeleting(Self);
  inherited BeforeDestruction;
end;

procedure TGTNodeDataType.Burn;
begin

end;

procedure TGTNodeDataType.Init;
begin

end;

{ TGTNodeFIFO }

constructor TGTNodeFIFO.Create(const ADataType: TGTNodeDataType);
begin
  FDataType := ADataType;
end;

procedure TGTNodeFIFO.FreeItem(AData: Pointer);
begin
  FDataType.FreeItem(AData);
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
  const AInPipe: TGTNodeFIFO; const ANumber: TGTNodePortNumber);
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
  end;
  if not (AValue.OutPipe.DataType = FInPipe.DataType) then
    raise EGTNodeTypeError.Create('Incompatible types.');
  FSource := AValue;
  if FSource <> nil then
  begin
    //LinkSource(FSource);
    FSource.Link(Self);
  end;
end;

{ TGTNodeOutPort }

constructor TGTNodeOutPort.Create(const AOwner: TGTNode;
  const AOutPipe: TGTNodeFIFO; const ANumber: TGTNodePortNumber);
begin
  inherited Create(AOwner, ANumber);
  FDestPorts := TGTNodeInPorts.Create;
  FOutPipe := AOutPipe;
end;

destructor TGTNodeOutPort.Destroy;
begin
  ForceState(osUnlocked);
  UnlinkAll;
  FDestPorts.Free;
  inherited Destroy;
end;


procedure TGTNodeOutPort.DestDeleting(Sender: TObject);
begin
  ForceState(osUnlocked);
  Unlink(Sender as TGTNodeInPort);
end;

procedure TGTNodeOutPort.Link(const APort: TGTNodeInPort);
var
  I: Integer;
begin
  ForceState(osUnlocked);
  I := FDestPorts.IndexOf(APort);
  if I >= 0 then
    Exit;
  if APort.InPipe.DataType <> FOutPipe.DataType then
    raise EGTNodeTypeError.Create('Incompatible types.');
  FDestPorts.Add(APort);
  APort.Source := Self;
end;

procedure TGTNodeOutPort.Unlink(const APort: TGTNodeInPort);
var
  I: Integer;
begin
  ForceState(osUnlocked);
  I := FDestPorts.IndexOf(APort);
  if I < 0 then
    Exit;
  Unlink(I);
end;

procedure TGTNodeOutPort.Unlink(const AIndex: Integer);
var
  Port: TGTNodeInPort;
begin
  ForceState(osUnlocked);
  Port := FDestPorts[AIndex];
  FDestPorts.Delete(AIndex);
  Port.Source := nil;
end;

procedure TGTNodeOutPort.UnlinkAll;
begin
  ForceState(osUnlocked);
  while FDestPorts.Count > 0 do
    Unlink(0);
end;

procedure TGTNodeOutPort.Push;
var
  I: Integer;
begin
  ForceState(osLocked);
end;

{ TGTNodeThread }

constructor TGTNodeThread.Create(const AOwnerNode: TGTNode);
begin
  inherited Create(True);
  FOwner := AOwnerNode;
  FResetRequested := False;
  FResetSemaphore := TM.SemaphoreInit;
  SetupInPorts([]);
  SetupOutPorts([]);
end;

destructor TGTNodeThread.Destroy;
begin
  TM.SemaphoreDestroy(FResetSemaphore);
  SetupInPorts([]);
  SetupOutPorts([]);
  inherited Destroy;
end;

procedure TGTNodeThread.CheckReset;
begin
  if FResetRequested then
  begin
    Burn;
    TM.SemaphorePost(FResetSemaphore);
    Suspend;
    Init;
    FResetRequested := False;
  end;
end;

function TGTNodeThread.GetOwner: TGTNode;
begin
  Exit(FOwner);
end;

procedure TGTNodeThread.SetupInPorts(
  const ATypes: array of TGTNodeDataType);
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
      FInPorts[I] := TGTNodeFIFO.Create(ATypes[I]);
  end;
  for I := 0 to L-1 do
  begin
    if FInPorts[I].DataType <> ATypes[I] then
    begin
      FInPorts[I].Free;
      FInPorts[I] := TGTNodeFIFO.Create(ATypes[I]);
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
      FOutPorts[I] := TGTNodeFIFO.Create(ATypes[I]);
  end;
  for I := 0 to L-1 do
  begin
    if FOutPorts[I].DataType <> ATypes[I] then
    begin
      FOutPorts[I].Free;
      FOutPorts[I] := TGTNodeFIFO.Create(ATypes[I]);
    end;
  end;
  FOwner.SetupOutPorts(Count);
end;

procedure TGTNodeThread.Burn;
begin
  BurnDataSet(FInData);
  BurnDataSet(FOutData);
end;

procedure TGTNodeThread.Init;
begin
  FInCount := Length(FInPorts);
  FInData := InitDataSet(FInCount);
  FOutCount := Length(FOutPorts);
  FOutData := InitDataSet(FOutCount);
  Suspend;
end;

function TGTNodeThread.RequireInput(const AInput: TGTNodePortNumber): Boolean;
begin
  Exit(True);
end;

procedure TGTNodeThread.Execute;
var
  I: Integer;
  Got: TGTNodePortNumber;
begin
  Init;
  try
    while not Terminated do
    begin
      CheckReset;
      Got := 0;
      while Got < FInCount do
      begin
        CheckReset;
        Got := 0;
        for I := 0 to FInCount - 1 do
        begin
          if (FInData[I] = nil) then
          begin
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
      ProcessDataSet(FInData, FOutData);
      for I := 0 to FOutCount - 1 do
      begin
        FOutPorts[I].Push(FOutData[I]);
      end;
      ThreadSwitch;
    end;
  finally
    Burn;
  end;
end;

procedure TGTNodeThread.Reset;
begin
  FResetRequested := True;
end;

procedure TGTNodeThread.WaitForReset;
begin
  FResetRequested := True;
  TM.SemaphoreWait(FResetSemaphore);
end;

{ TGTNode }

constructor TGTNode.Create(const AOvermind: TGTNodeOvermind;
  const AProcessorThread: TGTNodeThread; const AOwnsThread: Boolean);
begin
  if AOvermind = nil then
    RequireOvermind;
  inherited Create;
  FProcessorThread := AProcessorThread;
  FOwnsThread := AOwnsThread;
  FOvermind := AOvermind;
end;

destructor TGTNode.Destroy;
begin
  if FOwnsThread then
    FProcessorThread.Free
  else
  begin
    FProcessorThread := nil;
    SetupInPorts(0);
    SetupOutPorts(0);
  end;
  inherited Destroy;
end;

function TGTNode.GetInPort(AIndex: Integer): TGTNodeInPort;
begin
  Exit(FInPorts[AIndex]);
end;

function TGTNode.GetOutPort(AIndex: Integer): TGTNodeOutPort;
begin
  Exit(FOutPorts[AIndex]);
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

procedure TGTNode.Reset;
begin
  FProcessorThread.Reset;
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
      FInPorts[I] := TGTNodeInPort.Create(Self, FProcessorThread.FInPorts[I], I);
  end;
  for I := 0 to L-1 do
  begin
    if FInPorts[I].FInPipe <> FProcessorThread.FInPorts[I] then
    begin
      FInPorts[I].Free;
      FInPorts[I] := TGTNodeInPort.Create(Self, FProcessorThread.FInPorts[I], I);
    end;
  end;
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
end;

procedure TGTNode.WaitForReset;
begin
  FProcessorThread.WaitForReset;
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
  FTypes := TGTNodeDataTypes.Create;
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
  FTypes.Free;
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

procedure TGTNodeOvermind.DoTypeCreated(const AType: TGTNodeDataType);
begin
  FTypes.Add(AType);
end;

procedure TGTNodeOvermind.DoTypeDeleting(const AType: TGTNodeDataType);
begin
  FTypes.Remove(AType);
end;

procedure TGTNodeOvermind.ForceState(const AState: TGTNodeOvermindState);
begin
  if FState <> AState then
    raise EGTNodeLockError.CreateFmt('%s state required.', [Copy(GetEnumName(TypeInfo(AState), Ord(AState)), 2, 100)]);
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

procedure TGTNodeOvermind.Init;
var
  Node: TGTNode;
begin
  ForceState(osUnlocked);
  for Node in FNodes do
    Node.FProcessorThread.Resume; // first resume after lock will initialize
  FState := osInitialized;
end;

procedure TGTNodeOvermind.Lock;
var
  Node: TGTNode;
  DataType: TGTNodeDataType;
begin
  ForceState(osInitialized);
  for DataType in FTypes do
    DataType.Init;
  for Node in FNodes do
    Node.FProcessorThread.Resume; // first resume after init will start
  FState := osLocked;
end;

function TGTNodeOvermind.NewNode(const AThread: TGTNodeThread;
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
  if FState = osInitialized then
    Result.FProcessorThread.Resume; // first resume will initialize thread and node
end;

procedure TGTNodeOvermind.Unlock;
var
  Node: TGTNode;
  DataType: TGTNodeDataType;
begin
  ForceState(osLocked);
  for Node in FNodes do
    Node.Reset;
  for Node in FNodes do
    Node.WaitForReset;
  for DataType in FTypes do
    DataType.Burn;
  FState := osUnlocked;
end;

initialization
Assert(GetThreadManager(TM));

end.

