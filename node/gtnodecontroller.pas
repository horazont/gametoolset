(*******************************************************************************
** File Name: gtnodecontroller.pas
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
unit GTNodeController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTBase, GTNodes, fgl, GTXML;

type
  TGTServerNodeController = class;

  TGTNodeThreadClasses = specialize TFPGList<TGTNodeThreadClass>;

  { TGTRemoteDescriptor }

  TGTRemoteDescriptor = class (TGTBaseObject)
  protected
    FUID: QWord;
  public
    procedure LoadFromXML(const XMLNode: TxmlNode; Context: IGTLoaderContext =
       nil); override;
  published
    property UID: QWord read FUID;
  end;

  TGTNodeClassDescriptor = class (TGTRemoteDescriptor)
  private
    FName, FDescription: String;
  published
    property Name: String read FName write FName;
    property Description: String read FDescription write FDescription;
  end;

  { TGTNodeDataTypeDescriptor }

  TGTNodeDataTypeDescriptor = class (TGTRemoteDescriptor)
  public
    constructor CreateFromType(const AType: TGTNodeDataTypeClass);
  private
    FName: String;
  published
    property Name: String read FName write FName;
  end;

  { TGTNodeDescriptor }

  TGTNodeDescriptor = class (TGTRemoteDescriptor)
  public
    constructor CreateFromNode(const ANode: TGTNode;
      const AController: TGTServerNodeController; const RegisterForUpdates: Boolean = True);
  private
    FInputCount: QWord;
    FInputNodes: array of TGTNodeDescriptor;
    FInputTypes: array of TGTNodeDataTypeDescriptor;
    FOutputCount: QWord;
    FOutputTypes: array of TGTNodeDataTypeDescriptor;
    function GetInputNode(AIndex: Integer): TGTNodeDescriptor;
    function GetInputType(AIndex: Integer): TGTNodeDataTypeDescriptor;
    function GetOutputType(AIndex: Integer): TGTNodeDataTypeDescriptor;
  protected
    procedure NodeChanged(Sender: TObject);
    procedure NodeDeleting(Sender: TObject);
  public
    property InputNode[AIndex: Integer]: TGTNodeDescriptor read GetInputNode;
    property InputTypes[AIndex: Integer]: TGTNodeDataTypeDescriptor read GetInputType;
    property OutputTypes[AIndex: Integer]: TGTNodeDataTypeDescriptor read GetOutputType;
  published
    property InputCount: QWord read FInputCount;
    property OutputCount: QWord read FOutputCount;
  end;

  { TGTNodeController }

  TGTNodeController = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  public
  //  function Get
  end;

  { TGTServerNodeController }

  TGTServerNodeController = class (TGTNodeController)
  public
    function GetNodeDescriptor(const ANode: TGTNode): TGTNodeDescriptor;
  end;

procedure RegisterNodeThreadClass(const AClass: TGTNodeThreadClass);

implementation

var
  Registry: TGTNodeThreadClasses = nil;

procedure RegisterNodeThreadClass(const AClass: TGTNodeThreadClass);
begin
  if Registry.IndexOf(AClass) >= 0 then
    Exit;
  Registry.Add(AClass);
end;

{ TGTRemoteDescriptor }

procedure TGTRemoteDescriptor.LoadFromXML(const XMLNode: TxmlNode;
  Context: IGTLoaderContext);
var
  Node: TxmlNode;
begin
  inherited LoadFromXML(XMLNode, Context);
  Node := XMLNode.Node['UID'];
  if Node <> nil then
    FUID := StrToQWord(Node.Content);
end;

{ TGTNodeDataTypeDescriptor }

constructor TGTNodeDataTypeDescriptor.CreateFromType(
  const AType: TGTNodeDataTypeClass);
begin

end;

{ TGTNodeDescriptor }

constructor TGTNodeDescriptor.CreateFromNode(const ANode: TGTNode;
  const AController: TGTServerNodeController; const RegisterForUpdates: Boolean
  );
var
  I: Integer;
begin
  FOutputCount := ANode.PortCount;
  FInputCount := ANode.InPortCount;
  SetLength(FInputNodes, FInputCount);
  SetLength(FInputTypes, FInputCount);
  SetLength(FOutputTypes, FOutputCount);
  for I := 0 to FInputCount - 1 do
  begin
    FInputTypes[I] := TGTNodeDataTypeDescriptor.CreateFromType(ANode.InPort[I].InPipe.DataTypeClass);
    FInputNodes[I] := AController.GetNodeDescriptor(ANode);
  end;
  for I := 0 to FOutputCount - 1 do
  begin
    FOutputTypes[I] := TGTNodeDataTypeDescriptor.CreateFromType(TGTNodeDataTypeClass(ANode.Port[I].OutPipe.DataType.ClassType));
  end;

  if RegisterForUpdates then
  begin
    ANode.OnChange.RegisterHandler(@NodeChanged);
    ANode.OnDestruction.RegisterHandler(@NodeDeleting);
  end;
end;

function TGTNodeDescriptor.GetInputNode(AIndex: Integer): TGTNodeDescriptor;
begin
  Exit(FInputNodes[AIndex]);
end;

function TGTNodeDescriptor.GetInputType(AIndex: Integer
  ): TGTNodeDataTypeDescriptor;
begin
  Exit(FInputTypes[AIndex]);
end;

function TGTNodeDescriptor.GetOutputType(AIndex: Integer
  ): TGTNodeDataTypeDescriptor;
begin
  Exit(FOutputTypes[AIndex]);
end;

procedure TGTNodeDescriptor.NodeChanged(Sender: TObject);
begin

end;

procedure TGTNodeDescriptor.NodeDeleting(Sender: TObject);
begin
  Free;
end;

{ TGTNodeController }

constructor TGTNodeController.Create;
begin

end;

destructor TGTNodeController.Destroy;
begin
  inherited Destroy;
end;

{ TGTServerNodeController }

function TGTServerNodeController.GetNodeDescriptor(const ANode: TGTNode
  ): TGTNodeDescriptor;
begin

end;

initialization
Registry := TGTNodeThreadClasses.Create;

RegisterGTClass(TGTNodeClassDescriptor);

finalization
Registry.Free;

end.

