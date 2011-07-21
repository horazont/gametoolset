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
  Classes, SysUtils, GTBase, GTNodes, fgl;

type
  TGTNodeController = class;

  TGTNodeThreadClasses = specialize TFPGList<TGTNodeThreadClass>;

  TGTNodeClassDescriptor = class (TGTBaseObject)
  private
    FName, FDescription: String;
  published
    property Name: String read FName write FName;
    property Description: String read FDescription write FDescription;
  end;

  TGTNodeDataTypeDescriptor = class (TGTBaseObject)
  private
    FName: String;
  published
    property Name: String read FName write FName;
  end;

  { TGTNodeDescriptor }

  TGTNodeDescriptor = class (TGTBaseObject)
  public
    constructor CreateFromNode(const ANode: TGTNode; const AController: TGTNodeController);
  private
    FID: QWord;
    FInputCount: QWord;
    FOutputCount: QWord;
    function GetInputNode(AIndex: Integer): TGTNodeDescriptor;
    function GetInputType(AIndex: Integer): TGTNodeDataTypeDescriptor;
    function GetOutputType(AIndex: Integer): TGTNodeDataTypeDescriptor;
  public
    property InputNode[AIndex: Integer]: TGTNodeDescriptor read GetInputNode;
    property InputTypes[AIndex: Integer]: TGTNodeDataTypeDescriptor read GetInputType;
    property OutputTypes[AIndex: Integer]: TGTNodeDataTypeDescriptor read GetOutputType;
  published
    property ID: QWord read FID write FID;
    property InputCount: QWord read FInputCount;
    property OutputCount: QWord read FOutputCount;
  end;

  TGTNodeController = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  public
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

{ TGTNodeDescriptor }

constructor TGTNodeDescriptor.CreateFromNode(const ANode: TGTNode;
  const AController: TGTNodeController);
begin
  FOutputCount := ANode.PortCount;
  FInputCount := ANode.InPortCount;
end;

function TGTNodeDescriptor.GetInputNode(AIndex: Integer): TGTNodeDescriptor;
begin

end;

function TGTNodeDescriptor.GetInputType(AIndex: Integer
  ): TGTNodeDataTypeDescriptor;
begin

end;

function TGTNodeDescriptor.GetOutputType(AIndex: Integer
  ): TGTNodeDataTypeDescriptor;
begin

end;

initialization
Registry := TGTNodeThreadClasses.Create;

RegisterGTClass(TGTNodeClassDescriptor);

finalization
Registry.Free;

end.

