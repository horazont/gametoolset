(*******************************************************************************
** File Name: gtstuff.pas
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
unit GTStuff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTBase, GTXML;

type

  { TGTStringContainer }

  TGTStringContainer = class (TGTBaseObject)
  public
    constructor Create; override;
  private
    FValue: String;
  public
    procedure AssignTo(Dest: TPersistent); override;
    class function ClassToString: String; override;
    procedure LoadFromXML(const XMLNode: TxmlNode; Context: IGTLoaderContext=
                 nil); override;
    procedure SaveToXML(const XMLNode: TxmlNode); override;
    function ToString: String; override;
  published
    property Value: String read FValue write FValue stored false;
  end;

function FormatDataSize(const DataSize: SizeUInt): String;
function FormatDataSize(const DataSize: Double): String;

implementation

function FormatDataSize(const DataSize: SizeUInt): String;
begin
  Exit(FormatDataSize(Double(DataSize)));
end;

function FormatDataSize(const DataSize: Double): String;
const
  Suffixes : array [0..4] of String = ('B', 'KiB', 'MiB', 'GiB', 'TiB');
var
  I: Integer;
  Suffix: String;
  Value, NewValue: Double;
begin
  Value := DataSize;
  for I := 1 to 4 do
  begin
    NewValue := Value / 1024;
    if NewValue >= 1.0 then
    begin
      Value := NewValue;
    end
    else
    begin
      Suffix := Suffixes[I-1];
      Break;
    end;
  end;
  Exit(Format('%.2f %s', [Value, Suffix]));
end;

{ TGTStringContainer }

constructor TGTStringContainer.Create;
begin
  inherited Create;
  FValue := '';
end;

procedure TGTStringContainer.AssignTo(Dest: TPersistent);
begin
  if Dest is TGTStringContainer then
  begin
    TGTStringContainer(Dest).FValue := FValue;
  end
  else
    inherited AssignTo(Dest);
end;

class function TGTStringContainer.ClassToString: String;
begin
  Result := 'String';
end;

procedure TGTStringContainer.LoadFromXML(const XMLNode: TxmlNode;
  Context: IGTLoaderContext);
begin
  FValue := XMLNode.Content;
end;

procedure TGTStringContainer.SaveToXML(const XMLNode: TxmlNode);
begin
  XMLNode.Content := FValue;
end;

function TGTStringContainer.ToString: String;
begin
  Result := FValue;
end;

end.

