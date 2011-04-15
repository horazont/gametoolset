(*******************************************************************************
** File Name: gtfilebase.pas
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
unit GTFileBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gtbase;

type

  { TGTFileNode }

  TGTFileNode = class (TGTBaseEditable)
  public
    constructor Create(AFileName: String); virtual;
    destructor Destroy; override;
  private
    FFileName: String;
    FOnChange: TGTEventList;
  public
    procedure NotifyChange;
    function ToString: String; override;
  public
    property OnChange: TGTEventList read FOnChange;
  published
    property FileName: String read FFileName;
  end;
  TGTFileClass = class of TGTFileNode;

implementation

{ TGTFileNode }

constructor TGTFileNode.Create(AFileName: String);
begin
  inherited Create;
  FFileName := AFileName;
  FOnChange := TGTEventList.Create;
end;

destructor TGTFileNode.Destroy;
begin
  FOnChange.Free;
  inherited Destroy;
end;

procedure TGTFileNode.NotifyChange;
begin
  FOnChange.Call(Self);
end;

function TGTFileNode.ToString: String;
begin
  Result := FFileName;
end;

end.

