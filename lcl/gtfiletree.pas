(*******************************************************************************
** File Name: gtfiletree.pas
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
unit GTFileTree;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  GTSession, GTEditor, GTFileBase;

type

  { TGTFileTree }

  TGTFileTree = class(TCustomTreeView)
  private
    FSession: TGTSession;
    procedure SetSession(const AValue: TGTSession);
    { Private declarations }
  protected
    function ForceSession: TGTSession;
    function CanEdit(Node: TTreeNode): Boolean; override;
    procedure DblClick; override;
    procedure LinkSession(ASession: TGTSession); virtual;
    procedure UnlinkSession(ASession: TGTSession); virtual;
  public
    procedure UpdateTree;
  published
    property Align;
    property Session: TGTSession read FSession write SetSession;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Game Toolset',[TGTFileTree]);
end;

{ TGTFileTree }

procedure TGTFileTree.SetSession(const AValue: TGTSession);
begin
  if FSession = AValue then
    Exit;
  if FSession <> nil then
    UnlinkSession(FSession);
  FSession := AValue;
  if AValue <> nil then
    LinkSession(AValue);
end;

function TGTFileTree.ForceSession: TGTSession;
begin
  if FSession = nil then
    EGTEditorNeedsSession.Create(ClassName+' needs a session assigned to perform this operation.');
  Result := FSession;
end;

function TGTFileTree.CanEdit(Node: TTreeNode): Boolean;
begin
  Result:=inherited CanEdit(Node);
  if Result then
  begin
    Result := False;
  end;
end;

procedure TGTFileTree.DblClick;
var
  Node: TTreeNode;
  FileNode: TGTFileNode;
begin
  inherited DblClick;
  Node := Selected;
  if Node = nil then
    Exit;
  FileNode := TGTFileNode(Node.Data);
  if FileNode <> nil then
    ForceSession.DoEditObject(FileNode, Self);
end;

procedure TGTFileTree.LinkSession(ASession: TGTSession);
begin

end;

procedure TGTFileTree.UnlinkSession(ASession: TGTSession);
begin

end;

procedure TGTFileTree.UpdateTree;

  function EnforcePath(APath: String): TTreeNode;
  var
    Segments: TStringList;
    I: Integer;
    Prev: TTreeNode;
    ThisSegment: String;
  begin
    APath := StringReplace(APath, '\', '/', [rfReplaceAll]);
    Segments := TStringList.Create;
    try
      Segments.Delimiter := '/';
      Segments.DelimitedText := APath;
      Prev := nil;
      Result := nil;
      for I := 0 to Segments.Count - 1 do
      begin
        ThisSegment := Segments[I];
        Result := Items.FindTopLvlNode(ThisSegment);
        if Result = nil then
          Result := Items.AddChild(Prev, ThisSegment);
      end;
    finally
      Segments.Free;
    end;
  end;

var
  I: Integer;
  FileNode: TGTFileNode;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    SortType := stText;
    ForceSession;
    for I := 0 to FSession.FileCount - 1 do
    begin
      FileNode := FSession.Files[I];
      EnforcePath(FileNode.FileName).Data := FileNode;
    end;
  finally
    Items.EndUpdate;
  end;
end;

end.
