(*******************************************************************************
** File Name: gtrttitable.pas
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
unit GTRTTITable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  gtbase, ComCtrls, typinfo, GTEditor, variants;

type
  EGTRTTITableError = class (Exception);

  { TGTRTTITable }

  TGTRTTITable = class(TGTEditor)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FCanTrace: Boolean;
    FPropCount: Integer;
    FPropList: PPropList;
    FTraceProps: TStringList;
  private
    procedure HandleObjChange(Sender: TObject);
    procedure HandleObjDestruction(Sender: TObject);
    procedure HandleObjPropChange(Sender: TObject; const PropertyName: String;
      PropInfo: PPropInfo; const OldValue, NewValue: Variant);
    procedure ListDblClick(Sender: TObject);
    procedure ListDeleting(Sender: TObject; Item: TListItem);
  protected
    FList: TListView;
  protected
    procedure AddColumn(const ACaption: String; APropIdx: Integer;
      AutoTrace: Boolean = False);
    procedure AddItem(AInstance: TObject);
    procedure BeginUpdate;
    procedure BuildColumns; virtual;
    procedure Clear;
    procedure DoItemDblClick(AItem: TListItem); virtual;
    procedure EndUpdate;
    function GetItemClass: TClass; virtual; abstract;
    procedure DoNodeChange(const AItem: TListItem; const ANode: TGTBaseObject); virtual;
    procedure DoNodePropChange(const AItem: TListItem; const APropIdx: Integer;
      const NewValue: Variant; const Column: TListColumn); virtual;
    function IndexOfProp(const APropInfo: PPropInfo): Integer;
    function IndexOfProp(const APropName: String): Integer;
    procedure LinkObject(const AObject: TGTBaseObject);
    function PropAsString(AInstance: TObject; APropInfo: PPropInfo): String;
    procedure ReadProps(AClass: TClass);
    procedure UpdateItem(AItem: TListItem; AInstance: TObject); virtual;
    procedure UnlinkObject(const AObject: TGTBaseObject);
  public
    procedure UpdateView; override;
  published
    { Published declarations }
  end;

implementation

{ TGTRTTITable }

constructor TGTRTTITable.Create(AOwner: TComponent);
var
  Cls: TClass;
begin
  inherited Create(AOwner);
  FList := TListView.Create(Self);
  FList.Parent := Self;
  FList.Align := alClient;
  FList.ViewStyle := vsReport;
  FList.OnDblClick := @ListDblClick;
  FList.OnDeletion := @ListDeleting;
  Cls := GetItemClass;
  FCanTrace := Cls.InheritsFrom(TGTBaseObject);
  ReadProps(Cls);
  if FCanTrace then
    FTraceProps := TStringList.Create
  else
    FTraceProps := nil;
end;

destructor TGTRTTITable.Destroy;
begin
  Clear;
  if FPropList <> nil then
    FreeMem(FPropList);
  FreeAndNil(FTraceProps);
  inherited Destroy;
end;

procedure TGTRTTITable.HandleObjChange(Sender: TObject);
var
  Item: TListItem;
begin
  Item := FList.Items.FindData(Sender);
  if not Assigned(Item) then
    Exit;
  DoNodeChange(Item, TGTBaseObject(Item.Data));
end;

procedure TGTRTTITable.HandleObjDestruction(Sender: TObject);
var
  Item: TListItem;
begin
  Item := FList.Items.FindData(Sender);
  if Assigned(Item) then
    Item.Delete;
end;

procedure TGTRTTITable.HandleObjPropChange(Sender: TObject;
  const PropertyName: String; PropInfo: PPropInfo; const OldValue,
  NewValue: Variant);
var
  Item: TListItem;
  Idx: Integer;
begin
  Item := FList.Items.FindData(Sender);
  if not Assigned(Item) then
    Exit;
  Idx := FTraceProps.IndexOf(PropertyName);
  if Idx < 0 then
    Exit;
  DoNodePropChange(Item, IndexOfProp(PropInfo), NewValue, TListColumn(FTraceProps.Objects[Idx]));
end;

procedure TGTRTTITable.ListDblClick(Sender: TObject);
begin
  if Assigned(FList.Selected) then
    DoItemDblClick(FList.Selected);
end;

procedure TGTRTTITable.ListDeleting(Sender: TObject; Item: TListItem);
begin
  if FCanTrace then
    UnlinkObject(TGTBaseObject(Item.Data));
end;

procedure TGTRTTITable.AddColumn(const ACaption: String; APropIdx: Integer;
  AutoTrace: Boolean);
var
  Col: TListColumn;
begin
  Col := FList.Columns.Add;
  with Col do
  begin
    Tag := APropIdx;
    Caption := ACaption;
    AutoSize := True;
  end;
  if AutoTrace and (APropIdx >= 0) then
  begin
    if not FCanTrace then
      raise EGTRTTITableError.Create('Cannot autotrace for given class type.');
    FTraceProps.AddObject(FPropList^[APropIdx]^.Name, Col);
  end;
end;

procedure TGTRTTITable.AddItem(AInstance: TObject);
var
  ListItem: TListItem;
begin
  ListItem := FList.Items.Add;
  UpdateItem(ListItem, AInstance);
  if FCanTrace then
    LinkObject(AInstance as TGTBaseObject);
end;

procedure TGTRTTITable.BeginUpdate;
begin
  FList.Items.BeginUpdate;
end;

procedure TGTRTTITable.BuildColumns;
var
  I: Integer;
  PropInfo: PPropInfo;
begin
  FList.Columns.Clear;
  for I := 0 to FPropCount - 1 do
  begin
    PropInfo := FPropList^[I];
    AddColumn(PropInfo^.Name, I);
  end;
end;

procedure TGTRTTITable.Clear;
var
  I: Integer;
begin
  if FCanTrace then
  begin
    for I := 0 to FList.Items.Count - 1 do
      UnlinkObject(TGTBaseObject(FList.Items[I].Data));
    FList.Items.Clear;
  end
  else
    FList.Items.Clear;
end;

procedure TGTRTTITable.DoItemDblClick(AItem: TListItem);
begin

end;

procedure TGTRTTITable.EndUpdate;
begin
  FList.Items.EndUpdate;
end;

procedure TGTRTTITable.DoNodeChange(const AItem: TListItem;
  const ANode: TGTBaseObject);
begin

end;

procedure TGTRTTITable.DoNodePropChange(const AItem: TListItem;
  const APropIdx: Integer; const NewValue: Variant; const Column: TListColumn);
var
  Idx: Integer;
begin
  Idx := Column.Index;
  Column.AutoSize := False;
  if Idx = 0 then
    AItem.Caption := NewValue
  else
    AItem.SubItems[Idx-1] := NewValue;
  Column.Width := 1000;
  Column.AutoSize := True;
end;

function TGTRTTITable.IndexOfProp(const APropInfo: PPropInfo): Integer;
begin
  for Result := 0 to FPropCount - 1 do
    if FPropList^[Result] = APropInfo then
      Exit;
  Result := -1;
end;

function TGTRTTITable.IndexOfProp(const APropName: String): Integer;
var
  CmpName: ShortString;
begin
  CmpName := APropName;
  for Result := 0 to FPropCount - 1 do
    if FPropList^[Result]^.Name = CmpName then
      Exit;
  Result := -1;
end;

procedure TGTRTTITable.LinkObject(const AObject: TGTBaseObject);
begin
  AObject.OnDestruction.RegisterHandler(@HandleObjDestruction);
  AObject.OnPropChange.RegisterHandler(@HandleObjPropChange);
  AObject.OnChange.RegisterHandler(@HandleObjChange);
end;

function TGTRTTITable.PropAsString(AInstance: TObject; APropInfo: PPropInfo): String;
begin
  case APropInfo^.PropType^.Kind of
    tkInteger, tkChar, tkWChar, tkClass, tkBool:
      Result := IntToStr(GetOrdProp(AInstance, APropInfo));
    tkEnumeration:
      Result := GetEnumProp(AInstance, APropInfo);
    tkSet:
      Result := GetSetProp(AInstance, APropInfo, False);
    {$ifndef FPUNONE}
    tkFloat:
      Result := Format('%.4f', [GetFloatProp(AInstance, APropInfo)]);
    {$endif}
    tkString, tkLString, tkAString:
      Result := GetStrProp(AInstance, APropInfo);
    tkWString:
      Result := GetWideStrProp(AInstance, APropInfo);
    tkUString:
      Result := GetUnicodeStrProp(AInstance, APropInfo);
    tkVariant:
      Result := GetVariantProp(AInstance, APropInfo);
    tkInt64:
      Result := IntToStr(GetInt64Prop(AInstance, APropInfo));
  else
    Result := 'read error';
  end;
end;

procedure TGTRTTITable.ReadProps(AClass: TClass);
begin
  if FPropList <> nil then
    FreeMem(FPropList);
  FPropCount := GetTypeData(AClass.ClassInfo)^.PropCount;
  FPropList := GetMem(SizeOf(Pointer) * FPropCount);
  GetPropInfos(AClass.ClassInfo, FPropList);
end;

procedure TGTRTTITable.UpdateItem(AItem: TListItem; AInstance: TObject);
var
  I: Integer;
  AIndex: Integer;
begin
  with AItem do
  begin
    Data := AInstance;
    AIndex := FList.Columns[0].Tag;
    if AIndex >= 0 then
      Caption := PropAsString(AInstance, FPropList^[AIndex]);
    SubItems.Clear;
    for I := 1 to FList.Columns.Count - 1 do
    begin
      AIndex := FList.Columns[I].Tag;
      if AIndex >= 0 then
        SubItems.Add(PropAsString(AInstance, FPropList^[AIndex]));
    end;
  end;
end;

procedure TGTRTTITable.UnlinkObject(const AObject: TGTBaseObject);
begin
  AObject.OnDestruction.UnRegisterHandler(@HandleObjDestruction);
  AObject.OnChange.UnRegisterHandler(@HandleObjChange);
  AObject.OnPropChange.UnRegisterHandler(@HandleObjPropChange);
end;

procedure TGTRTTITable.UpdateView;
begin
  inherited UpdateView;
  FList.Clear;
  BuildColumns;
end;

end.
