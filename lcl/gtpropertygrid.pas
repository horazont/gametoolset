(*******************************************************************************
** File Name: gtpropertygrid.pas
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
unit GTPropertyGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, RTTIGrids,
  GTBase, typinfo, ObjectInspector, Messages, LCLType, PropEdits, GTIntMap,
  contnrs, RTTICtrls;

type

  { TGTPropertyDrag }

  TGTPropertyDrag = class (TDragObject)
  public
    constructor Create(AControl: TControl); override;
  private
    FOwningObject: TPersistent;
    FInspectedObject: TPersistent;
    FPropertyName: String;
    FPropertyPath: String;
    FPropertyInfo: PPropInfo;
    function GetPropertyInfo: PPropInfo;
  public
    property OwningObject: TPersistent read FOwningObject;
    property InspectedObject: TPersistent read FInspectedObject;
    property PropertyName: String read FPropertyName;
    property PropertyPath: String read FPropertyPath;
    property PropertyInfo: PPropInfo read GetPropertyInfo;
  end;

  { TGTPropertyGrid }

  TGTPropertyGrid = class(TTIPropertyGrid)
  public
    constructor Create(TheOwner: TComponent); override;
  private
    FDragFinallyRunning: Boolean;
    FFutureDragRow: TOIPropertyGridRow;
    FOldHintRow: Integer;
    FOldHintCol: Integer;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    function DoDragMsg(ADragMessage: TDragMessage; APosition: TPoint;
       ADragObject: TDragObject; ATarget: TControl; ADocking: Boolean
       ): LRESULT; override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
  public
    { Public declarations }
  published
    property ShowHint;
    property ParentShowHint;
  end;

  TGTPropertyMetadata = record
    OwnerClass: TClass;
    TypeInfo: PTypeInfo;
    PropertyName: String;
    DisplayName: String;
    Hint: String;
  end;
  PGTPropertyMetadata = ^TGTPropertyMetadata;

  TGTEnumValueMetadata = record
    TypeInfo: PTypeInfo;
    Value: ptrint;
    DisplayName: String;
    Hint: String;
  end;
  PGTEnumValueMetadata = ^TGTEnumValueMetadata;


procedure Register;

procedure RegisterPropertyMetadata(PropType: PTypeInfo; OwnerClass: TClass;
  const PropertyName: String; const DisplayName: String; const Hint: String);
procedure RegisterEnumValueMetadata(EnumType: PTypeInfo; Value: ptrint;
  const DisplayName: String; const Hint: String);
procedure RegisterContinousEnumValueMetadata(EnumType: PTypeInfo;
  MinValue: ptrint; DisplayNames: array of String; Hints: array of String);

function LookupPropertyMetadata(PropType: PTypeInfo; OwnerClass: TClass;
  const PropertyName: String): PGTPropertyMetadata;
function LookupEnumValueMetadata(EnumType: PTypeInfo; Value: ptrint): PGTEnumValueMetadata;

procedure MapEnumValueMetadataToAliases(EnumType: PTypeInfo;
  Aliases: TStrings; Clear: Boolean = True);

implementation

type
  TNameMap = TFPDataHashTable;
  TClassMap = specialize TGTIntHashMap<TNameMap>;
  TTypeMap = specialize TGTIntHashMap<TClassMap>;
  TValueMap = specialize TGTIntMap<PGTEnumValueMetadata>;
  TEnumTypeMap = specialize TGTIntHashMap<TValueMap>;

var
  PropertyMetadataTree: TTypeMap;
  EnumValueMetadataTree: TEnumTypeMap;

procedure Register;
begin
  RegisterComponents('Game Toolset',[TGTPropertyGrid]);
end;

procedure RegisterPropertyMetadata(PropType: PTypeInfo; OwnerClass: TClass;
  const PropertyName: String; const DisplayName: String; const Hint: String);
var
  ClassMap: TClassMap;
  NameMap: TNameMap;
  Item: PGTPropertyMetadata;
begin
  ClassMap := PropertyMetadataTree.Items[ptruint(Proptype)];
  if ClassMap = nil then
  begin
    if PropType = nil then
      ClassMap := TClassMap.Create(6151)
    else
      ClassMap := TClassMap.Create(769);
    PropertyMetadataTree.Items[ptruint(PropType)] := ClassMap;
  end;

  NameMap := ClassMap.Items[ptruint(OwnerClass)];
  if NameMap = nil then
  begin
    if OwnerClass = nil then
      NameMap := TNameMap.CreateWith(6151, @RSHash)
    else
      NameMap := TNameMap.CreateWith(769, @RSHash);
    ClassMap.Items[ptruint(OwnerClass)] := NameMap;
  end;

  New(Item);
  Item^.TypeInfo := PropType;
  Item^.OwnerClass := OwnerClass;
  Item^.PropertyName := PropertyName;
  Item^.DisplayName := DisplayName;
  Item^.Hint := Hint;
  NameMap.Add(PropertyName, Item);
end;

procedure RegisterEnumValueMetadata(EnumType: PTypeInfo; Value: ptrint;
  const DisplayName: String; const Hint: String);
var
  ValueMap: TValueMap;
  Item: PGTEnumValueMetadata;
  MinValue, MaxValue: Integer;
  TypeData: PTypeData;
begin
  TypeData := GetTypeData(EnumType);
  MinValue := TypeData^.MinValue;
  MaxValue := TypeData^.MaxValue;
  ValueMap := EnumValueMetadataTree.Items[ptruint(EnumType)];
  if ValueMap = nil then
  begin
    if EnumType = nil then
      raise Exception.Create('Cannot register enum value metadata for nil type.');
    ValueMap := TValueMap.Create((MaxValue - MinValue)+1);
    EnumValueMetadataTree.Items[ptruint(EnumType)] := ValueMap;
  end;

  New(Item);
  Item^.TypeInfo := EnumType;
  Item^.Value := Value;
  Item^.DisplayName := DisplayName;
  Item^.Hint := Hint;
  ValueMap.Items[Value - MinValue] := Item;
end;

procedure RegisterContinousEnumValueMetadata(EnumType: PTypeInfo;
  MinValue: ptrint; DisplayNames: array of String; Hints: array of String);
var
  I: Integer;
begin
  if Length(Hints) = 0 then
  begin
    for I := 0 to High(DisplayNames) do
      RegisterEnumValueMetadata(EnumType, I + MinValue, DisplayNames[I], '');
  end
  else
  begin
    if Length(DisplayNames) <> Length(Hints) then
      raise Exception.CreateFmt('Got %d elements for display names but %d elements for hints.', [Length(DisplayNames), Length(Hints)]);
    for I := 0 to High(DisplayNames) do
      RegisterEnumValueMetadata(EnumType, I + MinValue, DisplayNames[I], Hints[I]);
  end;
end;

function LookupPropertyMetadata(PropType: PTypeInfo; OwnerClass: TClass;
  const PropertyName: String): PGTPropertyMetadata;

  function CheckClassMap(Map: TClassMap): PGTPropertyMetadata;

    function CheckNameMap(Map: TNameMap): PGTPropertyMetadata;
    begin
      Result := Map[PropertyName];
    end;

  var
    NameMap: TNameMap;
  begin
    NameMap := Map.Items[ptruint(OwnerClass)];
    if NameMap <> nil then
      Result := CheckNameMap(NameMap)
    else
      Result := nil;
    if Result = nil then
    begin
      NameMap := Map.Items[0];
      if NameMap <> nil then
        Result := CheckNameMap(NameMap);
    end;
  end;
var
  ClassMap: TClassMap;
begin
  ClassMap := PropertyMetadataTree.Items[ptruint(PropType)];
  if ClassMap <> nil then
    Result := CheckClassMap(ClassMap)
  else
    Result := nil;
  if Result = nil then
  begin
    ClassMap := PropertyMetadataTree.Items[0];
    if ClassMap <> nil then
      Result := CheckClassMap(ClassMap);
  end;
end;

function LookupEnumValueMetadata(EnumType: PTypeInfo; Value: ptrint
  ): PGTEnumValueMetadata;

  function CheckValueMap(Map: TValueMap): PGTEnumValueMetadata;
  begin
    Result := Map.Items[Value - GetTypeData(EnumType)^.MinValue];
  end;

var
  ValueMap: TValueMap;
begin
  ValueMap := EnumValueMetadataTree.Items[ptruint(EnumType)];
  if ValueMap <> nil then
    Result := CheckValueMap(ValueMap)
  else
    Result := nil;
end;

procedure MapEnumValueMetadataToAliases(EnumType: PTypeInfo; Aliases: TStrings;
  Clear: Boolean);
var
  ValueMap: TValueMap;
  I: Integer;
  TypeData: PTypeData;
  Entry: PGTEnumValueMetadata;
  MinValue: LongInt;
begin
  if Clear then
    Aliases.Clear;

  ValueMap := EnumValueMetadataTree.Items[ptruint(EnumType)];
  if ValueMap = nil then
    Exit;

  TypeData := GetTypeData(EnumType);
  Minvalue := TypeData^.MinValue;
  for I := 0 to (TypeData^.MaxValue - MinValue) do
  begin
    Entry := ValueMap[I];
    if Entry = nil then
      Continue;
    Aliases.Add(GetEnumName(EnumType, I+MinValue)+'='+Entry^.DisplayName);
  end;
end;

procedure SetupMetadataRegistry;
begin
  PropertyMetadataTree := TTypeMap.Create(3079);
  EnumValueMetadataTree := TEnumTypeMap.Create(3079);
end;

procedure BurnMetadataRegistry;
begin
  // Won't free this stuff. It only happens at the end of the application and
  // would waste time since the memory will be freed by the OS anyways.
end;

{ TGTPropertyDrag }

constructor TGTPropertyDrag.Create(AControl: TControl);
begin
  inherited Create(AControl);
end;

function TGTPropertyDrag.GetPropertyInfo: PPropInfo;
begin
  if FPropertyInfo = nil then
    FPropertyInfo := GetPropInfo(FOwningObject, FPropertyName);
  Result := FPropertyInfo;
end;

{ TGTPropertyGrid }

constructor TGTPropertyGrid.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  DragMode := dmManual;
  FOldHintCol := -1;
  FOldHintRow := -1;
end;

procedure TGTPropertyGrid.MouseMove(Shift: TShiftState; X, Y: integer);
var
  Item, Col: Integer;
  HintType: TPropEditHint;
begin
  inherited MouseMove(Shift, X, Y);
  if Dragging then
  begin
    if not FDragFinallyRunning then
      inherited;
    Exit;
  end;
  Item := MouseToIndex(Y, True);
  if Item >= 0 then
  begin
    if Y > SplitterX+1 then
    begin
      Col := 1;
      HintType := pehValue;
    end
    else
    begin
      HintType := pehName;
      Col := 0;
    end;
    if (FOldHintRow = Item) and (FOldHintCol = Col) then
      Exit;
    Hint := Rows[Item].Editor.GetHint(HintType, X, Y);
  end
  else if FOldHintRow <> -1 then
  begin
    Hint := '';
    FOldHintRow := -1;
    FOldHintCol := -1;
  end;

(*  if (ssLeft in Shift) and (Item >= 0) then
  begin
    //FFutureDragRow := Rows[Item];
    //WriteLn('Found valid row, initiiating drag.');
    WriteLn('Drag disabled');
    //BeginDrag(False);
    //FDragFinallyRunning := False;
  end;*)
//  else -- re-enable this when drag'n'drop gets implemented.
end;

function TGTPropertyGrid.DoDragMsg(ADragMessage: TDragMessage;
  APosition: TPoint; ADragObject: TDragObject; ATarget: TControl;
  ADocking: Boolean): LRESULT;
begin
  if not (ADragObject is TGTPropertyDrag) then
    Result:=inherited DoDragMsg(ADragMessage, APosition, ADragObject, ATarget,
      ADocking)
  else
  begin
    case ADragMessage of
      dmDragMove:
        WriteLn('Move');
    else
      Result:=inherited DoDragMsg(ADragMessage, APosition, ADragObject, ATarget,
        ADocking);
    end;
  end;
end;

procedure TGTPropertyGrid.DoStartDrag(var DragObject: TDragObject);
var
  RealRow: TOIPropertyGridRow;
begin
  if FFutureDragRow = nil then
  begin
    WriteLn('Drag started without proper row');
    inherited;
    Exit;
  end;
  if Assigned(DragObject) then
    DragObject.Free;
  DragObject := TGTPropertyDrag.Create(Self);
  with TGTPropertyDrag(DragObject) do
  begin
    FOwningObject := FFutureDragRow.Editor.GetComponent(0);
    FInspectedObject := TIObject;
    RealRow := FFutureDragRow;
    FPropertyName := FFutureDragRow.Editor.GetPropInfo^.Name;
    while (RealRow <> nil) and (RealRow.Name <> FPropertyName) do
      RealRow := RealRow.Parent;
    Assert(RealRow <> nil);
    FPropertyPath := Self.PropertyPath(RealRow);
    WriteLn('Dragging ', FPropertyPath, ' [', FPropertyName, ']');
  end;
  FDragFinallyRunning := True;
end;

procedure TGTPropertyGrid.DoEndDrag(Target: TObject; X, Y: Integer);
begin

end;

initialization
SetupMetadataRegistry;

finalization
BurnMetadataRegistry;


end.
