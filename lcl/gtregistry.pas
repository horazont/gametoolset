unit GTRegistry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTEditor, GTBase;

type
  TEditorEnumerator = procedure (AEditorClass: TGTEditorClass) of object;

procedure RegisterEditor(const AEditorClass: TGTEditorClass;
  const ObjClass: TGTBaseEditableClass);
function FindEditor(const AObject: TGTBaseEditable;
  const MinClass: TGTEditorClass = nil): TGTEditorClass;
procedure EnumerateEditors(Callback: TEditorEnumerator; SkipDuplicates: Boolean = True;
  const MinClass: TGTEditorClass = nil);

implementation

type
  TRegistryEntry = record
    EditorClass: TGTEditorClass;
    ObjClass: TGTBaseEditableClass;
  end;
  PRegistryEntry = ^TRegistryEntry;
  PPRegistryEntry = ^PRegistryEntry;

var
  EditorRegistry: TFPList = nil;

procedure RegisterEditor(const AEditorClass: TGTEditorClass;
  const ObjClass: TGTBaseEditableClass);
var
  Entry: PRegistryEntry;
begin
  if EditorRegistry = nil then
    EditorRegistry := TFPList.Create;
  Entry := GetMem(SizeOf(TRegistryEntry));
  Entry^.EditorClass := AEditorClass;
  Entry^.ObjClass := ObjClass;
  EditorRegistry.Add(Entry);
end;

function FindEditor(const AObject: TGTBaseEditable;
  const MinClass: TGTEditorClass = nil): TGTEditorClass;
var
  QuickList: PPointerList;
  I: Integer;
  Entry: PPRegistryEntry;
begin
  if EditorRegistry = nil then
  begin
    Result := nil;
    Exit;
  end;
  QuickList := EditorRegistry.List;
  Entry := PPRegistryEntry(@QuickList^[0]);
  if MinClass <> nil then
  begin
    for I := 0 to EditorRegistry.Count - 1 do
    begin
      if (AObject is Entry^^.ObjClass) and (Entry^^.EditorClass.InheritsFrom(MinClass)) then
      begin
        Result := Entry^^.EditorClass;
        Exit;
      end;
      Inc(Entry);
    end;
  end
  else
  begin
    for I := 0 to EditorRegistry.Count - 1 do
    begin
      if (AObject is Entry^^.ObjClass) then
      begin
        Result := Entry^^.EditorClass;
        Exit;
      end;
      Inc(Entry);
    end;
  end;
  Result := nil;
end;

procedure EnumerateEditors(Callback: TEditorEnumerator;
  SkipDuplicates: Boolean = True;
  const MinClass: TGTEditorClass = nil);
var
  QuickList: PPointerList;
  I: Integer;
  Entry: PPRegistryEntry;
  DupeList: TFPList;
begin
  if EditorRegistry = nil then
    Exit;
  QuickList := EditorRegistry.List;
  Entry := PPRegistryEntry(@QuickList^[0]);
  if SkipDuplicates then
  begin
    DupeList := TFPList.Create;
    try
      for I := 0 to EditorRegistry.Count - 1 do
      begin
        if (MinClass = nil) or (Entry^^.EditorClass.InheritsFrom(MinClass)) then
        begin
          if DupeList.IndexOf(Entry^^.EditorClass) < 0 then
          begin
            Callback(Entry^^.EditorClass);
            DupeList.Add(Entry^^.EditorClass);
          end;
        end;
        Inc(Entry);
      end;
    finally
      DupeList.Free;
    end;
  end
  else
  begin
    for I := 0 to EditorRegistry.Count - 1 do
    begin
      if (MinClass = nil) or (Entry^^.EditorClass.InheritsFrom(MinClass)) then
        Callback(Entry^^.EditorClass);
      Inc(Entry);
    end;
  end;
end;

end.

