(*******************************************************************************
** File Name: gtintmap.pas
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
unit GTIntMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { TGTIntMap }

  generic TGTIntMap<TValueType> = class (TObject)
  public
    constructor Create(InitialSize: Integer);
  private
    FMap: array of TValueType;
  protected
    function GetItem(Key: Integer): TValueType;
    procedure SetItem(Key: Integer; AValue: TValueType);
  public
    function DeleteItem(Key: Integer): TValueType;
  public
    property Items[Key: Integer]: TValueType read GetItem write SetItem; default;
  end;

  { TGTIntHashMap }
  TKeyType = ptruint;

  generic TGTIntHashMap<TValueType> = class (TObject)
  type public
    TEntry = record
      Key: TKeyType;
      Value: TValueType;
    end;
    PEntry = ^TEntry;
  var public
    constructor Create(ATableSize: Integer);
    destructor Destroy; override;
  private
    FEnumI: Integer;
    FEnumJ: Integer;
    FEnumList: TFPList;
    FTable: array of TFPList;
    FTableSize: Integer;
  protected
    function GetList(Key: TKeyType): TFPList;
    function GetOrCreateList(Key: TKeyType): TFPList;
    function GetItem(Key: TKeyType): TValueType;
    function IsKeyInList(AList: TFPList; AKey: TKeyType): Boolean;
    procedure SetItem(Key: TKeyType; AValue: TValueType);
  public
    procedure Clear;
    procedure DeleteItem(Key: TKeyType);
    function EnumGetFirst: TValueType;
    function EnumGetNext: TValueType;
    function HasKey(Key: TKeyType): Boolean;
  public
    property Items[Key: TKeyType]: TValueType read GetItem write SetItem; default;
  end;

implementation

{ TstwGIntMap }

constructor TGTIntMap.Create(InitialSize: Integer);
begin
  SetLength(FMap, InitialSize);
end;

function TGTIntMap.GetItem(Key: Integer): TValueType;
begin
  if (Key < 0) or (Key > High(FMap)) then
    Exit(nil);
  Result := FMap[Key];
end;

procedure TGTIntMap.SetItem(Key: Integer; AValue: TValueType);
begin
  if (Key < 0) then
    raise EListError.CreateFmt('Invalid key ''%d''.', [Key]);
  if Key > High(FMap) then
    SetLength(FMap, Key+1);
  FMap[Key] := AValue;
end;

function TGTIntMap.DeleteItem(Key: Integer): TValueType;
begin
  Result := FMap[Key];
  FMap[Key] := nil;
end;

{ TstwGIntHashMap }

constructor TGTIntHashMap.Create(ATableSize: Integer);
begin
  FTableSize := ATableSize;
  SetLength(FTable, FTableSize);
  FillByte(FTable[0], SizeOf(Pointer)*FTableSize, 0);
end;

destructor TGTIntHashMap.Destroy;
var
  I, J: Integer;
  List: TFPList;
  Entry: PEntry;
begin
  for I := 0 to High(FTable) do
  begin
    List := FTable[I];
    if TObject(List) <> nil then
    begin
      for J := 0 to List.Count - 1 do
      begin
        Entry := List[J];
        Dispose(Entry);
      end;
      List.Free;
      FTable[I] := nil;
    end;
  end;
  inherited Destroy;
end;

function TGTIntHashMap.GetList(Key: TKeyType): TFPList;
begin
  Result := FTable[Key mod FTableSize];
end;

function TGTIntHashMap.GetOrCreateList(Key: TKeyType): TFPList;
var
  Pos: Integer;
begin
  Pos := Key mod FTableSize;
  Result := FTable[Pos];
  if Result = nil then
  begin
    Result := TFPList.Create;
    FTable[Pos] := Result;
  end;
end;

function TGTIntHashMap.GetItem(Key: TKeyType): TValueType;
var
  List: TFPList;
  I: Integer;
  Item: PEntry;
begin
  List := GetList(Key);
  if List = nil then
  begin
    WriteLn('List not found.');
    Exit(nil);
  end;
  for I := 0 to List.Count - 1 do
  begin
    Item := PEntry(List[I]);
    if Item^.Key = Key then
      Exit(Item^.Value);
  end;
  WriteLn('Key not found.');
  Exit(nil);
end;

function TGTIntHashMap.IsKeyInList(AList: TFPList; AKey: TKeyType): Boolean;
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    if PEntry(AList[I])^.Key = AKey then
      Exit(True);
  Result := False;
end;

procedure TGTIntHashMap.SetItem(Key: TKeyType; AValue: TValueType);
var
  List: TFPList;
  NewEntry: PEntry;
begin
  WriteLn('assigning ', IntToHex(ptrint(AValue), 16), ' to ', IntTohex(ptrint(Key), 16));
  List := GetOrCreateList(Key);
  if IsKeyInList(List, Key) then
    raise EListError.CreateFmt('Duplicte key: %d', [Key]);
  New(NewEntry);
  NewEntry^.Key := Key;
  WriteLn('k=', IntToHex(NewEntry^.Key, 16));
  NewEntry^.Value := AValue;
  List.Add(NewEntry);
end;

procedure TGTIntHashMap.Clear;
var
  I, J: Integer;
  List: TFPList;
begin
  for I := 0 to High(FTable) do
  begin
    List := FTable[I];
    if List <> nil then
    begin
      for J := 0 to List.Count - 1 do
      begin
        FreeMem(List[J]);
      end;
      FreeAndNil(FTable[I]);
    end;
  end;

end;

procedure TGTIntHashMap.DeleteItem(Key: TKeyType);
var
  List: TFPList;
  I: Integer;
  Entry: PEntry;
begin
  List := GetList(Key);
  if List = nil then
    Exit;
  for I := 0 to List.Count - 1 do
  begin
    Entry := PEntry(List[I]);
    if Entry^.Key = Key then
    begin
      Dispose(Entry);
      List.Delete(I);
      Exit;
    end;
  end;
end;

function TGTIntHashMap.EnumGetFirst: TValueType;
begin
  FEnumI := -1;
  FEnumJ := -1;
  FEnumList := nil;
  Exit(EnumGetNext);
end;

function TGTIntHashMap.EnumGetNext: TValueType;
var
  Max: Integer;
begin
  if FEnumList = nil then
  begin
    Max := high(FTable);
    repeat
      Inc(FEnumI);
      if FEnumI > Max then
        Exit(nil);
      FEnumList := FTable[FEnumI];
    until FEnumList <> nil;
  end;
  FEnumJ := 0;
  if FEnumJ > FEnumList.Count - 1 then
  begin
    FEnumList := nil;
    Exit(EnumGetNext);
  end;
  Result := PEntry(FEnumList[FEnumJ])^.Value;
  Inc(FEnumJ);
end;

function TGTIntHashMap.HasKey(Key: TKeyType): Boolean;
var
  List: TFPList;
begin
  List := GetList(Key);
  if List = nil then
    Exit(False);
  Exit(IsKeyInList(List, Key));
end;

end.

