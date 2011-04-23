unit GTOrderedQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type

  { TGTOrderedQueue }

  generic TGTOrderedQueue<_T> = class (TObject)
  private type
    PT = ^_T;
  var public
    constructor Create(const AOwnsItems: Boolean = False);
    destructor Destroy; override;
  private
    FCapacity, FCount: Integer;
    FItems: PT;
    FOwnsItems: Boolean;
  private
    procedure Expand;
    function GetItem(Index: Integer): _T;
  protected
    procedure FreeItem(var Item: _T); virtual;
    function GetValue(const Item: _T): Double; virtual;
  public
    procedure Add(const AItem: _T);
    procedure Clear;
    function Empty: Boolean;
    procedure Invalidate(const AItem: _T);
    procedure Remove(const AItem: _T);
    function PopFirst: _T;
  public
    property Items[Index: Integer]: _T read GetItem; default;
  published
    property Capacity: Integer read FCapacity;
    property Count: Integer read FCount;
  end;

implementation

{ TGTOrderedQueue }

constructor TGTOrderedQueue.Create(const AOwnsItems: Boolean);
begin
  FCapacity := 0;
  FCount := 0;
  FOwnsItems := AOwnsItems;
  FItems := nil;
end;

destructor TGTOrderedQueue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TGTOrderedQueue.Expand;
begin
  FCapacity += 128;
  ReallocMem(FItems, SizeOf(_T) * FCapacity);
end;

function TGTOrderedQueue.GetItem(Index: Integer): _T;
begin
  Exit(FItems[Index]);
end;

procedure TGTOrderedQueue.FreeItem(var Item: _T);
begin

end;

function TGTOrderedQueue.GetValue(const Item: _T): Double;
begin
  Exit(NaN);
end;

procedure TGTOrderedQueue.Add(const AItem: _T);
var
  NewValue, ThisValue: Double;
  ThisItem: _T;
  Added: Boolean;
  I: Integer;
begin
  if FCount = FCapacity then
    Expand;
  NewValue := GetValue(AItem);
  Added := False;
  for I := 0 to FCount - 1 do
  begin
    ThisItem := FItems[I];
    ThisValue := GetValue(ThisItem);
    if ThisValue < NewValue then
    begin
      Move(FItems[I], FItems[I+1], SizeOf(_T) * (FCount - I));
      FItems[I] := AItem;
      Added := True;
      Break;
    end;
  end;
  if not Added then
    FItems[FCount] := AItem;
  Inc(FCount);
end;

procedure TGTOrderedQueue.Clear;
var
  Item: PT;
  I: Integer;
begin
  if FOwnsItems then
  begin
    Item := FItems;
    for I := 0 to FCount - 1 do
    begin
      FreeItem(Item^);
      Inc(Item);
    end;
  end;
  FCount := 0;
end;

function TGTOrderedQueue.Empty: Boolean;
begin
  Exit(FCount = 0);
end;

procedure TGTOrderedQueue.Invalidate(const AItem: _T);
var
  I: Integer;
  NewValue, CurrValue: Double;
  CurrItem, TmpItem: _T;
  Inserted, Found: Boolean;
begin
  NewValue := GetValue(AItem);
  Inserted := False;
  Found := False;
  for I := 0 to FCount - 1 do
  begin
    CurrItem := FItems[I];
    CurrValue := GetValue(CurrItem);
    if Inserted then
    begin
      FItems[I] := TmpItem;
      if CurrItem = AItem then
        Exit;
    end
    else if Found then
    begin
      FItems[I-1] := FItems[I];
      if CurrValue <= NewValue then
      begin
        FItems[I] := AItem;
        Exit;
      end;
    end
    else
    begin
      if CurrItem = AItem then
      begin
        Found := True;
      end
      else if CurrValue <= NewValue then
      begin
        TmpItem := CurrItem;
        FItems[I] := AItem;
        Inserted := True;
      end;
    end;
  end;
  // the only way to end up here is if we found but did not insert or if we
  // inserted but did not find
  if Inserted then
  begin
    // new item inserted illegally using Invalidate. We'll accept that for now
    if FCount = FCapacity then
      Expand;
    FItems[FCount] := TmpItem;
    Inc(FCount);
  end
  else if Found then
  begin
    // in that case, all items had a higher value than this item, so put it at
    // the end of the list
    FItems[FCount-1] := AItem;
  end;
end;

procedure TGTOrderedQueue.Remove(const AItem: _T);
var
  Item: PT;
  I, J: Integer;
begin
  Item := FItems;
  J := -1;
  for I := 0 to FCount - 1 do
  begin
    if Item[0] = AItem then
    begin
      J := I+1;
      Inc(Item);
      Break;
    end;
    Inc(Item);
  end;
  if J < 0 then
    Exit;
  for I := J to FCount - 1 do
  begin
    Item[-1] := Item[0];
    Inc(Item);
  end;
  Dec(FCount);
end;

function TGTOrderedQueue.PopFirst: _T;
begin
  Result := FItems[0];
  Dec(FCount);
  Move(FItems[1], FItems[0], SizeOf(_T) * (FCount));
end;

end.

