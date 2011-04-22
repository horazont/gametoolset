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
  Inc(FCount);
  if not Added then
    FItems[FCount] := AItem;
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
  Exit(FCount > 0);
end;

procedure TGTOrderedQueue.Invalidate(const AItem: _T);
var
  I: Integer;
  NewValue: Double;
  Inserted: Boolean;
begin
  NewValue := GetValue(AItem);
  Inserted := False;
  for I := 0 to FCount - 1 do
  begin

  end;
end;

procedure TGTOrderedQueue.Remove(const AItem: _T);
begin

end;

function TGTOrderedQueue.PopFirst: _T;
begin

end;

end.

