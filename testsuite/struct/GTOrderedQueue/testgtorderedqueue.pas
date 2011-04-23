unit TestGTOrderedQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TestGTOrderedQueueCommons;

type

  { TTestGTOrderedQueue }

  TTestGTOrderedQueue= class(TTestCase)
  private
    Queue: TComplexQueue;
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure TestAddRemove;
    procedure TestInvalidate;
  end; 

implementation

procedure TTestGTOrderedQueue.TestAddRemove;
var
  Item1, Item2: PComplex;
begin
  AssertTrue('Queue.Empty', Queue.Empty);
  New(Item1);
  Item1^ := Complex(1, 0);
  New(Item2);
  Item2^ := Complex(0, 2);
  Queue.Add(Item1);
  AssertEquals(1, Queue.Count);
  Queue.Add(Item2);
  AssertEquals(2, Queue.Count);
  Queue.Remove(Item2);
  AssertEquals(1, Queue.Count);
  AssertTrue('Call to Queue.PopFirst should return Item1.', Queue.PopFirst = Item1);
  Dispose(Item1);
  Dispose(Item2);
end;

procedure TTestGTOrderedQueue.TestInvalidate;
var
  Item1, Item2, Item3: PComplex;
begin
  New(Item1);
  New(Item2);
  New(Item3);
  try
    Item1^ := Complex(1, 0);
    Item2^ := Complex(0, 2);
    Item3^ := Complex(1, 1);
    // Item1 < Item3 < Item2
    Queue.Add(Item1);
    Queue.Add(Item2);
    Queue.Add(Item3);
    AssertEquals(3, Queue.Count);
    AssertTrue('Queue[0] = Item2', Queue[0] = Item2);
    AssertTrue('Queue[1] = Item3', Queue[1] = Item3);
    AssertTrue('Queue[2] = Item1', Queue[2] = Item1);
    Item3^.Im := 100;
    Queue.Invalidate(Item3);
    AssertTrue('Queue[0] = Item3', Queue[0] = Item3);
    AssertTrue('Queue[1] = Item2', Queue[1] = Item2);
    AssertTrue('Queue[2] = Item1', Queue[2] = Item1);
  finally
    Dispose(Item3);
    Dispose(Item2);
    Dispose(Item1);
  end;
end;

procedure TTestGTOrderedQueue.SetUp;
begin
  Queue := TComplexQueue.Create(False);
end; 

procedure TTestGTOrderedQueue.TearDown;
begin
  Queue.Free;
end; 

initialization

  RegisterTest(TTestGTOrderedQueue);
end.

