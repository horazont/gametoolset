unit TestGTThreadFIFO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, GTThreadFIFO;

type

  TTestGTThreadFIFO= class(TTestCase)
  private
    Fifo: TGTThreadFIFO;
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure TestPushPop;
  end; 

implementation

procedure TTestGTThreadFIFO.TestPushPop;
var
  P1, P2: Pointer;
begin
  P1 := Pointer($1);
  P2 := Pointer($2);
  AssertNull('Fifo.Pop', Fifo.Pop);
  AssertTrue('Fifo.Empty', Fifo.Empty);
  Fifo.Push(P1);
  AssertFalse('Fifo.Empty', Fifo.Empty);
  Fifo.Push(P2);
  AssertTrue('Fifo.Pop = P1', Fifo.Pop = P1);
  AssertTrue('Fifo.PopBlocking = P2', Fifo.PopBlocking = P2);
  AssertTrue('Fifo.Pop = nil', Fifo.Pop = nil);
  AssertTrue('Fifo.Empty', Fifo.Empty);
end; 

procedure TTestGTThreadFIFO.SetUp; 
begin
  Fifo := TGTThreadFIFO.Create;
end; 

procedure TTestGTThreadFIFO.TearDown; 
begin
  Fifo.Free;
end; 

initialization

  RegisterTest(TTestGTThreadFIFO); 
end.

