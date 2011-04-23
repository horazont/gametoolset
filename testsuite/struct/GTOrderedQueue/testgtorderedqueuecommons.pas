unit TestGTOrderedQueueCommons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTOrderedQueue;

type
  TComplex = record
    Re, Im: Double;
  end;
  PComplex = ^TComplex;

  TComplexQueue_ = specialize TGTOrderedQueue<PComplex>;

  { TComplexQueue }

  TComplexQueue = class (TComplexQueue_)
  protected
    procedure FreeItem(var Item: PComplex); override;
    function GetValue(const Item: PComplex): Double; override;
  end;

function Complex(const Re, Im: Double): TComplex;

implementation

function Complex(const Re, Im: Double): TComplex;
begin
  Result.Re := Re;
  Result.Im := Im;
end;

{ TComplexQueue }

procedure TComplexQueue.FreeItem(var Item: PComplex);
begin
  Dispose(Item);
end;

function TComplexQueue.GetValue(const Item: PComplex): Double;
begin
  Result := sqrt(sqr(Item^.Re) + sqr(Item^.Im));
end;

end.

