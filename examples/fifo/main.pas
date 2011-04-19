unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, GTThreadFIFO, math;

type
  TProcessingInput = record
    Max: Integer;
  end;
  PProcessingInput = ^TProcessingInput;

  TProcessingOutput = record
    Max: Integer;
    ProcessingThreadID: TThreadID;
    Result: Double;
  end;
  PProcessingOutput = ^TProcessingOutput;

  { TFibThread }

  TFibThread = class (TThread)
  public
    constructor Create(const FifoIn, FifoOut: TGTThreadFIFO);
  private
    FFifoIn, FFifoOut: TGTThreadFIFO;
  protected
    function IterativeFibonacci(const Max: Integer): Double;
  public
    procedure Execute; override;
  end;

  { TForm1 }

TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Button1: TButton;
    Label1: TLabel;
    LIndex: TLabel;
    MResults: TMemo;
    SNumber: TSpinEdit;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFifoToThread, FFifoFromThread: TGTThreadFIFO;
    FFibThread1, FFibThread2: TFibThread;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
var
  Data: PProcessingOutput;
begin
  if not FFifoFromThread.Empty then
  begin
    Data := PProcessingOutput(FFifoFromThread.Pop);
    // no need to check for nil, as we are only reading from this thread
    MResults.Lines.Add(Format('F(%d)=%16f (thread: %16.16x)', [Data^.Max, Data^.Result, ptrint(Data^.ProcessingThreadID)]));
    Dispose(Data);
  end;
  Done := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Input: PProcessingInput;
begin
  New(Input);
  Input^.Max := SNumber.Value;
  FFifoToThread.Push(Input);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FFifoFromThread := TGTThreadFIFO.Create;
  FFifoToThread := TGTThreadFIFO.Create;
  FFibThread1 := TFibThread.Create(FFifoToThread, FFifoFromThread);
  FFibThread2 := TFibThread.Create(FFifoToThread, FFifoFromThread);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FFibThread2.Terminate;
  FFibThread1.Terminate;
  FFibThread2.WaitFor;
  FFibThread1.WaitFor;
  // actually, we would have to dispose the data in left the fifos ourselves,
  // but I'm ignoring that for this example
  FFifoFromThread.Free;
  FFifoToThread.Free;
end;

{$R *.lfm}

{ TFibThread }

constructor TFibThread.Create(const FifoIn, FifoOut: TGTThreadFIFO);
begin
  inherited Create(False);
  FFifoIn := FifoIn;
  FFifoOut := FifoOut;
  // strip fpu exceptions
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
                   exOverflow, exUnderflow, exPrecision]);
end;

function TFibThread.IterativeFibonacci(const Max: Integer): Double;
var
  I: Integer;
  J, K: Double;
begin
  if (Max <= 0) then
    Exit(NaN)
  else if (Max = 1) then
    Exit(1)
  else if (Max = 2) then
    Exit(1);
  Result := 0;
  J := 1;
  K := 1;
  I := 2;
  while I < Max do
  begin
    Result := J;
    J := K + J;
    K := Result;
    Inc(I);
    Sleep(1); // just to make it slooow
  end;
  Result := J;
end;

procedure TFibThread.Execute;
var
  Input: PProcessingInput;
  Output: PProcessingOutput;
begin
  try
    while not Terminated do
    begin
      while FFifoIn.Empty do
      begin
        // this workaround is needed until timeout for WaitData is implemented
        if Terminated then
          Exit;
        Sleep(1);
      end;

      // Get our data from the fifo
      Input := PProcessingInput(FFifoIn.Pop);
      // we have to check for nil here, a concurrent thread might have been
      // faster than we were
      if Input = nil then
        Continue;

      // We have to allocate the structs explicitly. Just using them from stack
      // (i.e. as non-pointer variables) *will* fail!
      New(Output);

      // Do the heavy calculations
      Output^.Max := Input^.Max;
      Output^.ProcessingThreadID := ThreadID;
      Output^.Result := IterativeFibonacci(Input^.Max);

      // Dispose the input struct
      Dispose(Input);

      // And push the output
      FFifoOut.Push(Output);
    end;
  except

  end;
end;

end.

