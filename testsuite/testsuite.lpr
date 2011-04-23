program testsuite;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
  {$ifdef UNIX}
  {$ifdef UseCThreads}cthreads, {$endif}
  {$endif}
  Classes, consoletestrunner, TestGTOrderedQueue, TestGTOrderedQueueCommons,
  gametoolsetpkg, TestGTThreadFIFO;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
