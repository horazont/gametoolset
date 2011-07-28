unit GTMainThreadNode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTNodes;

type

  { TGTMainThreadNodeThread }

  TGTMainThreadNodeThread = class (TGTNodeThread)
  protected
    procedure CommandSuspend; override;
  public
    procedure CheckCommand;
    procedure DoResume; override;
    procedure Execute; override;
    procedure WaitForCommand; override;
  end;

implementation

{ TGTMainThreadNodeThread }

procedure TGTMainThreadNodeThread.CommandSuspend;
begin
  // must not suspend here.
end;

procedure TGTMainThreadNodeThread.CheckCommand;
begin
  inherited CheckCommand;
end;

procedure TGTMainThreadNodeThread.DoResume;
begin
  // must not resume here.
end;

procedure TGTMainThreadNodeThread.Execute;
begin
  while not Terminated do
    Sleep(1);
end;

procedure TGTMainThreadNodeThread.WaitForCommand;
begin
  CheckCommand;
end;

end.

