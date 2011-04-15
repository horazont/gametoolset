unit GTSynapseStreamWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock;

type

  { TSynapseStream }

  TSynapseStream = class (TStream)
  public
    constructor Create(const ASocket: TBlockSocket; AOwnsSocket: Boolean);
    destructor Destroy; override;
  private
    FOwnsSocket: Boolean;
    FSocket: TBlockSocket;
  protected
    function GetPosition: Int64; override;
    function GetSize: Int64; override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  public
    property Socket: TBlockSocket read FSocket;
  end;

implementation

{ TSynapseStream }

constructor TSynapseStream.Create(const ASocket: TBlockSocket;
  AOwnsSocket: Boolean);
begin
  inherited Create;
  FSocket := ASocket;
  FOwnsSocket := AOwnsSocket;
end;

destructor TSynapseStream.Destroy;
begin
  if FOwnsSocket then
  begin
    FSocket.CloseSocket;
    FSocket.Free;
  end;
  inherited Destroy;
end;

function TSynapseStream.GetPosition: Int64;
begin
  Exit(-1);
end;

function TSynapseStream.GetSize: Int64;
begin
  Exit(-1);
end;

function TSynapseStream.Read(var Buffer; Count: Longint): Longint;
begin
  Exit(FSocket.RecvBufferEx(@Buffer, Count, -1));
end;

function TSynapseStream.Write(const Buffer; Count: Longint): Longint;
begin
  Exit(FSocket.SendBuffer(@Buffer, Count));
end;

end.

