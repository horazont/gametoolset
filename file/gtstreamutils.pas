unit GTStreamUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  EGTStreamError = class (Exception);

procedure CheckRead(const AStream: TStream; var Buffer; Count: Longint);

implementation

procedure CheckRead(const AStream: TStream; var Buffer; Count: Longint);
begin
  if AStream.Read(Buffer, Count) < Count then
    raise EGTStreamError.Create('Stream read error.');
end;

end.

