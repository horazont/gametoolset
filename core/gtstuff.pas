unit GTStuff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTBase, GTXML;

type

  { TGTStringContainer }

  TGTStringContainer = class (TGTBaseObject)
  public
    constructor Create; override;
  private
    FValue: String;
  public
    procedure AssignTo(Dest: TPersistent); override;
    class function ClassToString: String; override;
    procedure LoadFromXML(const XMLNode: TxmlNode; Context: IGTLoaderContext=
                 nil); override;
    procedure SaveToXML(const XMLNode: TxmlNode); override;
    function ToString: String; override;
  published
    property Value: String read FValue write FValue stored false;
  end;

function FormatDataSize(const DataSize: SizeUInt): String;
function FormatDataSize(const DataSize: Double): String;

implementation

function FormatDataSize(const DataSize: SizeUInt): String;
begin
  Exit(FormatDataSize(Double(DataSize)));
end;

function FormatDataSize(const DataSize: Double): String;
const
  Suffixes : array [0..4] of String = ('B', 'KiB', 'MiB', 'GiB', 'TiB');
var
  I: Integer;
  Suffix: String;
  Value, NewValue: Double;
begin
  Value := DataSize;
  for I := 1 to 4 do
  begin
    NewValue := Value / 1024;
    if NewValue >= 1.0 then
    begin
      Value := NewValue;
    end
    else
    begin
      Suffix := Suffixes[I-1];
      Break;
    end;
  end;
  Exit(Format('%.2f %s', [Value, Suffix]));
end;

{ TGTStringContainer }

constructor TGTStringContainer.Create;
begin
  inherited Create;
  FValue := '';
end;

procedure TGTStringContainer.AssignTo(Dest: TPersistent);
begin
  if Dest is TGTStringContainer then
  begin
    TGTStringContainer(Dest).FValue := FValue;
  end
  else
    inherited AssignTo(Dest);
end;

class function TGTStringContainer.ClassToString: String;
begin
  Result := 'String';
end;

procedure TGTStringContainer.LoadFromXML(const XMLNode: TxmlNode;
  Context: IGTLoaderContext);
begin
  FValue := XMLNode.Content;
end;

procedure TGTStringContainer.SaveToXML(const XMLNode: TxmlNode);
begin
  XMLNode.Content := FValue;
end;

function TGTStringContainer.ToString: String;
begin
  Result := FValue;
end;

end.

