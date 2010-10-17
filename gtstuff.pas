unit GTStuff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTBase, xml;

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

implementation

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

