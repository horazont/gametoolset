unit GTTemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTBase, GTSession, xml, GTReferenceManager;

type
  { TGTTemplate }

  TGTTemplate = class (TPersistent)
  public
    constructor Create(AClass: TGTBaseClass);
    destructor Destroy; override;
  private
    FInstance: TGTBaseObject;
    FName: String;
    FOnDestruction: TGTEventList;
    function GetOnPropUpdate: TGTPropChangeEventList;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(const Stream: TStream);
    procedure LoadFromXML(const XMLNode: TxmlNode);
    procedure SaveToStream(const Stream: TStream);
    procedure SaveToXML(const XMLNode: TxmlNode);
  published
    property Instance: TGTBaseObject read FInstance;
    property Name: String read FName write FName;
    property OnDestruction: TGTEventList read FOnDestruction;
    property OnPropUpdate: TGTPropChangeEventList read GetOnPropUpdate;
  end;

  TGTTemplateManager = specialize TGTReferenceManager<TGTTemplate>;

implementation

{ TGTTemplate }

constructor TGTTemplate.Create(AClass: TGTBaseClass);
begin
  inherited Create;
  FInstance := AClass.Create;
  FName := 'Nameless template';
  FOnDestruction := TGTEventList.Create;
end;

destructor TGTTemplate.Destroy;
begin
  FOnDestruction.Call(Self);
  FOnDestruction.Free;
  FInstance.Free;
  inherited Destroy;
end;

function TGTTemplate.GetOnPropUpdate: TGTPropChangeEventList;
begin
  Result := FInstance.OnPropChange;
end;

procedure TGTTemplate.AssignTo(Dest: TPersistent);
begin
  if (Dest is TGTBaseObject) then
    Dest.Assign(FInstance)
  else
    inherited AssignTo(Dest);
end;

procedure TGTTemplate.Assign(Source: TPersistent);
begin
  if (Source is TGTBaseObject) then
    FInstance.Assign(Source)
  else if (Source is TGTTemplate) then
  begin
    with TGTTemplate(Source) do
    begin
      Self.FInstance.Free;
      Self.FInstance := TGTBaseClass(FInstance.ClassType).Create;
      Self.FInstance.Assign(FInstance);
      Self.FName := FName;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TGTTemplate.LoadFromStream(const Stream: TStream);
begin

end;

procedure TGTTemplate.LoadFromXML(const XMLNode: TxmlNode);
begin

end;

procedure TGTTemplate.SaveToStream(const Stream: TStream);
begin

end;

procedure TGTTemplate.SaveToXML(const XMLNode: TxmlNode);
begin

end;

end.

