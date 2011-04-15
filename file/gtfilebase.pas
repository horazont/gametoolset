unit GTFileBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gtbase;

type

  { TGTFileNode }

  TGTFileNode = class (TGTBaseEditable)
  public
    constructor Create(AFileName: String); virtual;
    destructor Destroy; override;
  private
    FFileName: String;
    FOnChange: TGTEventList;
  public
    procedure NotifyChange;
    function ToString: String; override;
  public
    property OnChange: TGTEventList read FOnChange;
  published
    property FileName: String read FFileName;
  end;
  TGTFileClass = class of TGTFileNode;

implementation

{ TGTFileNode }

constructor TGTFileNode.Create(AFileName: String);
begin
  inherited Create;
  FFileName := AFileName;
  FOnChange := TGTEventList.Create;
end;

destructor TGTFileNode.Destroy;
begin
  FOnChange.Free;
  inherited Destroy;
end;

procedure TGTFileNode.NotifyChange;
begin
  FOnChange.Call(Self);
end;

function TGTFileNode.ToString: String;
begin
  Result := FFileName;
end;

end.

