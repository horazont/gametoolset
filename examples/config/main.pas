unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  RTTICtrls, GTConfig, GTPaths;

type

  { TPaths }

  TPaths = class(TGTPaths)
  public
    class function GetAppName: String; override;
  end;

  { TConfigClass }

  TConfigClass = class(TGTConfig)
  private
    FValue1: String;
    FValue2: String;
    FValue3: String;
    FValue4: Integer;
  published
    property Value1: String read FValue1 write FValue1;
    property Value2: String read FValue2 write FValue2;
    property Value3: String read FValue3 write FValue3;
    property Value4: Integer read FValue4 write FValue4;
  end;

  { TConfigForm }

  TConfigForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TIEdit1: TTIEdit;
    TIEdit2: TTIEdit;
    TIEdit3: TTIEdit;
    TISpinEdit1: TTISpinEdit;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ConfigForm: TConfigForm;
  Config: TConfigClass;

implementation

{ TConfigForm }

procedure TConfigForm.FormCreate(Sender: TObject);
begin
  TIEdit1.Link.TIObject := Config;
  TIEdit2.Link.TIObject := Config;
  TIEdit3.Link.TIObject := Config;
  TISpinEdit1.Link.TIObject := Config;
end;

{$R *.lfm}

{ TPaths }

class function TPaths.GetAppName: String;
begin
  Exit('gt-configtest');
end;

initialization
Paths := TPaths;
Config := TConfigClass.Create;
Config.Load;

finalization
Config.Save;
Config.Free;

end.

