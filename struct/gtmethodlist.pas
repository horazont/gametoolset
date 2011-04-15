unit GTMethodList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  PMethod = ^TMethod;

  TGTMethodList = class;

  TGTMethodListIterationCallback = procedure (const AList: TGTMethodList; const AMethod: TMethod) of object;

  TGTMethodListContainer = specialize TFPGList<PMethod>;

  { TGTMethodList }

  TGTMethodList = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FList: TGTMethodListContainer;
  public
    procedure AddMethod(const AMethod: TMethod);
    procedure Clear;
    function FindMethod(const AMethod: TMethod): Integer;
    procedure ForEach(const ACallback: TGTMethodListIterationCallback);
    procedure RemoveMethod(const AMethod: TMethod);
    procedure RemoveMethod(const AIndex: Integer);
  end;

implementation

{ TGTMethodList }

constructor TGTMethodList.Create;
begin
  FList := TGTMethodListContainer.Create;
end;

destructor TGTMethodList.Destroy;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    FreeMem(FList[I]);
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TGTMethodList.AddMethod(const AMethod: TMethod);
var
  Method: PMethod;
begin
  Method := GetMem(SizeOf(TMethod));
  Move(AMethod, Method^, SizeOf(TMethod));
  FList.Add(Method);
end;

procedure TGTMethodList.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    FreeMem(FList[I]);
  FList.Clear;
end;

function TGTMethodList.FindMethod(const AMethod: TMethod): Integer;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
  begin
    if (CompareMem(FList[I], @AMethod, SizeOf(TMethod))) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TGTMethodList.ForEach(const ACallback: TGTMethodListIterationCallback);
var
  I: Integer;
  Duplicate: TGTMethodListContainer;
begin
  Duplicate := TGTMethodListContainer.Create;
  try
    Duplicate.Assign(FList);
    for I := 0 to Duplicate.Count - 1 do
      ACallback(Self, Duplicate[I]^);
  finally
    Duplicate.Free;
  end;
end;

procedure TGTMethodList.RemoveMethod(const AMethod: TMethod);
var
  I: Integer;
begin
  I := FindMethod(AMethod);
  if I < 0 then
    Exit;
  FreeMem(FList[I]);
  FList.Delete(I);
end;

procedure TGTMethodList.RemoveMethod(const AIndex: Integer);
begin
  FreeMem(FList[AIndex]);
  FList.Delete(AIndex);
end;

end.

