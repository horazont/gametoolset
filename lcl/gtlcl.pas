{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit gtlcl; 

interface

uses
  GTEditor, GTFileTree, GTPropertyEditor, GTPropertyGrid, GTRegistry, 
  GTRTTITable, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GTEditor', @GTEditor.Register); 
  RegisterUnit('GTFileTree', @GTFileTree.Register); 
  RegisterUnit('GTPropertyGrid', @GTPropertyGrid.Register); 
end; 

initialization
  RegisterPackage('gtlcl', @Register); 
end.
