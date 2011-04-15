{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit gtlcl; 

interface

uses
    GTFileTree, GTPropertyEditor, GTPropertyGrid, GTRTTITable, GTEditor, 
  GTRegistry, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GTFileTree', @GTFileTree.Register); 
  RegisterUnit('GTPropertyGrid', @GTPropertyGrid.Register); 
  RegisterUnit('GTEditor', @GTEditor.Register); 
end; 

initialization
  RegisterPackage('gtlcl', @Register); 
end.
