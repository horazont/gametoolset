{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit gametoolsetpkg; 

interface

uses
    GTBase, GTSession, GTRegistry, GTFileBase, GTEditor, GTFileTree, 
  GTRTTITable, GTFileUtils, GTVFS, GTStuff, GTPropertyGrid, GTConfig, 
  GTPropertyEditor, GTXDG, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GTFileTree', @GTFileTree.Register); 
  RegisterUnit('GTPropertyGrid', @GTPropertyGrid.Register); 
end; 

initialization
  RegisterPackage('gametoolsetpkg', @Register); 
end.
