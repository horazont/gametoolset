{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit gtnode; 

interface

uses
  GTNodeController, GTNodes, GTNodeDataTypeBlockMemory, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('gtnode', @Register); 
end.
