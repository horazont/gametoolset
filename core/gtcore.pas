{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit gtcore; 

interface

uses
  GTBase, GTConfig, GTDebug, GTPaths, GTStuff, GTURI, GTXML, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('gtcore', @Register); 
end.
