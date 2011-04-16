{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit gtfile; 

interface

uses
  GTFileBase, GTFileUtils, GTSession, GTStreamUtils, GTURIAutoRegister, GTVFS, 
  GTProtocolFD, GTProtocolFile, GTProtocolVFS, GTVFSDirectoryMount, GTVFSXDG, 
  GTVFSConfig, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('gtfile', @Register); 
end.
