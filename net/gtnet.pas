{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit gtnet; 

interface

uses
    GTSynapseStreamWrapper, GTProtocolTCP, GTProtocolTCPSSL, 
  GTProtocolUnixSock, GTUnixSockStream, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('gtnet', @Register); 
end.
