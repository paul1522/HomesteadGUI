unit vagrantprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Pipes;

type
  TVagrantProcess = Class(TProcess)
  private
    Procedure FreeOutputStreams;
  protected
    Procedure CreateStreams(InHandle,OutHandle,ErrHandle : Longint);virtual;
  end;

implementation

Procedure TVagrantProcess.CreateStreams(InHandle,OutHandle,ErrHandle : Longint);
begin
  FreeOutputStreams;
  FOutputStream:=TInputPipeStream.Create (OutHandle);
  FStderrStream:=TInputPipeStream.Create(ErrHandle);
end;

Procedure TVagrantProcess.FreeOutputStreams;
begin
  If FStderrStream<>FOutputStream then
    FreeStream(THandleStream(FStderrStream));
  FreeStream(THandleStream(FOutputStream));
end;

end.

