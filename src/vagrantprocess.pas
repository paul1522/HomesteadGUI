unit vagrantprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Pipes;

type
  TVagrantProcess = class(TProcess)
  private
    procedure FreeOutputStreams;
  protected
    procedure CreateStreams(InHandle, OutHandle, ErrHandle: longint); virtual;
  end;

implementation

procedure TVagrantProcess.CreateStreams(InHandle, OutHandle, ErrHandle: longint);
begin
  FreeOutputStreams;
  FOutputStream := TInputPipeStream.Create(OutHandle);
  FStderrStream := TInputPipeStream.Create(ErrHandle);
end;

procedure TVagrantProcess.FreeOutputStreams;
begin
  if FStderrStream <> FOutputStream then
    FreeStream(THandleStream(FStderrStream));
  FreeStream(THandleStream(FOutputStream));
end;

end.
