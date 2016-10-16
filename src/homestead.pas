unit homestead;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, data, Forms;

type
  THomesteadStatus = (
    HomesteadUp,
    HomesteadHalted,
    HomesteadSuspended,
    HomesteadDestroyed
  );

  THomestead = class(TObject)
    public
      procedure Up(var Output : String; Console : TStrings);
      procedure Halt(var Output : String; Console : TStrings);
      procedure Suspend(var Output : String; Console : TStrings);
      procedure Resume(var Output : String; Console : TStrings);
      procedure Provision(var Output : String; Console : TStrings);
      procedure Reload(var Output : String; Console : TStrings);
      procedure DestroyBox(var Output : String; Console : TStrings);
      procedure ssh;
      procedure cmd;
      procedure DetatchProcess(ExeFile : string; Arg : String = '');
      function Status(var Output : String) : THomesteadStatus;
    private
      procedure Vagrant(Arg : string; var Output : string);
      procedure Vagrant(Arg : string; var Output : string; Console : TStrings);
      procedure Vagrant(Arg : string; Async : boolean);
  end;

const
  INI_FILE_NAME = 'HomesteadGUI.ini';

var
  AHomestead : THomestead;

implementation

procedure THomestead.Up(var Output : String; Console : TStrings);
begin
  Vagrant('up', Output, Console)
end;

procedure THomestead.Halt(var Output : String; Console : TStrings);
begin
  Vagrant('halt', Output, Console)
end;

procedure THomestead.Suspend(var Output : String; Console : TStrings);
begin
  Vagrant('suspend', Output, Console)
end;

procedure THomestead.Resume(var Output : String; Console : TStrings);
begin
  Vagrant('resume', Output, Console)
end;

procedure THomestead.Provision(var Output : String; Console : TStrings);
begin
  Vagrant('provision', Output, Console)
end;

procedure THomestead.Reload(var Output : String; Console : TStrings);
begin
  Vagrant('reload', Output, Console)
end;

procedure THomestead.DestroyBox(var Output : String; Console : TStrings);
begin
  Vagrant('destroy', Output, Console)
end;

procedure THomestead.ssh;
begin
  Vagrant('ssh', true)
end;

procedure THomestead.cmd;
begin
  DetatchProcess('cmd')
end;

function THomestead.Status(var Output : String) : THomesteadStatus;
begin
  Vagrant('status', Output);
  if Pos(',state,running', Output) <> 0 then
     Status := HomesteadUp
  else if Pos(',state,saved', Output) <> 0 then
     Status := HomesteadSuspended
  else if Pos(',state,not_created', Output) <> 0 then
     Status := HomesteadDestroyed
  else
     Status := HomesteadHalted
end;

procedure THomestead.DetatchProcess(ExeFile : string; Arg : string = '');
var
  Process: TProcess;
  I: Integer;
begin
  Process := TProcess.Create(nil);
  try
    Process.InheritHandles := False;
    Process.Options := [];
    Process.ShowWindow := swoShow;

    // Copy default environment variables including DISPLAY variable for GUI application to work
    for I := 1 to GetEnvironmentVariableCount do
      Process.Environment.Add(GetEnvironmentString(I));

    Process.Executable := ExeFile;
    Process.CurrentDirectory := Global.HomesteadDir;
    if Arg <> '' then
      Process.Parameters.Add(Arg);
    Process.Execute
  finally
    Process.Free
  end
end;

procedure THomestead.Vagrant(Arg : string; Async : boolean);
begin
  DetatchProcess(Global.VagrantCmd, Arg)
end;

procedure THomestead.Vagrant(Arg : string; var Output : string);
var
  Console : TStrings;
begin
  try
    Console := TStringList.Create;
    Vagrant(Arg, Output, Console)
  finally
    Console.Free
  end
end;

procedure THomestead.Vagrant(Arg : string; var Output : string; Console : TStrings);
const
  BUF_SIZE = 2048;
var
  VagrantProcess : TProcess;
  OutputStream : TMemoryStream;
  BytesRead    : longint;
  Buffer       : array[1..BUF_SIZE] of byte;
  p : int64;
begin
  VagrantProcess := TProcess.Create(nil);
  VagrantProcess.Executable := Global.VagrantCmd;
  VagrantProcess.CurrentDirectory := Global.HomesteadDir;
  VagrantProcess.Parameters.Add(Arg);
  if Arg = 'status' then
    VagrantProcess.Parameters.Add('--machine-readable');
  if Arg = 'destroy' then
    VagrantProcess.Parameters.Add('--force');
  VagrantProcess.Options := VagrantProcess.Options + [poUsePipes];
  VagrantProcess.ShowWindow := swoHIDE;
  VagrantProcess.Execute;
  OutputStream := TMemoryStream.Create;
  repeat
    BytesRead := VagrantProcess.Output.Read(Buffer, BUF_SIZE);
    OutputStream.Write(Buffer, BytesRead);
    p := OutputStream.Position;
    OutputStream.Position := 0;
    Console.LoadFromStream(OutputStream);
    OutputStream.Position := p;
    Application.ProcessMessages
  until BytesRead = 0;
  VagrantProcess.Free;
  Output := Console.Text
end;


begin
  AHomestead := THomestead.Create

end.

