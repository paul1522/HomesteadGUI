unit homestead;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, data, Forms, StdCtrls, jwawinuser;

type
  THomesteadStatus = (
    HomesteadUp,
    HomesteadHalted,
    HomesteadSuspended,
    HomesteadDestroyed
    );

  THomestead = class(TObject)
  public
    procedure Up(var Output: string; Console: TMemo);
    procedure Halt(var Output: string; Console: TMemo);
    procedure Suspend(var Output: string; Console: TMemo);
    procedure Resume(var Output: string; Console: TMemo);
    procedure Provision(var Output: string; Console: TMemo);
    procedure Reload(var Output: string; Console: TMemo);
    procedure DestroyBox(var Output: string; Console: TMemo);
    procedure ssh;
    procedure cmd;
    procedure Backup(var Output: string; Console: TMemo);
    procedure Restore(var Output: string; Console: TMemo);
    procedure DetatchProcess(ExeFile: string; Arg: string = '');
    function Status(var Output: string): THomesteadStatus;
  private
    procedure Vagrant(Arg: string; var Output: string);
    procedure Vagrant(Arg: string; var Output: string; Console: TMemo);
    procedure Vagrant(Arg: string; Async: boolean);
    //procedure Vagrant(Args : TStrings; Console : TMemo);
    //procedure Vagrant(Args : TStrings; var Output : string; Console : TMemo);
  end;

const
  INI_FILE_NAME = 'HomesteadGUI.ini';

var
  AHomestead: THomestead;

implementation

procedure THomestead.Up(var Output: string; Console: TMemo);
begin
  Vagrant('up', Output, Console);
end;

procedure THomestead.Halt(var Output: string; Console: TMemo);
begin
  Vagrant('halt', Output, Console);
end;

procedure THomestead.Suspend(var Output: string; Console: TMemo);
begin
  Vagrant('suspend', Output, Console);
end;

procedure THomestead.Resume(var Output: string; Console: TMemo);
begin
  Vagrant('resume', Output, Console);
end;

procedure THomestead.Provision(var Output: string; Console: TMemo);
begin
  Vagrant('provision', Output, Console);
end;

procedure THomestead.Reload(var Output: string; Console: TMemo);
begin
  Vagrant('reload', Output, Console);
end;

procedure THomestead.DestroyBox(var Output: string; Console: TMemo);
begin
  Vagrant('destroy', Output, Console);
end;

procedure THomestead.ssh;
begin
  Vagrant('ssh', True);
end;

procedure THomestead.cmd;
begin
  DetatchProcess('cmd');
end;

procedure THomestead.Backup(var Output: string; Console: TMemo);
var
  BackupCommand: TStrings;
begin
  //Vagrant(BackupCommand, Console);
end;

procedure THomestead.Restore(var Output: string; Console: TMemo);
var
  RestoreCommand: TStrings;
begin
  //Vagrant(RestoreCommand, Console);
end;

function THomestead.Status(var Output: string): THomesteadStatus;
begin
  Vagrant('status', Output);
  if Pos(',state,running', Output) <> 0 then
    Status := HomesteadUp
  else if Pos(',state,saved', Output) <> 0 then
    Status := HomesteadSuspended
  else if Pos(',state,not_created', Output) <> 0 then
    Status := HomesteadDestroyed
  else
    Status := HomesteadHalted;
end;

procedure THomestead.DetatchProcess(ExeFile: string; Arg: string = '');
var
  Process: TProcess;
  I: integer;
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
  end;
end;

procedure THomestead.Vagrant(Arg: string; Async: boolean);
begin
  DetatchProcess(Global.VagrantCmd, Arg);
end;

procedure THomestead.Vagrant(Arg: string; var Output: string);
var
  Console: TMemo;
begin
  try
    Console := TMemo.Create(nil);
    Vagrant(Arg, Output, Console)
  finally
    Console.Free
  end;
end;

procedure THomestead.Vagrant(Arg: string; var Output: string; Console: TMemo);
const
  BUF_SIZE = 2048;
var
  VagrantProcess: TProcess;
  OutputStream: TMemoryStream;
  BytesRead: longint;
  Buffer: array[1..BUF_SIZE] of byte;
  p: int64;
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
    Console.Lines.LoadFromStream(OutputStream);

    Console.SelStart := Length(Console.Text);
    Console.SelLength := 0;
    Console.Perform(EM_SCROLLCARET, 0, 0);
    Console.SetFocus;

    OutputStream.Position := p;
    Application.ProcessMessages
  until BytesRead = 0;
  VagrantProcess.Free;
  Output := Console.Text;
end;

(*
procedure THomestead.Vagrant(Args : TStrings; var Output : string; Console : TStrings);
*)

begin
  AHomestead := THomestead.Create

end.
