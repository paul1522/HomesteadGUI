unit homestead;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, VagrantProcess, Data, Forms,
  StdCtrls, jwawinuser, yaris_options;

type
  THomesteadStatus = (
    HomesteadUp,
    HomesteadHalted,
    HomesteadSuspended,
    HomesteadDestroyed
    );

  THomestead = class(TObject)
  public
    procedure ssh;
    procedure cmd;
    procedure PowerShell;
    procedure Yaris(Y: YarisOptions; Console: TMemo);
    procedure Up(var Output: string; Console: TMemo);
    procedure Halt(var Output: string; Console: TMemo);
    procedure Suspend(var Output: string; Console: TMemo);
    procedure Resume(var Output: string; Console: TMemo);
    procedure Provision(var Output: string; Console: TMemo);
    procedure Reload(var Output: string; Console: TMemo);
    procedure DestroyBox(var Output: string; Console: TMemo);
    procedure Backup(var Output: string; Console: TMemo);
    procedure Restore(var Output: string; Console: TMemo);
    procedure DetatchProcess(ExeFile: string; Arg: string = '');
    function Status(var Output: string): THomesteadStatus;
  private
    procedure Vagrant(Arg: string; var Output: string);
    procedure Vagrant(Arg: string; Async: boolean);
    procedure Vagrant(Arg: string; var Output: string; Console: TMemo);
    procedure Vagrant(Args: TStrings; var Output: string; Console: TMemo);
    procedure VagrantSsh(Arg: string; Console: TMemo);
  end;

const
  INI_FILE_NAME = 'HomesteadGUI.ini';

var
  AHomestead: THomestead;

implementation

procedure THomestead.Yaris(Y: YarisOptions; Console: TMemo);
var
  Command: string;
begin
  Command := 'yaris';
  if ysDev in Y.Switches then
    Command := Command + ' --dev';
  if ysAuth in Y.Switches then
    Command := Command + ' --auth';
  if ysNode in Y.Switches then
    Command := Command + ' --node';
  if ysBranch in Y.Switches then
    Command := Command + ' --branch=' + Y.BranchName;
  if ysStorm in Y.Switches then
    Command := Command + ' --jetbrains';
  if ysVoyager in Y.Switches then
    Command := Command + ' --voyager';
  if Global.NewProjectPath <> '' then
    Command := Command + ' --path=' + Global.NewProjectPath;
  Command := Command + ' ' + Y.ProjectName;
  VagrantSsh(Command, Console);
end;

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

procedure THomestead.Backup(var Output: string; Console: TMemo);
begin
  VagrantSsh('dbexport', Console);
end;

procedure THomestead.Restore(var Output: string; Console: TMemo);
begin
  VagrantSsh('dbimport', Console);
end;

procedure THomestead.ssh;
begin
  Vagrant('ssh', True);
end;

procedure THomestead.cmd;
begin
  DetatchProcess('cmd');
end;

procedure THomestead.PowerShell;
begin
  DetatchProcess('powershell');
end;

function THomestead.Status(var Output: string): THomesteadStatus;
begin
  Vagrant('status', Output);
  if Pos(',state,running', Output) <> 0 then
    Result := HomesteadUp
  else if Pos(',state,saved', Output) <> 0 then
    Result := HomesteadSuspended
  else if Pos(',state,not_created', Output) <> 0 then
    Result := HomesteadDestroyed
  else
    Result := HomesteadHalted;
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

procedure THomestead.VagrantSsh(Arg: string; Console: TMemo);
var
  Args: TStrings;
  Output: string;
begin
  Args := TStringList.Create;
  Args.Add('ssh');
  Args.Add('--');

  Args.Add('source ./.profile && source ./.bash_aliases && ' + Arg);

  //Args.Add('bash');
  //Args.Add('-l');
  //Args.Add('-i');
  //Args.Add('-c');
  //Args.Add('''' + Arg + '''');

  Vagrant(Args, Output, Console);
end;

procedure THomestead.Vagrant(Arg: string; var Output: string; Console: TMemo);
var
  Args: TStrings;
begin
  Args := TStringList.Create;
  Args.Add(Arg);
  if Arg = 'status' then
    Args.Add('--machine-readable');
  if Arg = 'destroy' then
    Args.Add('--force');
  Vagrant(Args, Output, Console);
end;


procedure THomestead.Vagrant(Args: TStrings; var Output: string; Console: TMemo);
const
  BUF_SIZE = 2048;
var
  VagrantProcess: TVagrantProcess;
  OutputStream: TMemoryStream;
  BytesRead: longint;
  Buffer: array[1..BUF_SIZE] of byte;
  p: int64;
begin
  VagrantProcess := TVagrantProcess.Create(nil);
  VagrantProcess.Executable := Global.VagrantCmd;
  VagrantProcess.CurrentDirectory := Global.HomesteadDir;
  VagrantProcess.Parameters := Args;
  VagrantProcess.Options := VagrantProcess.Options + [poUsePipes, poStderrToOutPut];
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

begin
  AHomestead := THomestead.Create
end.
