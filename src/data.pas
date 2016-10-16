unit data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IniFiles;

type

  { TGlobal }

  TGlobal = class(TDataModule)
  private
    { private declarations }
    FHomesteadDir : String;
    FVagrantCmd : String;
    FTextEditorCmd : String;
    FHostsFileEditorCmd : String;
    procedure SetHomesteadDir(Val : String);
    procedure SetVagrantCmd(Val : String);
    procedure SetTextEditorCmd(Val : String);
    procedure SetHostsFileEditorCmd(Val : String);
  public
    { public declarations }
    procedure Load;
    property HomesteadDir : String read FHomesteadDir write SetHomesteadDir;
    property VagrantCmd : String read FVagrantCmd write SetVagrantCmd;
    property TextEditorCmd : String read FTextEditorCmd write SetTextEditorCmd;
    property HostsFileEditorCmd : String read FHostsFileEditorCmd write SetHostsFileEditorCmd;

    procedure SaveIniFile;
  end;

const
  INI_FILE_NAME = 'HomesteadGUI.ini';

var
  Global: TGlobal;

implementation

{$R *.lfm}

{ TGlobal }

procedure TGlobal.SetHomesteadDir(Val : String);
begin
   if not FileExists(Val + '/Vagrantfile') then
     raise Exception.Create('Invalid HomesteadDir');
   FHomesteadDir := Val
end;

procedure TGlobal.SetVagrantCmd(Val : String);
begin
   if not FileExists(Val) then
     raise Exception.Create('Invalid VagrantCmd');
   FVagrantCmd := Val
end;

procedure TGlobal.SetTextEditorCmd(Val : String);
begin
   if not FileExists(Val) then
     raise Exception.Create('Invalid TextEditorCmd');
   FTextEditorCmd := Val
end;

procedure TGlobal.SetHostsFileEditorCmd(Val : String);
begin
   if not FileExists(Val) then
     raise Exception.Create('Invalid HostsFileEditorCmd');
   FHostsFileEditorCmd := Val
end;

procedure TGlobal.Load;
var
  Ini : TIniFile;
begin
  FHomesteadDir := GetEnvironmentVariable('USERPROFILE') + '\Vagrants\Homestead';
  FVagrantCmd := 'C:\HashiCorp\Vagrant\bin\vagrant.exe';
  FTextEditorCmd := 'C:\Program Files (x86)\Notepad++\notepad++.exe';
  FHostsFileEditorCmd := 'C:\Program Files (x86)\Hosts File Editor\HostsFileEditor.exe';

  Ini := TIniFile.Create(INI_FILE_NAME);
  try
    HomesteadDir := Ini.ReadString('General', 'homestead_dir', FHomesteadDir);
    VagrantCmd := Ini.ReadString('General', 'vagrant_cmd', FVagrantCmd);
    TextEditorCmd := Ini.ReadString('General', 'text_editor_cmd', FTextEditorCmd);
    HostsFileEditorCmd := Ini.ReadString('General', 'hosts_file_editor_cmd', FHostsFileEditorCmd)
  finally
    Ini.Free
  end
end;

procedure TGlobal.SaveIniFile;
var
  Ini : TIniFile;
begin
  Ini := TIniFile.Create(INI_FILE_NAME);
  try
    Ini.WriteString('General', 'homestead_dir', HomesteadDir);
    Ini.WriteString('General', 'vagrant_cmd', VagrantCmd);
    Ini.WriteString('General', 'text_editor_cmd', TextEditorCmd);
    Ini.WriteString('General', 'hosts_file_editor_cmd', HostsFileEditorCmd)
  finally
    Ini.Free
  end
end;

end.

