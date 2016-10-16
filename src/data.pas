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
    procedure SetHomesteadDir(Val : String);
    procedure SetVagrantCmd(Val : String);
  public
    { public declarations }
    procedure Load;
    property HomesteadDir : String read FHomesteadDir write SetHomesteadDir;
    property VagrantCmd : String read FVagrantCmd write SetVagrantCmd;

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

procedure TGlobal.Load;
var
  Ini : TIniFile;
begin
  Ini := TIniFile.Create(INI_FILE_NAME);
  try
    HomesteadDir := Ini.ReadString('General', 'homestead_dir', '');
    VagrantCmd := Ini.ReadString('General', 'vagrant_cmd', '')
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
    Ini.WriteString('General', 'vagrant_cmd', VagrantCmd)
  finally
    Ini.Free
  end
end;

end.

