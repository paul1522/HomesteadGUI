unit configure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, EditBtn, StdCtrls, ActnList, data, homestead;

type

  { TConfigDialog }

  TConfigDialog = class(TForm)
    EditHomesteadYaml: TAction;
    EditHosts: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    SaveButton: TButton;
    CancelButton: TButton;
    HomesteadDirSelector: TDirectoryEdit;
    VagrantCmdSelector: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure EditHomesteadYamlExecute(Sender: TObject);
    procedure EditHostsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    YamlFileName : String;
  public
    { public declarations }
  end;

const
  TextEditorCmd = 'C:\Program Files (x86)\Notepad++\notepad++.exe';
  HostsFileEditorCmd = 'C:\Program Files (x86)\Hosts File Editor\HostsFileEditor.exe';
var
  ConfigDialog: TConfigDialog;

implementation

{$R *.lfm}

{ TConfigDialog }

procedure TConfigDialog.FormShow(Sender: TObject);
begin
  HomesteadDirSelector.Text := Global.HomesteadDir;
  VagrantCmdSelector.Text := Global.VagrantCmd
end;

procedure TConfigDialog.SaveButtonClick(Sender: TObject);
begin
  Global.HomesteadDir := HomesteadDirSelector.Text;
  Global.VagrantCmd := VagrantCmdSelector.Text;
  Global.SaveIniFile
end;

procedure TConfigDialog.EditHomesteadYamlExecute(Sender: TObject);
begin
  AHomestead.DetatchProcess(TextEditorCmd, YamlFileName)
end;

procedure TConfigDialog.EditHostsExecute(Sender: TObject);
begin
  try
    AHomestead.DetatchProcess(HostsFileEditorCmd)
  except
    ShowMessage(HostsFileEditorCmd + ' failed to launch.')
  end
end;

procedure TConfigDialog.FormCreate(Sender: TObject);
begin
  YamlFileName := GetEnvironmentVariable('USERPROFILE') + '/.homestead/Homestead.yaml'
end;

end.

