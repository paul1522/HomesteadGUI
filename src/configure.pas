unit configure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, EditBtn, StdCtrls, ActnList, ComCtrls, data, homestead;

type

  { TConfigDialog }

  TConfigDialog = class(TForm)
    EditHomesteadYaml: TAction;
    EditHosts: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    HomesteadDirSelector: TDirectoryEdit;
    HostsFileEditorCmdSelector: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PageControl1: TPageControl;
    SaveButton: TButton;
    CancelButton: TButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TextEditorCmdSelector: TFileNameEdit;
    VagrantCmdSelector: TFileNameEdit;
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

var
  ConfigDialog: TConfigDialog;

implementation

{$R *.lfm}

{ TConfigDialog }

procedure TConfigDialog.FormShow(Sender: TObject);
begin
  HomesteadDirSelector.Text := Global.HomesteadDir;
  VagrantCmdSelector.Text := Global.VagrantCmd;
  TextEditorCmdSelector.Text := Global.TextEditorCmd;
  HostsFileEditorCmdSelector.Text := Global.HostsFileEditorCmd
end;

procedure TConfigDialog.SaveButtonClick(Sender: TObject);
begin
  Global.HomesteadDir := HomesteadDirSelector.Text;
  Global.VagrantCmd := VagrantCmdSelector.Text;
  Global.TextEditorCmd := TextEditorCmdSelector.Text;
  Global.HostsFileEditorCmd := HostsFileEditorCmdSelector.Text;
  Global.SaveIniFile
end;

procedure TConfigDialog.EditHomesteadYamlExecute(Sender: TObject);
begin
  AHomestead.DetatchProcess(Global.TextEditorCmd, YamlFileName)
end;

procedure TConfigDialog.EditHostsExecute(Sender: TObject);
begin
  try
    AHomestead.DetatchProcess(Global.HostsFileEditorCmd)
  except
    ShowMessage(Global.HostsFileEditorCmd + ' failed to launch.')
  end
end;

procedure TConfigDialog.FormCreate(Sender: TObject);
begin
  YamlFileName := GetEnvironmentVariable('USERPROFILE') + '/.homestead/Homestead.yaml'
end;

end.

