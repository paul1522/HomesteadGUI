unit configure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memds, DB, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, EditBtn, StdCtrls, ActnList, ComCtrls, DBGrids, Menus, DBCtrls,
  data, homestead, lclintf;

type

  { TConfigDialog }

  TConfigDialog = class(TForm)
    Button4: TButton;
    EditAfterSh: TAction;
    EditAliases: TAction;
    Button3: TButton;
    SiteSource: TDataSource;
    SiteNavigator: TDBNavigator;
    SiteGrid: TDBGrid;
    SiteData: TMemDataset;
    FolderSource: TDataSource;
    FolderNavigator: TDBNavigator;
    FolderGrid: TDBGrid;
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
    FolderData: TMemDataset;
    PageControl1: TPageControl;
    SaveButton: TButton;
    CancelButton: TButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TextEditorCmdSelector: TFileNameEdit;
    VagrantCmdSelector: TFileNameEdit;
    procedure EditAfterShExecute(Sender: TObject);
    procedure EditAliasesExecute(Sender: TObject);
    procedure EditHomesteadYamlExecute(Sender: TObject);
    procedure EditHostsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FolderDataNewRecord(DataSet: TDataSet);
    procedure HomesteadDirSelectorExit(Sender: TObject);
    procedure HostsFileEditorCmdSelectorExit(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TextEditorCmdSelectorExit(Sender: TObject);
    procedure VagrantCmdSelectorExit(Sender: TObject);
  private
    { private declarations }
    AfterShFileName, AliasesFileName: string;
    PathsLoaded: boolean;
    procedure LoadFolders;
    procedure LoadSites;
    procedure ValidateHomesteadDir;
    procedure ValidateVagrantCmd;
    procedure ValidateTextEditorCmd;
    procedure ValidateHostsFileEditorCmd;
  public
    { public declarations }
  end;

var
  ConfigDialog: TConfigDialog;

implementation

const
URL_HOMESTEAD = 'https://github.com/laravel/homestead';
URL_VAGRANT = 'https://vagrantup.com';
URL_NOTEPAD = 'https://notepad-plus-plus.org';
URL_HOSTS_FILE_EDITOR = 'https://scottlerch.github.io/HostsFileEditor/';

{$R *.lfm}

{ TConfigDialog }

procedure TConfigDialog.FormShow(Sender: TObject);
begin
  if Global.ConfigFileName = '' then begin
    ShowMessage('Cannot find the Homestead configuration file in any of the usual places.');
    Application.Terminate;
  end;
  HomesteadDirSelector.Text := Global.HomesteadDir;
  VagrantCmdSelector.Text := Global.VagrantCmd;
  TextEditorCmdSelector.Text := Global.TextEditorCmd;
  HostsFileEditorCmdSelector.Text := Global.HostsFileEditorCmd;
  TabSheet2.TabVisible := Global.ConfigIsJson;
  TabSheet3.TabVisible := Global.ConfigIsJson;
  EditHomesteadYaml.Caption := 'Edit Homestead' + ExtractFileExt(Global.ConfigFileName);
  if Global.ConfigIsJson and not PathsLoaded then
  begin
    LoadFolders;
    LoadSites;
    PathsLoaded := True;
  end;
end;

procedure TConfigDialog.SaveButtonClick(Sender: TObject);
begin
  try
    Global.HomesteadDir := HomesteadDirSelector.Text;
    Global.VagrantCmd := VagrantCmdSelector.Text;
    Global.TextEditorCmd := TextEditorCmdSelector.Text;
    Global.HostsFileEditorCmd := HostsFileEditorCmdSelector.Text;
    Global.Save(FolderData, SiteData)
  except
    ModalResult := mrNone
  end;
end;

procedure TConfigDialog.EditHomesteadYamlExecute(Sender: TObject);
begin
  try
    AHomestead.DetatchProcess(Global.TextEditorCmd, Global.ConfigFileName)
  except
    ShowMessage(Global.TextEditorCmd + ' failed to launch.')
  end;
end;

procedure TConfigDialog.EditAfterShExecute(Sender: TObject);
begin
  try
    AHomestead.DetatchProcess(Global.TextEditorCmd, AfterShFileName)
  except
    ShowMessage(Global.TextEditorCmd + ' failed to launch.')
  end;
end;

procedure TConfigDialog.EditAliasesExecute(Sender: TObject);
begin
  try
    AHomestead.DetatchProcess(Global.TextEditorCmd, AliasesFileName)
  except
    ShowMessage(Global.TextEditorCmd + ' failed to launch.')
  end;
end;

procedure TConfigDialog.EditHostsExecute(Sender: TObject);
begin
  try
    AHomestead.DetatchProcess(Global.HostsFileEditorCmd)
  except
    ShowMessage(Global.HostsFileEditorCmd + ' failed to launch.')
  end;
end;

procedure TConfigDialog.FormCreate(Sender: TObject);
begin
  AfterShFileName := GetEnvironmentVariable('USERPROFILE') + '/.homestead/after.sh';
  AliasesFileName := GetEnvironmentVariable('USERPROFILE') + '/.homestead/aliases';
  PageControl1.ActivePageIndex := 0;
  PathsLoaded := False;
  FolderGrid.Columns[0].SizePriority := 0;
  FolderGrid.Columns[0].Width := 50;
  SiteGrid.Columns[0].SizePriority := 0;
  SiteGrid.Columns[0].Width := 50;
  Label1.Hint := URL_HOMESTEAD;
  Label2.Hint := URL_VAGRANT;
  Label3.Hint := URL_NOTEPAD;
  Label4.Hint := URL_HOSTS_FILE_EDITOR;

end;

procedure TConfigDialog.FolderDataNewRecord(DataSet: TDataSet);
begin
  DataSet.FieldByName('Enabled').Value := True;
end;

procedure TConfigDialog.Label1Click(Sender: TObject);
begin
  OpenURL(URL_HOMESTEAD);
end;

procedure TConfigDialog.Label2Click(Sender: TObject);
begin
  OpenURL(URL_VAGRANT);
end;

procedure TConfigDialog.Label3Click(Sender: TObject);
begin
  OpenURL(URL_NOTEPAD);
end;

procedure TConfigDialog.Label4Click(Sender: TObject);
begin
  OpenURL(URL_HOSTS_FILE_EDITOR);
end;

procedure TConfigDialog.LoadFolders;
begin
  Global.LoadFolders(FolderData);
end;

procedure TConfigDialog.LoadSites;
begin
  Global.LoadSites(SiteData);
end;

procedure TConfigDialog.HomesteadDirSelectorExit(Sender: TObject);
begin
  ValidateHomesteadDir
end;

procedure TConfigDialog.VagrantCmdSelectorExit(Sender: TObject);
begin
  ValidateVagrantCmd
end;

procedure TConfigDialog.TextEditorCmdSelectorExit(Sender: TObject);
begin
  ValidateTextEditorCmd
end;

procedure TConfigDialog.HostsFileEditorCmdSelectorExit(Sender: TObject);
begin
  ValidateHostsFileEditorCmd
end;

procedure TConfigDialog.ValidateHomesteadDir;
begin
  HomesteadDirSelector.Color := clDefault;
  try
    Global.HomesteadDir := HomesteadDirSelector.Text
  except
    HomesteadDirSelector.Color := clRed;
  end;
end;

procedure TConfigDialog.ValidateVagrantCmd;
begin
  VagrantCmdSelector.Color := clDefault;
  try
    Global.VagrantCmd := VagrantCmdSelector.Text
  except
    VagrantCmdSelector.Color := clRed;
  end;
end;

procedure TConfigDialog.ValidateTextEditorCmd;
begin
  TextEditorCmdSelector.Color := clDefault;
  try
    Global.TextEditorCmd := TextEditorCmdSelector.Text
  except
    TextEditorCmdSelector.Color := clRed;
  end;
end;

procedure TConfigDialog.ValidateHostsFileEditorCmd;
begin
  HostsFileEditorCmdSelector.Color := clDefault;
  try
    Global.HostsFileEditorCmd := HostsFileEditorCmdSelector.Text
  except
    HostsFileEditorCmdSelector.Color := clRed;
  end;
end;

end.
