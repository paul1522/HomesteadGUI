unit configure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memds, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, EditBtn, StdCtrls, ActnList, ComCtrls, DBGrids, Menus, DbCtrls,
  data, homestead;

type

  { TConfigDialog }

  TConfigDialog = class(TForm)
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
    procedure EditHomesteadYamlExecute(Sender: TObject);
    procedure EditHostsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FolderDataNewRecord(DataSet: TDataSet);
    procedure SaveButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    YamlFileName : String;
    PathsLoaded : Boolean;
    procedure LoadFolders;
    procedure LoadSites;
    procedure SaveFolders;
    procedure SaveSites;
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
  HostsFileEditorCmdSelector.Text := Global.HostsFileEditorCmd;
  TabSheet2.TabVisible := Global.YamlIsJson;
  TabSheet3.TabVisible := Global.YamlIsJson;
  if Global.YamlIsJson and not PathsLoaded then begin
    LoadFolders;
    LoadSites;
    PathsLoaded := TRUE
  end
end;

procedure TConfigDialog.SaveButtonClick(Sender: TObject);
begin
  Global.HomesteadDir := HomesteadDirSelector.Text;
  Global.VagrantCmd := VagrantCmdSelector.Text;
  Global.TextEditorCmd := TextEditorCmdSelector.Text;
  Global.HostsFileEditorCmd := HostsFileEditorCmdSelector.Text;
  Global.Save(FolderData, SiteData)
end;

procedure TConfigDialog.EditHomesteadYamlExecute(Sender: TObject);
begin
  try
    AHomestead.DetatchProcess(Global.TextEditorCmd, YamlFileName)
  except
    ShowMessage(Global.TextEditorCmd + ' failed to launch.')
  end
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
  YamlFileName := GetEnvironmentVariable('USERPROFILE') + '/.homestead/Homestead.yaml';
  PageControl1.ActivePageIndex := 0;
  PathsLoaded := FALSE
end;

procedure TConfigDialog.FolderDataNewRecord(DataSet: TDataSet);
begin
  DataSet.FieldByName('Enabled').Value := true
end;

procedure TConfigDialog.LoadFolders;
begin
  Global.LoadFolders(FolderData)
end;

procedure TConfigDialog.LoadSites;
begin
  Global.LoadSites(SiteData)
end;

procedure TConfigDialog.SaveFolders;
begin

end;

procedure TConfigDialog.SaveSites;
begin

end;

end.

