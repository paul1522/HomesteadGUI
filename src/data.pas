unit data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IniFiles, fpjson, jsonparser, memds;

type

  { TGlobal }

  TGlobal = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
    FHomesteadDir: string;
    FVagrantCmd: string;
    FTextEditorCmd: string;
    FHostsFileEditorCmd: string;
    FJSONData: TJSONData;
    FYamlIsJson: boolean;
    FConfigFileName, IpAddr: string;
    FConfigDir : string;

    procedure SetHomesteadDir(Val: string);
    procedure SetVagrantCmd(Val: string);
    procedure SetTextEditorCmd(Val: string);
    procedure SetHostsFileEditorCmd(Val: string);
    procedure LoadJson;
    procedure LoadEnabledFolders(Folders: TMemDataset);
    procedure LoadDisabledFolders(Folders: TMemDataset);
    procedure LoadEnabledSites(Sites: TMemDataset);
    procedure LoadDisabledSites(Sites: TMemDataset);
    procedure LoadEnabledDatabases(Databases: TMemDataset);
    procedure LoadDisabledDatabases(Databases: TMemDataset);
    procedure LoadPaths(Folders: TMemDataset; Path: TJSONStringType; e: boolean);
    procedure LoadStrings(Dataset: TMemDataset; Path: TJSONStringType; e: boolean);
    procedure SaveIni;
    procedure SaveJson(FolderData: TMemDataset; SiteData: TMemDataset; DatabaseData: TMemDataset);
    procedure SaveJsonFolders(FolderData: TMemDataset);
    procedure SaveJsonSites(SiteData: TMemDataset);
    procedure SaveJsonDatabases(DatabaseData: TMemDataset);
    procedure SaveJsonPaths(PathData: TMemDataset; EnabledPath, DisabledPath: string);
    procedure SaveJsonStrings(StringData: TMemDataset; EnabledPath, DisabledPath: string);
    procedure UpdateHostsFile(SiteData: TMemDataset; Addr: string);
  public
    { public declarations }
    property HomesteadDir: string read FHomesteadDir write SetHomesteadDir;
    property VagrantCmd: string read FVagrantCmd write SetVagrantCmd;
    property TextEditorCmd: string read FTextEditorCmd write SetTextEditorCmd;
    property HostsFileEditorCmd: string read FHostsFileEditorCmd
      write SetHostsFileEditorCmd;
    property ConfigIsJson: boolean read FYamlIsJson;
    property ConfigFileName: string read FConfigFileName;
    property ConfigDir : string read FConfigDir;

    procedure LoadIni;
    function LoadValidConfig: boolean;
    procedure LoadFolders(Folders: TMemDataset);
    procedure LoadSites(Sites: TMemDataset);
    procedure LoadDatabases(Databases: TMemDataset);
    procedure Save(FolderData: TMemDataset; SiteData: TMemDataset; DatabaseData: TMemDataset);
  end;

const
  INI_FILE_NAME = 'HomesteadGUI.ini';

var
  Global: TGlobal;

implementation

{$R *.lfm}

{ TGlobal }

procedure TGlobal.DataModuleCreate(Sender: TObject);
begin
  LoadIni;
end;

procedure TGlobal.SetHomesteadDir(Val: string);
begin
  if not FileExists(Val + '/Vagrantfile') then
    raise Exception.Create('Invalid HomesteadDir');
  FHomesteadDir := Val;
end;

procedure TGlobal.SetVagrantCmd(Val: string);
begin
  if not FileExists(Val) then
    raise Exception.Create('Invalid VagrantCmd');
  FVagrantCmd := Val;
end;

procedure TGlobal.SetTextEditorCmd(Val: string);
begin
  if not FileExists(Val) then
    raise Exception.Create('Invalid TextEditorCmd');
  FTextEditorCmd := Val;
end;

procedure TGlobal.SetHostsFileEditorCmd(Val: string);
begin
  if not FileExists(Val) then
    raise Exception.Create('Invalid HostsFileEditorCmd');
  FHostsFileEditorCmd := Val;
end;

function TGlobal.LoadValidConfig: boolean;
var
  Valid: boolean;
begin
  Valid := True;

  try
    HomesteadDir := FHomesteadDir
  except
    FHomesteadDir := GetEnvironmentVariable('USERPROFILE') + '\Vagrants\Homestead'
  end;

  try
    VagrantCmd := FVagrantCmd
  except
    FVagrantCmd := 'C:\HashiCorp\Vagrant\bin\vagrant.exe'
  end;

  try
    TextEditorCmd := FTextEditorCmd
  except
    FTextEditorCmd := 'C:\Program Files (x86)\Notepad++\notepad++.exe'
  end;

  try
    HostsFileEditorCmd := FHostsFileEditorCmd
  except
    FHostsFileEditorCmd := 'C:\Program Files (x86)\Hosts File Editor\HostsFileEditor.exe'
  end;

  try
    HomesteadDir := FHomesteadDir
  except
    Valid := False
  end;

  try
    VagrantCmd := FVagrantCmd
  except
    Valid := False
  end;

  try
    TextEditorCmd := FTextEditorCmd
  except
    Valid := False
  end;

  try
    HostsFileEditorCmd := FHostsFileEditorCmd
  except
    Valid := False
  end;

  if ConfigFileName = '' then Valid := False;

  LoadJson;

  LoadValidConfig := Valid;
end;

procedure TGlobal.Save(FolderData: TMemDataset; SiteData: TMemDataset; DatabaseData: TMemDataset);
begin
  SaveIni;
  SaveJson(FolderData, SiteData, DatabaseData);
  UpdateHostsFile(SiteData, IpAddr);
  LoadIni;
end;

procedure TGLobal.LoadIni;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(INI_FILE_NAME);
  try
    FHomesteadDir := Ini.ReadString('General', 'homestead_dir', FHomesteadDir);
    FVagrantCmd := Ini.ReadString('General', 'vagrant_cmd', FVagrantCmd);
    FTextEditorCmd := Ini.ReadString('General', 'text_editor_cmd', FTextEditorCmd);
    FHostsFileEditorCmd := Ini.ReadString('General', 'hosts_file_editor_cmd',
      FHostsFileEditorCmd)
  finally
    Ini.Free
  end;

  FConfigDir := GetEnvironmentVariable('USERPROFILE') + '\.homestead';
  FConfigFileName := FConfigDir + '\Homestead.yaml';
  if FileExists(FConfigFileName) then exit;
  FConfigFileName := FConfigDir + '\Homestead.json';
  if FileExists(FConfigFileName) then exit;

  FConfigDir := FHomesteadDir;
  FConfigFileName := FConfigDir + '\Homestead.yaml';
  if FileExists(FConfigFileName) then exit;
  FConfigFileName := FConfigDir + '\Homestead.json';
  if FileExists(FConfigFileName) then exit;

  FConfigFileName := '';
end;

procedure TGlobal.LoadJson;
var
  Stream: TStream;
begin
  FYamlIsJson := False;
  if not FileExists(FConfigFileName) then exit;
  FYamlIsJson := True;
  Stream := TFileStream.Create(FConfigFileName, fmOpenRead);
  try
    try
      FJSONData := GetJSON(Stream)
    except
      FYamlIsJson := False
    end
  finally
    Stream.Free
  end;
  IpAddr := FJSONData.FindPath('ip').AsString;
end;

procedure TGlobal.LoadFolders(Folders: TMemDataset);
begin
  LoadEnabledFolders(Folders);
  LoadDisabledFolders(Folders);
end;

procedure TGlobal.LoadSites(Sites: TMemDataset);
begin
  LoadEnabledSites(Sites);
  LoadDisabledSites(Sites);
end;

procedure TGlobal.LoadDatabases(Databases: TMemDataset);
begin
  LoadEnabledDatabases(Databases);
  LoadDisabledDatabases(Databases);
end;

procedure TGlobal.LoadEnabledFolders(Folders: TMemDataset);
begin
  LoadPaths(Folders, 'folders', True);
end;

procedure TGlobal.LoadDisabledFolders(Folders: TMemDataset);
begin
  LoadPaths(Folders, 'disabled-folders', False);
end;

procedure TGlobal.LoadEnabledSites(Sites: TMemDataset);
begin
  LoadPaths(Sites, 'sites', True);
end;

procedure TGlobal.LoadDisabledSites(Sites: TMemDataset);
begin
  LoadPaths(Sites, 'disabled-sites', False);
end;

procedure TGlobal.LoadEnabledDatabases(Databases: TMemDataset);
begin
  LoadStrings(Databases, 'databases', True);
end;

procedure TGlobal.LoadDisabledDatabases(Databases: TMemDataset);
begin
  LoadStrings(Databases, 'disabled-databases', False);
end;

procedure TGlobal.LoadPaths(Folders: TMemDataset; Path: TJSONStringType; e: boolean);
var
  JFolders: TJSONArray;
  JFolder: TJSONEnum;
  mp, tp: string;
begin
  //Folders.Clear(FALSE);
  //Folders.Open;
  JFolders := TJSONArray(FJSONData.FindPath(Path));
  if JFolders = nil then
    exit;
  for JFolder in JFolders do
  begin
    mp := JFolder.Value.FindPath('map').AsString;
    tp := JFolder.Value.FindPath('to').AsString;
    Folders.Append;
    Folders.FieldByName('Enabled').Value := e;
    Folders.FieldByName('Map').Value := mp;
    Folders.FieldByName('To').Value := tp;
    Folders.Post;
  end;
end;

procedure TGlobal.LoadStrings(Dataset: TMemDataset; Path: TJSONStringType; e: boolean);
var
  JFolders: TJSONArray;
  JFolder: TJSONEnum;
  s: string;
begin
  JFolders := TJSONArray(FJSONData.FindPath(Path));
  if JFolders = nil then
    exit;
  for JFolder in JFolders do
  begin
    s := JFolder.Value.AsString;
    Dataset.Append;
    Dataset.FieldByName('Enabled').Value := e;
    Dataset.FieldByName('Database').Value := s;
    Dataset.Post;
  end;
end;

procedure TGlobal.SaveIni;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(INI_FILE_NAME);
  try
    Ini.WriteString('General', 'homestead_dir', HomesteadDir);
    Ini.WriteString('General', 'vagrant_cmd', VagrantCmd);
    Ini.WriteString('General', 'text_editor_cmd', TextEditorCmd);
    Ini.WriteString('General', 'hosts_file_editor_cmd', HostsFileEditorCmd)
  finally
    Ini.Free
  end;
end;

procedure TGlobal.SaveJson(FolderData: TMemDataset; SiteData: TMemDataset; DatabaseData: TMemDataset);
var
  Stream: TStream;
  s: UTF8String;
begin
  if not FYamlIsJson then
    exit;
  LoadJson;
  SaveJsonFolders(FolderData);
  SaveJsonSites(SiteData);
  SaveJsonDatabases(DatabaseData);
  Stream := TFileStream.Create(FConfigFileName, fmCreate);
  try
    s := FJSONData.FormatJSON([], 4);
    // Fix escaped forward slashes (/). Vagrant doesn't like them.
    S := StringReplace(s, '\/', '/', [rfReplaceAll]);
    Stream.Write(s[1], Length(s))
  finally
    Stream.Free
  end;
end;

procedure TGlobal.SaveJsonPaths(PathData: TMemDataset;
  EnabledPath, DisabledPath: string);
var
  JEnabledFolders, JDisabledFolders: TJSONArray;
  JMap: TJSONObject;
begin
  JEnabledFolders := TJSONArray(FJSONData.FindPath(EnabledPath));
  if JEnabledFolders <> nil then
    JEnabledFolders.Clear
  else
  begin
    JEnabledFolders := TJSONArray.Create;
    TJSONObject(FJSONData).Add(EnabledPath, JEnabledFolders);
  end;

  JDisabledFolders := TJSONArray(FJSONData.FindPath(DisabledPath));
  if JDisabledFolders <> nil then
    JDisabledFolders.Clear
  else
  begin
    JDisabledFolders := TJSONArray.Create;
    TJSONObject(FJSONData).Add(DisabledPath, JDisabledFolders);
  end;

  PathData.First;
  while not PathData.EOF do
  begin
    JMap := TJSONObject.Create(
      ['map', PathData.FieldByName('Map').AsString, 'to',
      PathData.FieldByName('To').AsString]);
    if PathData.FieldByName('Enabled').AsBoolean then
      JEnabledFolders.Add(JMap)
    else
      JDisabledFolders.Add(JMap);
    PathData.Next;
  end;

  if JEnabledFolders.Count = 0 then
    TJSONObject(FJSONData).Delete(EnabledPath);
  if JDisabledFolders.Count = 0 then
    TJSONObject(FJSONData).Delete(DisabledPath);

end;

procedure TGlobal.SaveJsonStrings(StringData: TMemDataset;
  EnabledPath, DisabledPath: string);
var
  JEnabledFolders, JDisabledFolders: TJSONArray;
  JMap: TJSONString;
begin
  JEnabledFolders := TJSONArray(FJSONData.FindPath(EnabledPath));
  if JEnabledFolders <> nil then
    JEnabledFolders.Clear
  else
  begin
    JEnabledFolders := TJSONArray.Create;
    TJSONObject(FJSONData).Add(EnabledPath, JEnabledFolders);
  end;

  JDisabledFolders := TJSONArray(FJSONData.FindPath(DisabledPath));
  if JDisabledFolders <> nil then
    JDisabledFolders.Clear
  else
  begin
    JDisabledFolders := TJSONArray.Create;
    TJSONObject(FJSONData).Add(DisabledPath, JDisabledFolders);
  end;

  StringData.First;
  while not StringData.EOF do
  begin
    JMap := TJSONString.Create(StringData.FieldByName('Database').AsString);
    if StringData.FieldByName('Enabled').AsBoolean then
      JEnabledFolders.Add(JMap)
    else
      JDisabledFolders.Add(JMap);
    StringData.Next;
  end;

  if JEnabledFolders.Count = 0 then
    TJSONObject(FJSONData).Delete(EnabledPath);
  if JDisabledFolders.Count = 0 then
    TJSONObject(FJSONData).Delete(DisabledPath);

end;

procedure TGlobal.SaveJsonFolders(FolderData: TMemDataset);
begin
  SaveJsonPaths(FolderData, 'folders', 'disabled-folders');
end;

procedure TGlobal.SaveJsonSites(SiteData: TMemDataset);
begin
  SaveJsonPaths(SiteData, 'sites', 'disabled-sites');
end;

procedure TGlobal.SaveJsonDatabases(DatabaseData: TMemDataset);
begin
  SaveJsonStrings(DatabaseData, 'databases', 'disabled-databases');
end;

procedure TGlobal.UpdateHostsFile(SiteData: TMemDataset; Addr: string);
var
  HostName: string;
begin
  SiteData.First;
  while not SiteData.EOF do
  begin
    HostName := SiteData.FieldByName('Map').AsString;
    // if SiteData.FieldByName('Enabled').AsBoolean then
    // uncomment or add host name to hosts file
    // else
    // comment host name if exists in hosts file
    SiteData.Next;
  end;
end;

end.
