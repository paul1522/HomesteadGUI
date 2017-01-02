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
    FHomesteadDir : String;
    FVagrantCmd : String;
    FTextEditorCmd : String;
    FHostsFileEditorCmd : String;
    FJSONData : TJSONData;
    FYamlIsJson : Boolean;
    YamlFileName : String;

    procedure SetHomesteadDir(Val : String);
    procedure SetVagrantCmd(Val : String);
    procedure SetTextEditorCmd(Val : String);
    procedure SetHostsFileEditorCmd(Val : String);
    procedure LoadIni;
    procedure LoadJson;
    procedure LoadEnabledFolders(Folders : TMemDataset);
    procedure LoadDisabledFolders(Folders : TMemDataset);
    procedure LoadEnabledSites(Sites : TMemDataset);
    procedure LoadDisabledSites(Sites : TMemDataset);
    procedure LoadPaths(Folders : TMemDataset; Path : TJSONStringType; e : Boolean);
    procedure SaveIni;
    procedure SaveJson(FolderData : TMemDataset; SiteData : TMemDataset);
    procedure SaveJsonFolders(FolderData : TMemDataset);
    procedure SaveJsonSites(SiteData : TMemDataset);
    procedure SaveJsonPaths(PathData : TMemDataset; EnabledPath, DisabledPath : String);
  public
    { public declarations }
    property HomesteadDir : String read FHomesteadDir write SetHomesteadDir;
    property VagrantCmd : String read FVagrantCmd write SetVagrantCmd;
    property TextEditorCmd : String read FTextEditorCmd write SetTextEditorCmd;
    property HostsFileEditorCmd : String read FHostsFileEditorCmd write SetHostsFileEditorCmd;
    property YamlIsJson : Boolean read FYamlIsJson;

    procedure Load;
    procedure LoadFolders(Folders : TMemDataset);
    procedure LoadSites(Sites : TMemDataset);
    procedure Save(FolderData : TMemDataset; SiteData : TMemDataset);
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
  YamlFileName := GetEnvironmentVariable('USERPROFILE') + '/.homestead/Homestead.yaml'
end;

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
begin
   LoadIni;
   LoadJson
end;

procedure TGlobal.Save(FolderData : TMemDataset; SiteData : TMemDataset);
begin
   SaveIni;
   SaveJson(FolderData, SiteData)
end;

procedure TGLobal.LoadIni;
var
  Ini : TIniFile;
begin
  try
    HomesteadDir := GetEnvironmentVariable('USERPROFILE') + '\Vagrants\Homestead'
  except
    FHomesteadDir := ''
  end;
  try
    VagrantCmd := 'C:\HashiCorp\Vagrant\bin\vagrant.exe'
  except
    FVagrantCmd := ''
  end;
  try
    TextEditorCmd := 'C:\Program Files (x86)\Notepad++\notepad++.exe'
  except
    FTextEditorCmd := ''
  end;
  try
    HostsFileEditorCmd := 'C:\Program Files (x86)\Hosts File Editor\HostsFileEditor.exe'
  except
    FHostsFileEditorCmd := ''
  end;
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

procedure TGlobal.LoadJson;
var
  Stream : TStream;
begin
  FYamlIsJson := TRUE;
  Stream := TFileStream.Create(YamlFileName, fmOpenRead);
  try
    try
      FJSONData := GetJSON(Stream)
    except
      FYamlIsJson := FALSE
    end
  finally
    Stream.Free
  end;
end;

procedure TGlobal.LoadFolders(Folders : TMemDataset);
begin
  LoadEnabledFolders(Folders);
  LoadDisabledFolders(Folders)
end;

procedure TGlobal.LoadSites(Sites : TMemDataset);
begin
  LoadEnabledSites(Sites);
  LoadDisabledSites(Sites)
end;

procedure TGlobal.LoadEnabledFolders(Folders : TMemDataset);
begin
  LoadPaths(Folders, 'folders', true)
end;

procedure TGlobal.LoadDisabledFolders(Folders : TMemDataset);
begin
  LoadPaths(Folders, 'disabled-folders', false)
end;

procedure TGlobal.LoadEnabledSites(Sites : TMemDataset);
begin
  LoadPaths(Sites, 'sites', true)
end;

procedure TGlobal.LoadDisabledSites(Sites : TMemDataset);
begin
  LoadPaths(Sites, 'disabled-sites', false)
end;

procedure TGlobal.LoadPaths(Folders : TMemDataset; Path : TJSONStringType; e : Boolean);
var
  JFolders : TJSONArray;
  JFolder : TJSONEnum;
  mp, tp : String;
begin
  JFolders := TJSONArray(FJSONData.FindPath(Path));
  if JFolders = nil then exit;
  for JFolder in JFolders do begin
     mp := JFolder.Value.FindPath('map').AsString;
     tp := JFolder.Value.FindPath('to').AsString;
     Folders.Append;
     Folders.FieldByName('Enabled').Value := e;
     Folders.FieldByName('Map').Value := mp;
     Folders.FieldByName('To').Value := tp;
     Folders.Post
  end
end;

procedure TGlobal.SaveIni;
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

procedure TGlobal.SaveJson(FolderData : TMemDataset; SiteData : TMemDataset);
var
  Stream : TStream;
  s : UTF8String;
begin
  if not FYamlIsJson then exit;
  SaveJsonFolders(FolderData);
  SaveJsonSites(SiteData);
  Stream := TFileStream.Create(YamlFileName, fmCreate);
  try
     s := FJSONData.FormatJSON([], 4);
     // Fix escaped forward slashes (/). Vagrant doesn't like them.
     S := StringReplace(s, '\/', '/', [rfReplaceAll]);
     Stream.Write(s[1], Length(s))
  finally
    Stream.Free
  end
end;

procedure TGlobal.SaveJsonPaths(PathData : TMemDataset; EnabledPath, DisabledPath : String);
var
  JEnabledFolders, JDisabledFolders : TJSONArray;
  JMap :TJSONObject;
begin

  JEnabledFolders := TJSONArray(FJSONData.FindPath(EnabledPath));
  if JEnabledFolders <> nil then JEnabledFolders.Clear
  else begin
    JEnabledFolders := TJSONArray.Create;
    TJSONObject(FJSONData).Add(EnabledPath, JEnabledFolders);
  end;

  JDisabledFolders := TJSONArray(FJSONData.FindPath(DisabledPath));
  if JDisabledFolders <> nil then JDisabledFolders.Clear
  else begin
    JDisabledFolders := TJSONArray.Create;
    TJSONObject(FJSONData).Add(DisabledPath, JDisabledFolders);
  end;

  PathData.First;
  while not PathData.EOF do begin
    JMap := TJSONObject.Create(
      [
        'map', PathData.FieldByName('Map').AsString,
        'to',  PathData.FieldByName('To').AsString
      ]
    );
    if PathData.FieldByName('Enabled').AsBoolean then JEnabledFolders.Add(JMap)
    else JDisabledFolders.Add(JMap);
    PathData.Next
  end;

  if JEnabledFolders.Count = 0 then TJSONObject(FJSONData).Delete(EnabledPath);
  if JDisabledFolders.Count = 0 then TJSONObject(FJSONData).Delete(DisabledPath);

end;

procedure TGlobal.SaveJsonFolders(FolderData : TMemDataset);
begin
  SaveJsonPaths(FolderData, 'folders', 'disabled-folders')
end;

procedure TGlobal.SaveJsonSites(SiteData : TMemDataset);
begin
  SaveJsonPaths(SiteData, 'sites', 'disabled-sites')
end;

end.

