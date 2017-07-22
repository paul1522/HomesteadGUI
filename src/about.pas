unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  lclintf, Data;

type

  { TAboutDialog }

  TAboutDialog = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    VersionLabel: TLabel;
    Label3: TLabel;
    UrlLabel: TLabel;
    procedure FormShow(Sender: TObject);
    procedure UrlLabelClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutDialog: TAboutDialog;

implementation

const
  URL_HOMESTEADGUI = 'https://github.com/paulgeneres/HomesteadGUI';

{$R *.lfm}

{ TAboutDialog }

procedure TAboutDialog.UrlLabelClick(Sender: TObject);
begin
  OpenURL(URL_HOMESTEADGUI);
end;

procedure TAboutDialog.FormShow(Sender: TObject);
begin
  VersionLabel.Caption := stringreplace(VersionLabel.Caption, '$version',
    APP_VERSION, []);
end;

end.
