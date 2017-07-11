unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  lclintf;

type

  { TAboutDialog }

  TAboutDialog = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    UrlLabel: TLabel;
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

end.
