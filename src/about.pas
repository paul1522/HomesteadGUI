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
        Label4: TLabel;
        procedure Label4Click(Sender: TObject);
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

procedure TAboutDialog.Label4Click(Sender: TObject);
begin
    OpenURL(URL_HOMESTEADGUI);
end;

end.

