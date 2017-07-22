unit splash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Data;

type

  { TSplashScreen }

  TSplashScreen = class(TForm)
    Label1: TLabel;
    VersionLabel: TLabel;
    Label3: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SplashScreen: TSplashScreen;

implementation

{$R *.lfm}

{ TSplashScreen }

procedure TSplashScreen.FormShow(Sender: TObject);
begin
  VersionLabel.Caption := stringreplace(VersionLabel.Caption, '$version',
    APP_VERSION, []);
end;

end.
