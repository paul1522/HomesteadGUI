program HomesteadGUI;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  runtimetypeinfocontrols,
  memdslaz,
  admin,
  configure,
  homestead,
  Data,
  about,
  splash,
  yaris,
  yaris_options, vagrantprocess { you can add units after this };

{$R *.res}

begin
  Application.Title := 'Homestead GUI';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TAdminForm, AdminForm);
  Application.CreateForm(TConfigDialog, ConfigDialog);
  Application.CreateForm(TGlobal, Global);
  Application.CreateForm(TAboutDialog, AboutDialog);
  Application.CreateForm(TSplashScreen, SplashScreen);
  Application.CreateForm(TYarisDialog, YarisDialog);
  Application.Run;
end.
