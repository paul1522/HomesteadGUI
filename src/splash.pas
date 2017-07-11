unit splash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TSplashScreen }

  TSplashScreen = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SplashScreen: TSplashScreen;

implementation

{$R *.lfm}

end.
