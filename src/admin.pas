unit admin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, ActnList, ComCtrls, configure, homestead, ComObj,
  LazFileUtils, data;

type

  TVagrantAction = (
    vaUp,
    vaDestroy,
    vaHalt,
    vaSuspend,
    vaResume,
    vaProvision,
    vaReload
  );

  { TAdminForm }

  TAdminForm = class(TForm)
    CmdAction: TAction;
    ConfigureAction: TAction;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    UpAction: TAction;
    HaltAction: TAction;
    SuspendAction: TAction;
    ResumeAction: TAction;
    ProvisionAction: TAction;
    ReloadAction: TAction;
    DestroyAction: TAction;
    SshAction: TAction;
    ActionList1: TActionList;
    OutputMemo: TMemo;
    ToolBar1: TToolBar;
    StatusLabel: TLabel;
    procedure CmdActionExecute(Sender: TObject);
    procedure ConfigureActionExecute(Sender: TObject);
    procedure DestroyActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HaltActionExecute(Sender: TObject);
    procedure ProvisionActionExecute(Sender: TObject);
    procedure ReloadActionExecute(Sender: TObject);
    procedure ResumeActionExecute(Sender: TObject);
    procedure SshActionExecute(Sender: TObject);
    procedure SuspendActionExecute(Sender: TObject);
    procedure UpActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure RefreshStatus;
    procedure StateIsUp;
    procedure StateIsHalted;
    procedure StateIsSuspended;
    procedure VagrantActionExecute(VagrantAction : TVagrantAction);
  public
    { public declarations }
  end;

var
  AdminForm: TAdminForm;

implementation

{$R *.lfm}

{ TAdminForm }

procedure TAdminForm.ConfigureActionExecute(Sender: TObject);
begin
  ConfigDialog.ShowModal
end;

procedure TAdminForm.CmdActionExecute(Sender: TObject);
begin
  AHomestead.cmd
end;

procedure TAdminForm.VagrantActionExecute(VagrantAction : TVagrantAction);
var
  Output : String;
begin
  Toolbar1.Enabled := false;
  Cursor := crHourGlass;
  StatusLabel.Caption := 'Vagrant is running. Please wait...';
  Application.ProcessMessages;
  case VagrantAction of
    vaUp : AHomestead.Up(Output, OutputMemo.Lines);
    vaDestroy : AHomestead.DestroyBox(Output, OutputMemo.Lines);
    vaHalt : AHomestead.Halt(Output, OutputMemo.Lines);
    vaSuspend : AHomestead.Suspend(Output, OutputMemo.Lines);
    vaResume : AHomestead.Resume(Output, OutputMemo.Lines);
    vaProvision : AHomestead.Provision(Output, OutputMemo.Lines);
    vaReload : AHomestead.Reload(Output, OutputMemo.Lines)
  end;
  Cursor := crDefault;
  RefreshStatus;
  Toolbar1.Enabled := true
end;

procedure TAdminForm.DestroyActionExecute(Sender: TObject);
begin
  if MessageDlg(
    'CONFIRM',
    'DO YOU REALLY WANT TO DESTROY THE HOMESTEAD INSTANCE',
    mtConfirmation,
    [mbYes, mbNo],
    0
  ) = mrYes then
  begin
    VagrantActionExecute(vaDestroy)
  end
end;

procedure TAdminForm.FormCreate(Sender: TObject);
begin
  OutputMemo.Text := '';
end;

procedure TAdminForm.SshActionExecute(Sender: TObject);
begin
  AHomestead.ssh
end;

procedure TAdminForm.UpActionExecute(Sender: TObject);
begin
  VagrantActionExecute(vaUp)
end;

procedure TAdminForm.HaltActionExecute(Sender: TObject);
begin
  VagrantActionExecute(vaHalt)
end;

procedure TAdminForm.SuspendActionExecute(Sender: TObject);
begin
  VagrantActionExecute(vaSuspend)
end;

procedure TAdminForm.ResumeActionExecute(Sender: TObject);
begin
  VagrantActionExecute(vaResume)
end;

procedure TAdminForm.ProvisionActionExecute(Sender: TObject);
begin
  VagrantActionExecute(vaProvision)
end;

procedure TAdminForm.ReloadActionExecute(Sender: TObject);
begin
  VagrantActionExecute(vaReload)
end;

procedure TAdminForm.FormShow(Sender: TObject);
begin
  try
    Global.Load
  except
    if ConfigDialog.ShowModal <> mrOK then
       Application.Terminate;
  end;
  RefreshStatus
end;

procedure TAdminForm.RefreshStatus;
var
  Output : String;
begin
  Cursor := crHourglass;
  StatusLabel.Caption := 'Checking Homestead status. Please wait...';
  Application.ProcessMessages;
  case AHomestead.Status(Output) of
    HomesteadUp : StateIsUp;
    HomesteadHalted : StateIsHalted;
    HomesteadSuspended : StateIsSuspended;
    HomesteadDestroyed : StateIsHalted
  end;
  Cursor := crDefault
end;

procedure TAdminForm.StateIsUp;
begin
  StatusLabel.Caption:= 'Homestead is running.';
  UpAction.Enabled := false;
  HaltAction.Enabled := true;
  SuspendAction.Enabled := true;
  ResumeAction.Enabled := false;
  ProvisionAction.Enabled := true;
  ReloadAction.Enabled := true;
  SshAction.Enabled := true
end;

procedure TAdminForm.StateIsHalted;
begin
  StatusLabel.Caption:= 'Homestead is halted.';
  UpAction.Enabled := true;
  HaltAction.Enabled := false;
  SuspendAction.Enabled := false;
  ResumeAction.Enabled := false;
  ProvisionAction.Enabled := false;
  ReloadAction.Enabled := false;
  SshAction.Enabled := false
end;


procedure TAdminForm.StateIsSuspended;
begin
  StatusLabel.Caption:= 'Homestead is suspended.';
  UpAction.Enabled := false;
  HaltAction.Enabled := false;
  SuspendAction.Enabled := false;
  ResumeAction.Enabled := true;
  ProvisionAction.Enabled := false;
  ReloadAction.Enabled := false;
  SshAction.Enabled := false
end;

end.

