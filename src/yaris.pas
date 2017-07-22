unit yaris;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TYarisDialog }

  TYarisDialog = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DevBox: TCheckBox;
    AuthBox: TCheckBox;
    NodeBox: TCheckBox;
    BranchBox: TCheckBox;
    StormBox: TCheckBox;
    VoyagerBox: TCheckBox;
    BranchField: TEdit;
    NameField: TEdit;
    Label1: TLabel;
    procedure BranchBoxChange(Sender: TObject);
    procedure BranchFieldExit(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  YarisDialog: TYarisDialog;

implementation

{$R *.lfm}

{ TYarisDialog }

procedure TYarisDialog.FormShow(Sender: TObject);
begin
  NameField.Text := '';
  NameField.Color := clDefault;
  DevBox.Checked := False;
  AuthBox.Checked := False;
  NodeBox.Checked := False;
  BranchBox.Checked := False;
  BranchField.Text := 'dev';
  BranchField.Enabled := False;
  StormBox.Checked := False;
  VoyagerBox.Checked := False;
end;

procedure TYarisDialog.Button1Click(Sender: TObject);
begin
  if NameField.Text <> '' then
    Button1.ModalResult := mrOk
  else
  begin
    Button1.ModalResult := mrNone;
    NameField.Color := clRed;
    ShowMessage('The project name cannot be left blank.');
  end;
  ModalResult := Button1.ModalResult;
end;

procedure TYarisDialog.BranchBoxChange(Sender: TObject);
begin
  if BranchBox.Checked then
    BranchField.Enabled := True
  else
    BranchField.Enabled := False;
end;

procedure TYarisDialog.BranchFieldExit(Sender: TObject);
begin
  BranchField.Text := Trim(TrimLeft(BranchField.Text));
  if BranchField.Text = '' then
    BranchField.Text := 'dev';
end;

end.
