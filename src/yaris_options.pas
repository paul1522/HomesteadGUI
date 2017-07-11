unit yaris_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  YarisSwitch = (
    ysDev,
    ysAuth,
    ysNode,
    ysBranch,
    ysStorm,
    ysVoyager
    );
  YarisSwitches = set of YarisSwitch;

  YarisOptions = record
    ProjectName: string;
    Switches: YarisSwitches;
    BranchName: string;
  end;


implementation

end.
