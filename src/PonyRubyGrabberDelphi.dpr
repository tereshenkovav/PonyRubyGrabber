﻿program PonyRubyGrabberDelphi;

{$APPTYPE GUI}

{$R *.res}

uses
  main in 'core\main.pas',
  scenetest in 'core\scenetest.pas',
  level in 'core\level.pas',
  commondata in 'core\commondata.pas',
  scenestart in 'core\scenestart.pas',
  subscenemenufin in 'core\subscenemenufin.pas',
  subscenemenugame in 'core\subscenemenugame.pas',
  profile in 'core\profile.pas';

begin
  with TMain.Create() do begin
    Run() ;
    Free ;
  end;
end.
