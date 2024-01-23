program PonyRubyGrabberDelphi;

{$APPTYPE GUI}

{$R *.res}

uses
  main in 'core\main.pas',
  scenetest in 'core\scenetest.pas',
  level in 'core\level.pas',
  commondata in 'core\commondata.pas';

begin
  with TMain.Create() do begin
    Run() ;
    Free ;
  end;
end.
