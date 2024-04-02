program PonyRubyGrabberDelphi;

{$APPTYPE GUI}

{$R *.res}

uses
  main in 'core\main.pas',
  scenegame in 'core\scenegame.pas',
  level in 'core\level.pas',
  commondata in 'core\commondata.pas',
  scenestart in 'core\scenestart.pas',
  subscenemenufin in 'core\subscenemenufin.pas',
  subscenemenugame in 'core\subscenemenugame.pas',
  profile in 'core\profile.pas',
  scenemainmenu in 'core\scenemainmenu.pas',
  scenelevelmenu in 'core\scenelevelmenu.pas',
  scenetotalwin in 'core\scenetotalwin.pas',
  sceneclosehandler in 'core\sceneclosehandler.pas',
  sceneabout in 'core\sceneabout.pas',
  monster in 'core\monster.pas',
  spawner in 'core\spawner.pas',
  hero in 'core\hero.pas',
  scenectrlmenu in 'core\scenectrlmenu.pas';

begin
  with TMain.Create() do begin
    Run() ;
    Free ;
  end;
end.
