unit CommonData;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,
  Profile, SfmlAnimation, Texts, Languages, ActionConfig ;

type

  { TCommonData }

  TCommonData = class
  private
  public
    class var Font:TSfmlFont ;
    class var profile:TProfile ;
    class var selector:TSfmlAnimation ;
    class var texts:TTexts ;
    class var languages:TLanguages ;
    class var soundon:Boolean ;
    class var actionconfig:TActionConfig ;
    class function Init():Boolean ;
    class procedure reloadTexts() ;
    class procedure UnInit() ;
  end;

const ACTION_LEFT = 'left' ;
const ACTION_RIGHT = 'right' ;
const ACTION_UP = 'up' ;
const ACTION_DOWN = 'down' ;
const ACTION_STOP = 'stop' ;
const ACTION_USE = 'use' ;
const ACTION_TRANSFORM_0 = 'transform_0' ;
const ACTION_TRANSFORM_1 = 'transform_1' ;
const ACTION_TRANSFORM_2 = 'transform_2' ;
const ACTION_TRANSFORM_3 = 'transform_3' ;
const ACTION_TRANSFORM_4 = 'transform_4' ;
const ACTION_TRANSFORM_5 = 'transform_5' ;

implementation
uses SfmlUtils, Helpers ;

{ TCommonData }

class function TCommonData.Init():Boolean ;
var i:Integer ;
begin
  Font:=TSfmlFont.Create('fonts'+PATH_SEP+'arial.ttf');
  selector:=TSfmlAnimation.Create('images'+PATH_SEP+'walkbot.png',5,8);
  selector.Scale(0.75,0.75) ;
  selector.Origin:=SfmlVector2f(100,30) ;
  selector.Play() ;
  profile:=TProfile.Create('PonyRubyGrabber') ;
  languages:=TLanguages.Create() ;
  languages.loadFromFile('texts'+PATH_SEP+'languages');
  languages.setCurrentByFile('texts'+PATH_SEP+'deflang');
  texts:=TTexts.Create() ;
  reloadTexts() ;
  // Инициализация действий
  actionconfig:=TActionConfig.Create() ;
  actionconfig.addAction(ACTION_LEFT,sfKeyLeft) ;
  actionconfig.addAction(ACTION_RIGHT,sfKeyRight) ;
  actionconfig.addAction(ACTION_UP,sfKeyUp) ;
  actionconfig.addAction(ACTION_DOWN,sfKeyDown) ;
  actionconfig.addAction(ACTION_STOP,sfKeySpace) ;
  actionconfig.addAction(ACTION_USE,sfKeyLControl) ;
  actionconfig.addAction(ACTION_TRANSFORM_0,sfKeyNum1) ;
  actionconfig.addAction(ACTION_TRANSFORM_1,sfKeyNum2) ;
  actionconfig.addAction(ACTION_TRANSFORM_2,sfKeyNum3) ;
  actionconfig.addAction(ACTION_TRANSFORM_3,sfKeyNum4) ;
  actionconfig.addAction(ACTION_TRANSFORM_4,sfKeyNum5) ;
  actionconfig.addAction(ACTION_TRANSFORM_5,sfKeyNum6) ;
  soundon:=True ;
  Result:=True ;
end ;

class procedure TCommonData.reloadTexts;
begin
  texts.loadFromFile('texts'+PATH_SEP+'texts.'+languages.getCurrent()) ;
end;

class procedure TCommonData.UnInit() ;
begin
  Font.Free ;
  selector.Free ;
  profile.Free ;
  texts.Free ;
  actionconfig.Free ;
  languages.Free ;
end ;

end.

