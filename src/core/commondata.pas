unit CommonData;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,
  Profile, SfmlAnimation, Texts, Languages ;

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
    class function Init():Boolean ;
    class procedure reloadTexts() ;
    class procedure UnInit() ;
  end;

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
  languages.Free ;
end ;

end.

