unit CommonData;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,
  Profile, SfmlAnimation ;

type

  { TCommonData }

  TCommonData = class
  private
  public
    class var Font:TSfmlFont ;
    class var profile:TProfile ;
    class var selector:TSfmlAnimation ;
    class function Init():Boolean ;
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
  Result:=True ;
end ;

class procedure TCommonData.UnInit() ;
begin
  Font.Free ;
  selector.Free ;
  profile.Free ;
end ;

end.

