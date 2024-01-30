unit CommonData;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,
  Profile ;

type

  { TCommonData }

  TCommonData = class
  private
  public
    class var Font:TSfmlFont ;
    class var profile:TProfile ;
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
  profile:=TProfile.Create('PonyRubyGrabber') ;
  Result:=True ;
end ;

class procedure TCommonData.UnInit() ;
begin
  Font.Free ;
  profile.Free ;
end ;

end.

