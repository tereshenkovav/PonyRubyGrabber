unit CommonData;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics ;

type

  { TCommonData }

  TCommonData = class
  private
  public
    class var Font:TSfmlFont ;
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

  Result:=True ;
end ;

class procedure TCommonData.UnInit() ;
begin
  Font.Free ;
end ;

end.

