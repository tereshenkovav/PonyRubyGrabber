unit sfmlutils ;

interface
uses SfmlGraphics,SfmlSystem ;

type
  TSpriteLoaderOption = (sloCentered,sloNoSmooth,sloMipmap) ;
  TSpriteLoaderOptions = set of TSpriteLoaderOption ;
  TFuncConvertPixel = function (c:TSfmlColor):TSfmlColor ;

function loadSprite(filename:string):TSfmlSprite ; overload ;
function loadSprite(filename:string; options:TSpriteLoaderOptions):TSfmlSprite ; overload ;
function createText(Font:TSfmlFont; utf8str:string; size:Integer;
  color:TSfmlColor):TSfmlText ;
function createSFMLColor(color:Cardinal):TSfmlColor ;
function createSFMLColorAlpha(color:Cardinal; alpha:Byte):TSfmlColor ;
function SfmlVector2i(X, Y: Integer): TSfmlVector2i;
procedure convertSpriteTexture(sprite:TSfmlSprite; converter:TFuncConvertPixel) ;
function funcMakeGray(c:TSfmlColor):TSfmlColor ;

implementation
uses SysUtils, Helpers ;

function funcMakeGray(c:TSfmlColor):TSfmlColor ;
var gr:Integer ;
begin
  gr:=(c.R+c.G+c.B) div 3 ;
  Result:=SfmlColorFromRGBA(gr,gr,gr,c.A) ;
end;

procedure convertSpriteTexture(sprite:TSfmlSprite; converter:TFuncConvertPixel) ;
var x,y:Integer ;
    pimg:PSfmlImage ;
begin
  pimg:=SfmlTextureCopyToImage(sprite.Texture) ;
  for x:=0 to SfmlImageGetSize(pimg).x-1 do
    for y:=0 to SfmlImageGetSize(pimg).y-1 do
      SfmlImageSetPixel(pimg,x,y,converter(SfmlImageGetPixel(pimg,x,y))) ;
  SfmlTextureUpdateFromImage(sprite.Texture,pimg,0,0) ;
  SfmlImageDestroy(pimg) ;
end;

function loadSprite(filename:string):TSfmlSprite ;
begin
  Result:=loadSprite(filename,[]) ;
end ;

function loadSprite(filename:string; options:TSpriteLoaderOptions):TSfmlSprite ;
var tex:TSfmlTexture ;
begin
  Result:=TSfmlSprite.Create;
  tex:=TSfmlTexture.Create(filename) ;
  if not(sloNoSmooth in options) then tex.Smooth:=True ;
  if (sloMipmap in options) then tex.GenerateMipmap() ;
  Result.SetTexture(tex,True) ;
  if (sloCentered in options) then
    Result.Origin:=SfmlVector2f(tex.Size.X/2,tex.Size.Y/2) ;
end ;

function createText(font:TSfmlFont; utf8str:string; size:Integer;
  color:TSfmlColor):TSfmlText ;
begin
  Result:=TSfmlText.Create;
  Result.UnicodeString:=UTF8Decode(utf8str);
  Result.Font:=font.Handle;
  Result.CharacterSize:=size;
  Result.FillColor:=color;
end ;

function createSFMLColor(color:Cardinal):TSfmlColor ;
begin
  Result.A:=$FF ;
  Result.B:=color and $FF ;
  Result.G:=(color shr 8) and $FF ;
  Result.R:=(color shr 16) and $FF ;
end;

function createSFMLColorAlpha(color:Cardinal; alpha:Byte):TSfmlColor ;
begin
  Result.A:=alpha ;
  Result.B:=color and $FF ;
  Result.G:=(color shr 8) and $FF ;
  Result.R:=(color shr 16) and $FF ;
end;

function SfmlVector2i(X, Y: Integer): TSfmlVector2i;
begin
  Result.X := X;
  Result.Y := Y;
end;

end.