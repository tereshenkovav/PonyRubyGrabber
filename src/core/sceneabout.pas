unit SceneAbout;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Scene, Helpers ;

type

  { TSceneAbout }

  TSceneAbout = class(TScene)
  private
    textTitle:TSfmlText ;
    textVer:TSfmlText ;
    textInfo:TSfmlText ;
    textCredits:TSfmlText ;
    textList:TSfmlText ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses SceneMainMenu, SfmlUtils, CommonData ;

function TSceneAbout.Init():Boolean ;
var str:string ;
begin
  textTitle:=createText(TCommonData.Font,TCommonData.texts.getText('ABOUT_TITLE'),28,SfmlWhite) ;

  if FileExists('texts'+PATH_SEP+'version.txt') then begin
    with TStringList.Create() do begin
      LoadFromFile('texts'+PATH_SEP+'version.txt') ;
      str:=Format(TCommonData.texts.getText('ABOUT_VERSION'),
        [Strings[0],Strings[1],Strings[2]]) ;
      Free ;
    end;
  end
  else
    str:='' ;
  textVer:=createText(TCommonData.Font,str,20,SfmlColorFromRGB(160,160,160)) ;

  textInfo:=createText(TCommonData.Font,TCommonData.texts.getText('ABOUT_INFO'),24,SfmlWhite) ;
  textCredits:=createText(TCommonData.Font,TCommonData.texts.getText('ABOUT_CREDITS'),28,SfmlWhite) ;

  if FileExists('texts'+PATH_SEP+'credits.txt') then
    str:=readAllText('texts'+PATH_SEP+'credits.txt').Replace(chr(13),'')
  else
    str:='' ;
  textList:=createText(TCommonData.Font,str,24,SfmlWhite) ;
  Result:=True ;
end ;

function TSceneAbout.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
begin
  Result:=Normal ;
  for event in events do
    if (event.event.EventType = sfEvtKeyPressed) then begin
      nextscene:=TSceneMainMenu.Create() ;
      Exit(TSceneResult.Switch) ;
    end ;
end ;

procedure TSceneAbout.RenderFunc() ;
begin
  DrawTextCentered(textTitle,wwidth/2,50) ;
  DrawTextCentered(textVer,wwidth/2,100) ;
  DrawTextCentered(textInfo,wwidth/2,170) ;
  DrawTextCentered(textCredits,wwidth/2,340) ;
  DrawTextCentered(textList,wwidth/2,400) ;
end ;

procedure TSceneAbout.UnInit() ;
begin
  textTitle.Free ;
  textVer.Free ;
  textInfo.Free ;
  textCredits.Free ;
  textList.Free ;
end ;

end.
