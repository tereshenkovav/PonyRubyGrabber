unit Profile;

interface
uses IniFiles ;

type
  { TProfile }

  TProfile = class
  private
    gamedir:string ;
    fullscr:Boolean ;
    soundon:Boolean ;
    function getProfileFile():string ;
  protected
    procedure WriteExData(ini:TIniFile) ; virtual ;
    procedure ReadExData(ini:TIniFile) ; virtual ;
    procedure InitExData() ; virtual ;
  public
    constructor Create(Agamedir:string) ;
    procedure switchFullScreen() ;
    function isFullScreen():Boolean ;
    procedure switchSoundOn() ;
    function isSoundOn():Boolean ;
    procedure Save() ;
  end;

implementation
uses Classes, SysUtils,
  Helpers ;

{ TProfile }

constructor TProfile.Create(Agamedir:string);
var ini:TIniFile ;
begin
  gamedir:=Agamedir ;
  if FileExists(getProfileFile()) then begin
    ini:=TIniFile.Create(getProfileFile()) ;
    fullscr:=ini.ReadBool('Profile','Fullscr',False) ;
    soundon:=ini.ReadBool('Profile','SoundOn',True) ;
    ReadExData(ini) ;
    ini.Free ;
  end
  else begin
    fullscr:=False ;
    soundon:=True ;
    InitExData() ;
  end;
end;

function TProfile.getProfileFile(): string;
var dir:string ;
begin
  {$ifdef unix}
  dir:=GetEnvironmentVariable('HOME')+'/.local/share/'+gamedir ;
  {$else}
  dir:=GetEnvironmentVariable('LOCALAPPDATA')+PATH_SEP+gamedir ;
  {$endif}
  if not DirectoryExists(dir) then ForceDirectories(dir) ;
  Result:=dir+PATH_SEP+'profile.ini' ;
end;

procedure TProfile.InitExData;
begin
end;

function TProfile.isFullScreen: Boolean;
begin
  Result:=fullscr ;
end;

function TProfile.isSoundOn: Boolean;
begin
  Result:=soundon ;
end;

procedure TProfile.ReadExData(ini: TIniFile);
begin
end;

procedure TProfile.Save();
var ini:TIniFile ;
begin
  ini:=TIniFile.Create(getProfileFile()) ;
  ini.WriteBool('Profile','FullScr',fullscr) ;
  ini.WriteBool('Profile','SoundOn',soundon) ;
  WriteExData(ini) ;
  ini.Free ;
end;

procedure TProfile.switchFullScreen;
begin
  fullscr:=not fullscr ;
  Save() ;
end;

procedure TProfile.switchSoundOn;
begin
  soundon:=not soundon ;
  Save() ;
end;

procedure TProfile.WriteExData(ini: TIniFile);
begin
end;

end.

