unit Profile;

interface

type
  { TProfile }

  TProfile = class
  private
    availlevel:Integer ;
    gamedir:string ;
    function getProfileFile():string ;
  public
    constructor Create(Agamedir:string) ;
    procedure Save() ;
    function getAvailLevel():Integer ;
    procedure MarkLevelCompleted(level:Integer) ;
  end;

implementation
uses Classes, SysUtils,
  Helpers, IniFiles ;

{ TProfile }

constructor TProfile.Create(Agamedir:string);
begin
  gamedir:=Agamedir ;
  if FileExists(getProfileFile()) then begin
    with TIniFile.Create(getProfileFile()) do begin
      availlevel:=ReadInteger('Profile','AvailLevel',0) ;
      Free ;
    end;
  end
  else begin
    availlevel:=0 ;
  end;
end;

function TProfile.getAvailLevel(): Integer;
begin
  Result:=availlevel ;
end;

function TProfile.getProfileFile(): string;
var dir:string ;
begin
  dir:=GetEnvironmentVariable('LOCALAPPDATA')+PATH_SEP+gamedir ;
  if not DirectoryExists(dir) then ForceDirectories(dir) ;
  Result:=dir+PATH_SEP+'profile.ini' ;
end;

procedure TProfile.MarkLevelCompleted(level: Integer);
begin
  if availlevel<level+1 then begin
    availlevel:=level+1 ;
    Save() ;
  end;
end;

procedure TProfile.Save();
begin
  with TIniFile.Create(getProfileFile()) do begin
    WriteInteger('Profile','AvailLevel',availlevel) ;
    Free ;
  end;
end;

end.

