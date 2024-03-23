move ..\..\bin\PonyRubyGrabberDelphi.exe ..\..\bin\PonyRubyGrabber.exe

@echo off
for /F %%i in ('git tag --list --sort=committerdate') do set BUILDTAG=%%i
for /F %%i in ('git rev-parse HEAD') do set BUILDCOMMIT=%%i
set BUILDCOMMIT=%BUILDCOMMIT:~0,8%
for /F %%i in ('git branch --show-current') do set BUILDBRANCH=%%i

echo %BUILDTAG% %BUILDCOMMIT% %BUILDBRANCH%

echo %BUILDTAG% > ..\..\data\texts\version.txt
echo %BUILDCOMMIT% >> ..\..\data\texts\version.txt
echo %BUILDBRANCH% >> ..\..\data\texts\version.txt

SET VERSION=%BUILDTAG:~1%
SET PASSFMLDIR=F:\LIBS\PasSFML\PasSFML-master
"C:\Program Files (x86)\NSIS\makensis.exe" /DPASSFMLDIR=%PASSFMLDIR% /DVERSION=%VERSION% /DGAMELANG=ru /DUPPERLANG=RU PonyRubyGrabber.nsi
"C:\Program Files (x86)\NSIS\makensis.exe" /DPASSFMLDIR=%PASSFMLDIR% /DVERSION=%VERSION% /DGAMELANG=en /DUPPERLANG=EN PonyRubyGrabber.nsi

call create_zip32.bat ru RU
call create_zip32.bat en EN
