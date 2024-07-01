if NOT "%~1" == "" goto mainproc

echo "Argument - lang code" 
exit

:mainproc

rm -f PonyRubyGrabber-%2-%VERSION%-Win32.zip
7z a -mx9 PonyRubyGrabber-%2-%VERSION%-Win32.zip ..\..\bin
7z a -mx9 PonyRubyGrabber-%2-%VERSION%-Win32.zip ..\..\data

SET TMPDIR=%TEMP%\Uyf72198fGCaH
mkdir %TMPDIR%\bin
copy ..\..\PascalGameEngine\csfml\win32\csfml-audio-2.dll    %TMPDIR%\bin
copy ..\..\PascalGameEngine\csfml\win32\csfml-graphics-2.dll %TMPDIR%\bin
copy ..\..\PascalGameEngine\csfml\win32\csfml-system-2.dll   %TMPDIR%\bin
copy ..\..\PascalGameEngine\csfml\win32\csfml-window-2.dll   %TMPDIR%\bin
copy ..\..\PascalGameEngine\csfml\win32\openal32.dll         %TMPDIR%\bin

mkdir %TMPDIR%\data\texts
echo %1> %TMPDIR%\data\texts\deflang

7z a -mx9 PonyRubyGrabber-%2-%VERSION%-Win32.zip %TMPDIR%\bin
7z a -mx9 PonyRubyGrabber-%2-%VERSION%-Win32.zip %TMPDIR%\data
