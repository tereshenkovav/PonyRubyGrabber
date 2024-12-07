for i in `git tag --list --sort=v:refname`; do BUILDTAG=$i; done

for i in `git rev-parse HEAD`; do BUILDCOMMIT=$i; done
BUILDCOMMIT=${BUILDCOMMIT:0:8}

for i in `git rev-parse --abbrev-ref HEAD`; do BUILDBRANCH=$i; done

echo $BUILDTAG $BUILDCOMMIT $BUILDBRANCH

VERSION=${BUILDTAG:1}

echo $BUILDTAG > ../../data/texts/version.txt
echo $BUILDCOMMIT >> ../../data/texts/version.txt
echo $BUILDBRANCH >> ../../data/texts/version.txt

appdir=/tmp/PonyRubyGrabber.app
mkdir $appdir
mkdir $appdir/Contents
mkdir $appdir/Contents/MacOS
mkdir $appdir/Contents/Frameworks
mkdir $appdir/Contents/Resources

cp Info.plist $appdir/Contents
cp Pkginfo $appdir/Contents

cp PonyRubyGrabber.icns $appdir/Contents/Resources

cp ../../bin/PonyRubyGrabberFPC $appdir/Contents/MacOS/PonyRubyGrabber
cp -r ../../data $appdir/Contents

cp -R /usr/local/lib/libcsfml*.dylib $appdir/Contents/Frameworks
cp -R /usr/local/lib/libsfml*.dylib $appdir/Contents/Frameworks

cp -R /Library/Frameworks/FLAC.framework $appdir/Contents/Frameworks
cp -R /Library/Frameworks/freetype.framework $appdir/Contents/Frameworks
cp -R /Library/Frameworks/ogg.framework $appdir/Contents/Frameworks
cp -R /Library/Frameworks/OpenAL.framework $appdir/Contents/Frameworks
cp -R /Library/Frameworks/vorbis.framework $appdir/Contents/Frameworks
cp -R /Library/Frameworks/vorbisenc.framework $appdir/Contents/Frameworks
cp -R /Library/Frameworks/vorbisfile.framework $appdir/Contents/Frameworks

cd /tmp 

echo en > $appdir/Contents/data/texts/deflang
zip -r9 PonyRubyGrabber-EN-$VERSION-MacOS.app.zip PonyRubyGrabber.app
hdiutil create -srcfolder $appdir -volname "PonyRubyGrabber" -fs HFS+ -fsargs "-c c=64,a=16,e=16" -format UDZO -size 30000k -imagekey zlib-level=9 PonyRubyGrabber-EN-$VERSION-MacOS.dmg

echo ru > $appdir/Contents/data/texts/deflang
zip -r9 PonyRubyGrabber-RU-$VERSION-MacOS.app.zip PonyRubyGrabber.app
hdiutil create -srcfolder $appdir -volname "PonyRubyGrabber" -fs HFS+ -fsargs "-c c=64,a=16,e=16" -format UDZO -size 30000k -imagekey zlib-level=9 PonyRubyGrabber-RU-$VERSION-MacOS.dmg
