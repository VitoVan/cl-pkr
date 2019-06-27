#!/bin/bash

if [ "$TRAVIS_OS_NAME" = "" ]
then
    case $(uname | tr '[:upper:]' '[:lower:]') in
        linux*)
            export OS=linux
            ;;
        darwin*)
            export OS=osx
            ;;
        msys*|mingw*)
            export OS=windows
            ;;
        *)
            export TRAVIS_OS_NAME=notset
            ;;
    esac
else
    export OS=$TRAVIS_OS_NAME
fi

if [[ "$OS" == "windows" ]]; then export EXE=".exe"; fi
if [[ "$OS" == "osx" ]]; then export APP=".app"; fi

export TCLKIT=bin/tclkit-gui$EXE

mkdir -p bin

rm -rf bin/color-picker$EXE
sbcl --disable-debugger \
     --load cl-pkr.asd \
     --eval "(ql:quickload 'cl-pkr)" \
     --eval "(asdf:make :cl-pkr)"

rm -rf out
mkdir -p out

if [[ "$OS" != "windows" ]]; then
    if [ ! -f "$TCLKIT" ]; then
        wget -O  $TCLKIT \
             https://github.com/VitoVan/kitgen/releases/download/8.6.9/$OS-tclkit-gui$EXE
    fi
    chmod +x bin/tclkit-gui;
fi

if [[ "$OS" == "windows" ]]
then
    if [ "$EB" = "" ]; then
        echo "Please set env EB to the path of editbin.exe"
        exit 42
    fi
    if [ ! -f bin/rh/rh.exe ]; then
        wget http://www.angusj.com/resourcehacker/resource_hacker.zip
        unzip resource_hacker.zip -d bin/rh
        mv bin/rh/ResourceHacker.exe bin/rh/rh.exe
    fi
    if [ ! -f bin/warp-packer.exe ]; then
        wget -O bin/warp-packer.exe https://github.com/dgiagio/warp/releases/download/v0.3.0/windows-x64.warp-packer.exe
    fi
    if [ ! -f "$TCLKIT" ]; then
        # get the not-UPX-ed version, to change icon with Resource Hacker
        wget -O $TCLKIT \
             https://github.com/VitoVan/kitgen/releases/download/continuous-windows/windows-tclkit-gui.ex
    fi
    mkdir -p out/tmp
    cp ./bin/color-picker.exe ./out/tmp/color-picker.exe
    bin/rh/rh.exe -open ./bin/tclkit-gui.exe -save ./out/tmp/tclkit-gui-noicon.exe -action delete -mask ICONGROUP,,
    bin/rh/rh.exe -open ./out/tmp/tclkit-gui-noicon.exe -save ./out/tmp/tclkit-gui.exe -action addskip -res ./resources/iconfile.ico -mask ICONGROUP,TK
    rm -rf ./out/tmp/tclkit-gui-noicon.exe
    bin/warp-packer.exe --arch windows-x64 --input_dir ./out/tmp/ --exec color-picker.exe --output ./out/tmp/color-picker-warp.exe
    bin/rh/rh.exe -open ./out/tmp/color-picker-warp.exe -save ./out/color-picker.exe -action addskip -res ./resources/iconfile.ico -mask ICONGROUP,MAINICON
    "$EB" /subsystem:windows ./out/color-picker.exe
    rm -rf out/tmp
fi

if [[ "$OS" == "osx" ]]
then
    export OSX_APP_DIR=out/color-picker.app/Contents
    mkdir -p $OSX_APP_DIR
    cp ./resources/Info.plist $OSX_APP_DIR/
    mkdir -p $OSX_APP_DIR/MacOS
    cp ./bin/color-picker ./bin/tclkit-gui $OSX_APP_DIR/MacOS
    mkdir -p $OSX_APP_DIR/Resources
    cp ./resources/iconfile.icns $OSX_APP_DIR/Resources
    cd out && zip -r -9 color-picker$APP.zip color-picker.app && cd ..
    rm -rf out/color-picker.app
fi

if [[ "$OS" == "linux" ]]
then
    export LINUX_APP_DIR=out/color-picker.AppDir
    mkdir -p $LINUX_APP_DIR
    cp resources/AppRun $LINUX_APP_DIR/AppRun
    cp resources/color-picker.desktop $LINUX_APP_DIR/
    cp resources/iconfile.svg $LINUX_APP_DIR/
    mkdir -p $LINUX_APP_DIR/usr/bin
    cp bin/color-picker $LINUX_APP_DIR/usr/bin/
    cp bin/tclkit-gui $LINUX_APP_DIR/usr/bin/
    if [ ! -f bin/appimagetool ]; then
        wget -O bin/appimagetool \
             https://github.com/AppImage/AppImageKit/releases/download/12/appimagetool-x86_64.AppImage
    fi
    chmod +x bin/appimagetool
    cd out && ../bin/appimagetool color-picker.AppDir color-picker.AppImage && cd ..

    rm -rf out/color-picker.AppDir
fi
