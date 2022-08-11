![Color Picker Icon](resources/iconfile.png)

# cl-pkr
Cross-Platform Color Picker Written in Common Lisp

![platform support](https://img.shields.io/badge/platform-Linux%20%7C%20macOS%20%7C%20Windows-blue.svg) [![Build Status](https://travis-ci.com/VitoVan/cl-pkr.svg?token=zGyrVcujB9VafCKBLXZc&branch=master)](https://travis-ci.com/VitoVan/cl-pkr)

### Screenshots:

- Linux

  ![Screenshot on Linux](screenshots/linux.png)

- macOS

  ![Screenshot on macOS](screenshots/osx.png)

- Windows

  ![Screenshot on Windows](screenshots/windows.png)

### Downloads:

- Linux

    [![color-picker.AppImage](https://img.shields.io/badge/Linux-color--picker.AppImage-blue.svg?logo=linux)](<https://github.com/VitoVan/cl-pkr/releases/latest/download/color-picker.AppImage>)

- macOS

    [![color-picker.app](https://img.shields.io/badge/macOS-color--picker.app-blue.svg?logo=apple)](<https://github.com/VitoVan/cl-pkr/releases/latest/download/color-picker.app.zip>)

- Windows

    [![color-picker.exe](https://img.shields.io/badge/Windows-color--picker.exe-blue.svg?logo=windows)](<https://github.com/VitoVan/cl-pkr/releases/latest/download/color-picker.exe>
    )

> You are supposed to run it on a 64-bit machine

> Tested on Fedora 30, macOS Mojave and Windows 10 1809

### Usage:

- On macOS:
    - [Cmd + C] to Copy HEX
    - [Cmd + Shift + C] to Copy RGB
    - [Cmd + Option + C] to Copy HSL

- On Windows or Linux
    - [Control + C] to Copy HEX
    - [Control + Shift + C] to Copy RGB
    - [Control + Alt + C] to Copy HSL

### Known Issues:

- Multi-Monitor not supported
    You can only pick color from the main display

### Related Links:

- [Meditations on Color Picker](http://vito.sdf.org/picker.html)

- [cl-icebox](https://github.com/VitoVan/cl-icebox) Cross-Platform GUI framework written in Common Lisp

----

### Hacking:

1. Make sure you have SBCL with Quicklisp installed

    - Install a proper SBCL, you can download [here](http://www.sbcl.org/platform-table.html)
    - Install Quicklisp, you can follow the tutorial [here](https://www.quicklisp.org/beta/#installation)

2. Make sure you have a bin folder and have a proper tclkit inside

    - `mkdir -p bin` or just right click to create a folder name `bin`
    - Download yourself a proper tclkit and rename it to `tclkit-gui` [here](https://github.com/VitoVan/kitgen/releases/tag/8.6.9)

3. build your application

    ```bash
    sbcl --disable-debugger --load cl-pkr.asd --eval "(ql:quickload 'cl-pkr)" --eval "(asdf:make :cl-pkr)"
    ```

Voilà! Check your `bin` folder for the magic!

> What? You use [Emacs](https://www.gnu.org/software/emacs/) and [SLIME](https://common-lisp.net/project/slime/)? Great!

> Eval `(progn (load "cl-pkr.asd") (ql:quickload 'cl-pkr) (setf cl-icebox::*hacking* t))` in your REPL, then you can call `(cl-pkr:color-picker)`, have fun!

### Deploy:

There's two options:

1. Use [Travis CI](https://travis-ci.com/) for the good of your health

    you need to add `GITHUB_TOKEN` according to [uploadtool](https://github.com/probonopd/uploadtool)

2. Use `deploy.sh` in the root folder

    Run `deploy.sh` and then check your `out` folder.

    - Linux and macOS should work out of the box (unless you don't have [wget](https://www.gnu.org/software/wget/) or internet connection).
    - on Windows, you need to have
        - A decent BASH, [Git BASH](https://git-scm.com/download/win) or [MSYS2](https://www.msys2.org/) both should work fine
        - Make sure you have [unzip](http://infozip.sourceforge.net/UnZip.html) and [wget](https://www.gnu.org/software/wget/) in your BASH.
        - [EDITBIN](https://docs.microsoft.com/en-us/cpp/build/reference/editbin-reference) which is part of [Build Tools for Visual Studio](https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2019)
            - Find the location of your `vcvarsall.bat`, then open `cmd`, and call `vcvarsall.bat x64`
            - Then, type `where editbin`, you'll get the absolute path of `editbin.exe`
            - `export EB='C:\long path with space\editbin.exe'` in your BASH
            - Now you are blessed to run `deploy.sh`

### Credits

- Icon made by [DinosoftLabs](https://www.flaticon.com/authors/dinosoftlabs) from www.flaticon.com
- [Tcl/Tk](https://www.tcl.tk/)
- Tclkit build system http://tclkit.googlecode.com/, [forked here](https://github.com/VitoVan/kitgen)
- [Resource Hacker](http://www.angusj.com/resourcehacker/)
- [Warp](https://github.com/dgiagio/warp)
- [AppImage](https://appimage.org/)

---

![Lisp Caution](http://www.lisperati.com/lisplogo_warning2_256.png)
