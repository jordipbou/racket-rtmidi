# racket-rtmidi
RtMidi bindings for Racket

## Compiling rtmidi

First of all, update submodules to download rtmidi sources with:

    git submodule init
    git submodule update

### Windows

* Open CMake (cmake-gui). Click on "Browse Source..." and select rtmidi folder.
* Click on "Browse build..." and select a directory for build output (you can create a new directory also).
* Click on "Configure"
  - Select your Visual Studio version
  - Select your platform (Win32 or x64)
  - Click "Finish"
* Click on "Generate"
* Open Visual Studio
* Select File > Open > Project/Solution and open RtMidi.sln that was created inside build directory.
* Select Release as Build Configuration (Build > Configuration Manager) and press F7 for build solution.
* You can find rtmidi.dll inside Release directory (inside build directory).