Personal Emacs configuration
============================

Installation
------------

### Clone repo
```
git clone https://github.com/etsit/emacs-dotfiles
```

### On Linux / OS X
```
ln -sv emacs-dotfiles/.emacs ~/.emacs
```

### On Windows
1. Copy emacs-dotfiles\.emacs to your user's home folder
2. Set HOME environment variable to point to your user's home folder

### Adjust the following settings to your system and preferences
- Scripts location: default-directory 
- Window size and position:
  - set-frame-size
  - set-frame-position
- Packages to install on start-up
  - Package config: prelude-packages

Start Emacs and wait for packages to be downloaded, compiled and
installed on initial launch.
