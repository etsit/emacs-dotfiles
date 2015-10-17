Personal Emacs configuration
============================

Installation
------------

Clone Git repo in ~/.vim
```
git clone https://github.com/etsit/emacs-dotfiles
```

Copy .emacs to your HOME folder
```
cp emacs-dotfiles/.emacs ~
```

Adjust the following settings to your system and preferences
- Scripts location: default-directory 
- Window size and position:
  - set-frame-size
  - set-frame-position
- Packages to install on start-up
  - Package config: prelude-packages

Start Emacs and wait for packages to be downloaded, compiled and
installed on initial launch.
