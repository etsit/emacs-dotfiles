Personal Emacs configuration
============================

Installation
------------

### Clone repo
```
git clone https://github.com/etsit/emacs-dotfiles
```

### On Linux / MacOS
```
ln -sv ~/emacs-dotfiles/.emacs ~/.emacs
touch ~/.emacs-custom.el
mkdir ~/.emacs-d
```

### On Windows
- Copy emacs-dotfiles\.emacs to your user's home folder
- Set HOME environment variable to point to your user's home folder

### Adjust the following settings to your system and preferences
- Scripts location: default-directory 
- Window size and position:
  - set-frame-size
  - set-frame-position
- Packages to install on start-up
  - Package config: prelude-packages
- Paths to
  - Org-mode agenda files folder: look for 'load-org-agenda-files-recursively'
  - Default directory: look for 'command-line-default-directory'

Start Emacs and wait for packages to be downloaded, compiled and
installed on initial launch.
