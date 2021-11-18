# emacs-config
My personnal config for emacs


* Install pip packages from pip file
* Add (load-file "<path_to_repo>/config.el")
* ln -s <path_to_repo>/pycodestyle ~/.config/pycodestyle
* ln -s $PWD/.tmux.conf ~/.
* ln -s $PWD/alacritty.yml ~/.config/alacritty/alacritty.yml

* install system packages:
sudo apt-get install -y libjansson4 libjansson-dev
wget -qO- https://deb.nodesource.com/setup_16.x | sudo -E bash -
sudo apt install nodejs
sudo npm install -g pyright
sudo add-apt-repository ppa:ubuntu-elisp/ppa
sudo apt update
sudo apt upgrade
sudo apt-get install pandoc

note:
inside screen:
```
TERM=xterm-256color emacs -nw
```
