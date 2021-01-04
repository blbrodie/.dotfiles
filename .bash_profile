if [ -r ~/.bashrc ]; then
   source ~/.bashrc
fi
if [ -e /Users/benbrodie/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/benbrodie/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
