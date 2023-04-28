<div align="center">

# Zenit Emacs

The pinnacle of bikeshedding

![Supports Emacs 29+](https://img.shields.io/badge/Supports-Emacs_29+-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)
![Build status](https://img.shields.io/github/actions/workflow/status/FrauH0lle/emacs.d/ci.yml?label=pipeline&style=flat-square)
![Activity](https://img.shields.io/github/commit-activity/m/FrauH0lle/emacs.d?style=flat-square)

</div>

This is my personal Emacs configuration which was heavily inspired by 

* [Doom Emacs](https://github.com/hlissner/doom-emacs/tree/develop)
* [Spacemacs](https://github.com/syl20bnr/spacemacs/tree/develop)
* [Radian Emacs](https://github.com/raxod502/radian/tree/develop/emacs)
* [Joe Schafer's config](https://github.com/jschaf/dotfiles/tree/master/emacs)

Well, you could say, that it is basically a fork of Doom Emacs and you wouldn't
be wrong. However, this configuration takes and combines ideas from multiple
configurations and is thus my personal best-of.

Feel free to use it and browse the code, but please keep in mind that this is
first and foremost a personal configuration and not a framework like Spacemacs
and Doom Emacs.

## Prerequisites

* Emacs 29+
* [ripgrep](https://github.com/BurntSushi/ripgrep)
* [fd (optional)](https://github.com/sharkdp/fd)

## Install

``` sh
# Clone Emacs configuration
git clone https://github.com/FrauH0lle/emacs.d.git ~/.emacs.d

# Deploy Emacs configuration
~/.emacs.d/bin/emacs-config deploy
```
