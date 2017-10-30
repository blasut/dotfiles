# Dotfiles

These are my dotfiles, finally getting around to get them organized.

Followed this guide: http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html?round=two

# Switching between emacs and spacemacs

if using emacs:
stow -D emacs
stow spacemacs
else:
stow -D spacemacs
stow emacs

# ivymacs

The ivymacs folder is an expteriment with using def-package, which-key, evil, and ivy for a spacemacs inspired feeling. It's good for experimenting with different layers and sometimes spacemacs is way to big or hard to use with some special package. 

# Notes
- Install ag and use that for helm searching
- Figure this out: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org#replacing-text-in-several-files


## Setup

In the setup folder there are some scripts to help get my comp to the same settings


TODO:

- Check this out: http://whattheemacsd.com/project-defuns.el-01.html
- Check out: http://inclojurewetrust.blogspot.de/2013/01/duplicating-s-expressions-on-line.html
- Check out: http://batsov.com/projectile/
- Check out: http://tuhdo.github.io/helm-projectile.html
- Check out: https://github.com/magnars/.emacs.d/blob/master/settings/key-bindings.el#L30-L43
