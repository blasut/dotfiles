## # reset permissions back to 'normal'
## sudo chown -R root:wheel /usr/local


# allow staffs to manage homebrew's local install directory
sudo chgrp -R staff /usr/local/var
sudo chmod -R g+w /usr/local/var

sudo chgrp -R staff /usr/local/bin
sudo chmod -R g+w /usr/local/bin

sudo chgrp -R staff /usr/local/Cellar
sudo chmod -R g+w /usr/local/Cellar

sudo chgrp -R staff /usr/local/lib
sudo chmod -R g+w /usr/local/lib

sudo chgrp -R staff /usr/local/etc
sudo chmod -R g+w /usr/local/etc

sudo chgrp -R staff /usr/local/opt
sudo chmod -R g+w /usr/local/opt

# These folder doesn't exist anymore in high sierra?
# sudo chgrp -R staff /usr/local/include
# sudo chmod -R g+w /usr/local/include

sudo chgrp -R staff /usr/local/share
sudo chmod -R g+w /usr/local/share

sudo chgrp -R staff /usr/local/Homebrew
sudo chmod -R g+w /usr/local/Homebrew

sudo chgrp -R staff /usr/local/sbin
sudo chmod -R g+w /usr/local/sbin

sudo chgrp -R staff /usr/local/Frameworks
sudo chmod -R g+w /usr/local/Frameworks

# not sure why this was needed
sudo chgrp -R staff /usr/local/var/homebrew
sudo chmod -R g+w /usr/local/var/homebrew

# Fix persm for casks
sudo chgrp -R staff /usr/local/Caskroom
sudo chmod -R g+w /usr/local/Caskroom

# allow staffs to homebrew's local cache of formulae and source files
sudo chgrp -R staff ~/Library/Caches/Homebrew
sudo chmod -R g+w ~/Library/Caches/Homebrew

echo "homebrew can now be used by any user in the staff group."
