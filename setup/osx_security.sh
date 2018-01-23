###############################################################################
# Security                                                                    #
###############################################################################
# this destroys the filevault key when going to standby
sudo pmset -a destroyfvkeyonstandby 1

# enter hibernate immedialety after going to sleep
sudo pmset -a hibernatemode 25

# Settings for making sure the hibernate immediately doesn't fuck up my shit
sudo pmset -a powernap 0
sudo pmset -a standby 0
sudo pmset -a standbydelay 0
sudo pmset -a autopoweroff 0


# Set firmware password
# https://support.apple.com/en-us/HT204455

# Set computer and locahostname
sudo scutil --set ComputerName sup
sudo scutil --set LocalHostName local

# Hide user account in the macOS login window
# https://support.apple.com/en-us/HT203998
# sudo dscl . create /Users/USERNAME IsHidden 1

# Firewall
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate on
## Enable logging
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setloggingmode on
## Enable stealth mode
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setstealthmode on

# Automatic whitelisting
## For built-in apps
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setallowsigned off
## For signed apps
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setallowsignedapp off

echo "Restarting firewall"
sudo pkill -HUP socketfilterfw

# Disable apple captive portal assistant
sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.captive.control Active -bool false

brew install openssl

# Remove info about what I type
rm -rfv "~/Library/LanguageModeling/*" "~/Library/Spelling/*" "~/Library/Suggestions/*"
chmod -R 000 ~/Library/LanguageModeling ~/Library/Spelling ~/Library/Suggestions
chflags -R uchg ~/Library/LanguageModeling ~/Library/Spelling ~/Library/Suggestions

# Clean the Siri db
rm -rfv ~/Library/Assistant/SiriAnalytics.db
chmod -R 000 ~/Library/Assistant/SiriAnalytics.db
chflags -R uchg ~/Library/Assistant/SiriAnalytics.db

# Remove logs of downloaded files
# https://gist.github.com/hellais/4552537
sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent where LSQuarantineEventIdentifier like "%%";'

# To never store such information you can sym link it to dev null
ln -s /dev/null ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV2

