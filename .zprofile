#
# /etc/zprofile and ~/.zprofile are run for login shells
#

# all bourne shells should source /etc/profile
if [ "$OS_KIND" = Linux ]; then
   source /etc/profile
fi
