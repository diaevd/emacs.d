all : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1
realclean : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 realclean
	/usr/bin/perl -e '1 while unlink $$ARGV[0]' Makefile
distclean : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 distclean
	/usr/bin/perl -e '1 while unlink $$ARGV[0]' Makefile


force_do_it :
	@ true
build : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 build
clean : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 clean
code : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 code
config_data : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 config_data
diff : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 diff
dist : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 dist
distcheck : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 distcheck
distdir : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 distdir
disthtml : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 disthtml
distinstall : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 distinstall
distmeta : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 distmeta
distsign : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 distsign
disttest : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 disttest
docs : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 docs
fakeinstall : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 fakeinstall
help : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 help
html : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 html
info : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 info
install : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 install
installdeps : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 installdeps
manifest : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 manifest
manifest_skip : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 manifest_skip
manpages : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 manpages
muse : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 muse
pardist : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 pardist
ppd : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 ppd
ppmdist : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 ppmdist
prereq_data : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 prereq_data
prereq_report : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 prereq_report
pure_install : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 pure_install
retest : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 retest
skipcheck : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 skipcheck
test : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 test
testall : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 testall
testcover : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 testcover
testdb : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 testdb
testpod : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 testpod
testpodcoverage : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 testpodcoverage
versioninstall : force_do_it
	/usr/bin/perl Build --makefile_env_macros 1 versioninstall

.EXPORT : INSTALLSITEBIN INSTALLVENDORMAN3DIR INSTALLSITEMAN1DIR INSTALLVENDORLIB INSTALLVENDORBIN INSTALLDIRS INSTALLSITEMAN3DIR INSTALLPRIVLIB TEST_VERBOSE INSTALLMAN1DIR INSTALLMAN3DIR DESTDIR INSTALL_BASE VERBINST INSTALLVENDORMAN1DIR UNINST PREFIX INSTALLSITELIB INSTALLSCRIPT INSTALLBIN INSTALLVENDORSCRIPT INSTALLSITESCRIPT INSTALLSITEARCH INC POLLUTE INSTALLVENDORARCH INSTALLARCHLIB

