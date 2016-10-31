use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME     => 'perltidy-mode',
    AUTHOR   => 'Joshua ben Jore <jjore@cpan.org>',
    VERSION  => '0.02',
    ABSTRACT => 'Automatically perltidies your perl source code',
    dist     => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean => { FILES => 'perltidy-mode-*' },
);
