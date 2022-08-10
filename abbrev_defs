;;-*-coding: utf-8;-*-
(define-abbrev-table 'cperl-mode-abbrev-table
  '(
    ("usedump" "use Data::Dumper qw(Dumper);" pde-abbv-no-blank :count 0)
    ("useenc" "use Encode qw(encode decode from_to);" pde-abbv-no-blank :count 0)
    ("usegtk" "use Gtk2 '-init';
use Glib qw(TRUE FALSE);

my $window = Gtk2::Window->new('toplevel');
$window->signal_connect('delete_event' => sub { Gtk2->main_quit; });" pde-abbv-no-blank :count 0)
    ("useopt" "use Getopt::Long;
GetOptions();" pde-abbv-no-blank :count 0)
   ))

