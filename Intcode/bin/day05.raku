use lib '../lib';
use Intcode;

sub MAIN($filename) {
    my $str = $filename.IO.slurp.chomp;
    my Int @vals = $str.split(",").map(&val);
    my $s = SystemI.new(:mem(@vals));
    $s.input = 1;

    my $output;
    while (run-until-event($s) == Output) {
        $output = $s.output.shift;
        last unless $output == 0;
    }
    if $s.step == Hlt {
        say $output;
    } else {
        die "Diagnostic failed after: " ~ $s.get-ip ~ " " ~ $s.state;
    }

    # part 2
    my $s2 = SystemI.new(:mem(@vals));
    $s2.input = 5; # diagnostic ID
    say run-full($s2).output.first;
}
