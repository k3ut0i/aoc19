use lib '../lib';
use Intcode;

sub MAIN($filename) {
    my $str = $filename.IO.slurp.chomp;
    my Int @vals = $str.split(",").map(&val);
    my $s = SystemI.new(:mem(@vals));
    $s.input = 1;

    repeat {
        run-until-output($s);
    } while $s.output == 0;

    if $s.step == Hlt {
        say $s.output;
    } else {
        die "Diagnostic failed after: " ~ $s.get-ip;
    }

    # part 2
    my $s2 = SystemI.new(:mem(@vals));
    $s2.input = 5; # diagnostic ID
    say run-full($s2).output;
}
