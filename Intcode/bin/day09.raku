use lib '../lib';
use Intcode;

sub MAIN($filename){
    my $str = $filename.IO.slurp.chomp;
    my Int @vals = $str.split(",").map(&val);
    #part 1
    my $s1 = SystemI.new(:mem(@vals));
    $s1.input.push(1);
    run-full($s1);
    die "More that one output value" unless $s1.output.elems == 1;
    say $s1.output.first;

    #part 2
    my $s2 = SystemI.new(:mem(@vals));
    $s2.input.push(2);
    run-full($s2);
    say $s2.output.first;
};
