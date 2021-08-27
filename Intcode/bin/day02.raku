use lib '../lib';
use Intcode;


sub system-fun(Int @prog, Int $noun, Int $verb --> Int) {
    @prog[1] = $noun; @prog[2] = $verb;
    my $s = SystemI.new(:mem(@prog));
    return run-full($s).get-mem(0);
}
sub MAIN($filename) {
    my $str = $filename.IO.slurp.chomp;
    my Int @vals = $str.split(",").map(&val);
    say system-fun(@vals, 12, 2); # part 1;

    for 0 .. 99 -> $noun {
	for 0 .. 99 -> $verb {
	    if system-fun(@vals, $noun, $verb) == 19690720 {
		say $noun, " ", $verb;
		return 0;
	    }
	}
    }
    return 1;
}
