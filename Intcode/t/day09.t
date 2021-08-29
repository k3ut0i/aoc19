# -*- mode: raku -*-
use lib 'lib';
use lib '../lib';
use Test;
use-ok("Intcode");
use Intcode;

subtest {
    my @prog = [109,1,204,-1,1001,100,1,100,1008,100,
                16,101,1006,101,0,99];
    my $s = SystemI.new(:mem(@prog));
    run-full($s);
    my @output = $s.output;
    is-deeply(@output, @prog, "Output of the full run: is quine?");
}, "example 1";

subtest {
    my $s = SystemI.new(:mem([1102,34915192,34915192,7,4,7,99,0]));
    run-full($s);
    is($s.output.first.Str.chars, 16, "Output size");
}, "example 2";

subtest {
    my $s = SystemI.new(:mem(104,1125899906842624,99));
    run-full($s);
    is($s.output.first,1125899906842624, "output");
}, "example 3";
done-testing;
