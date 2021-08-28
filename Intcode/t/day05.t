# -*- mode : raku -*-

use lib 'lib';
use lib '../lib';
use Test;
use-ok("Intcode");
use Intcode :DEFAULT, :testing;

is-deeply(parse-opcode(1002),
   Map.new('type' => Mul, 'args' => [Link, Direct, Link]),
   "opcode parsing for parameter modes: 1002");

is-deeply(parse-opcode(99),
   Map.new('type' => Hlt, 'args' => [Link, Link, Link]),
   "opcode parsing for parameter modes: 99");

subtest {
    my $s = SystemI.new(:mem([3,0,4,0,99]));
    my $e = run-until-event($s, 13);
    is($e, Output, "Event type");
    is($s.output.shift, 13, "Output value");
}, "run 3,0,4,0,99 until output";

subtest {
    my $s = SystemI.new(:mem([3,0,4,0,99]));
    $s.input = 13;
    is(run-full($s).output, 13, "full run");
},"run 3,0,4,0,99 full";

subtest {
    my @prog = [3,21,1008,21,8,20,1005,20,22,107,8,21,
                20,1006,20,31, 1106,0,36,98,0,0,1002,21,
                125,20,4,20,1105,1,46,104, 999,1105,1,46,
                1101,1000,1,20,4,20,1105,1,46,98,99];
    my $s1 = SystemI.new(:mem(@prog));
    my $s2 = SystemI.new(:mem(@prog));
    my $s3 = SystemI.new(:mem(@prog));
    is(run-until-event($s1, 7), Output, "lt Event type");
    is(run-until-event($s2, 8), Output, "eq Event type");
    is(run-until-event($s3, 9), Output, "gt Event type");
    is($s1.output.shift, 999, "lt output");
    is($s2.output.shift, 1000, "eq output");
    is($s3.output.shift, 1001, "gt output");
}, "given example prog: cmp w.r.t 8";

done-testing;
