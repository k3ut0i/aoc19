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

is(run-until-output(SystemI.new(:mem([3,0,4,0,99])), 13).output, 13,
   "run 3,0,4,0,99 until output");

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
    is(run-until-output(SystemI.new(:mem(@prog)), 7).output, 999, "lt");
    is(run-until-output(SystemI.new(:mem(@prog)), 8).output, 1000, "eq");
    is(run-until-output(SystemI.new(:mem(@prog)), 9).output, 1001, "gt");
}, "given example prog: cmp w.r.t 8";

done-testing;
