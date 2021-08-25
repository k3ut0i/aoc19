# -*- mode: raku -*-
use Test;
use lib 'lib';
use lib '../lib';

my $num_tests = 10;
plan $num_tests;

use-ok "Intcode";
use Intcode; # flycheck cries if I do not do this.

my $s = SystemI.new(:mem([1, 0, 0, 3, 99]));
is($s.^name, 'Intcode::SystemI', "SystemI initialization");

$s.step(); # first step
is($s.state, Running, "step1: state");
is($s.get_mem(), [1, 0, 0, 2, 99], "step1: memory");

$s.step(); # second step
is($s.state, Halted, "step2: state");
is($s.get_mem(), [1, 0, 0, 2, 99], "step2: memory");

subtest {
    my $s = SystemI.new(:mem([2,3,0,3,99]));
    is(run-full($s).get_mem(), [2, 3, 0, 6, 99], "mem final");
}, "System: 2,3,0,3,99";

subtest {
    my $s = SystemI.new(:mem([2,4,4,5,99,0]));
    is(run-full($s).get_mem(), [2,4,4,5,99,9801], "mem final");
}, "System: 2,4,4,5,99,0";

subtest {
    my $s = SystemI.new(:mem([1,1,1,4,99,5,6,0,99]));
    is(run-full($s).get_mem(), [30,1,1,4,2,5,6,0,99], "mem final");
}, "System: 1,1,1,4,99,5,6,0,99";

subtest {
    my $s = SystemI.new(:mem([1,9,10,3,2,3,11,0,99,30,40,50]));
    is(run-full($s).get_mem(), [3500,9,10,70,2,3,11,0,99,30,40,50], "mem final");
}, "full example";

done-testing();
