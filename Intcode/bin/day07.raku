use lib '../lib';
use Intcode;
use Test;

my constant $namps = 5;
sub run-amplifier(Int @vals, Int @phase-seq, Bool $feeback = False) {
    my SystemI @amp[$namps];
    for ^$namps -> $n { # Initialize
        @amp[$n] = SystemI.new(:mem(@vals));
        @amp[$n].input = @phase-seq[$n];
    }
    
    my $input = 0;
    EXIT: repeat {
        for ^$namps -> $n {
            @amp[$n].input.push($input);
            my $e = run-until-event(@amp[$n]);
            last EXIT if $e == Exit;
            die "Expected Output event, got: " ~ $e unless $e == Output;
            $input = @amp[$n].output.shift;
        }
    } while ($feeback);

    return $input;
    
}

multi sub MAIN($filename) {
    my $str = $filename.IO.slurp.chomp;
    my Int @vals = $str.split(",").map(&val);
    #part 1
    say (^5).permutations
    .map(-> @ps {run-amplifier(@vals, Array[Int].new(@ps))})
    .reduce(&infix:<max>);
    #part 2
    say (5..9).permutations
    .map(-> @ps {run-amplifier(@vals, Array[Int].new(@ps), True)})
    .reduce(&infix:<max>);
}

multi sub MAIN() { # testing
    subtest {
        my Int @vals = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0];
        my Int @phase-seq = [4,3,2,1,0];
        is(run-amplifier(@vals,@phase-seq), 43210, "Max signal");
    }, "Example 1";
    subtest {
        my Int @vals = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,
                        23,23,1,24,23,23,4,23,99,0,0];
        my Int @phase-seq = [0,1,2,3,4];
        is(run-amplifier(@vals,@phase-seq), 54321, "Max signal");
    }, "Example 2";
    subtest {
        my Int @vals = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,
                        0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,
                        31,99,0,0,0];
        my Int @phase-seq = [1,0,4,3,2];
        is(run-amplifier(@vals,@phase-seq), 65210, "Max signal");
    }, "Example 3";

    # PART 2
    subtest {
        my Int @vals = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                        27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5];
        my Int @phase-seq = [9,8,7,6,5];
        is(run-amplifier(@vals, @phase-seq, True), 139629729, "Max signal");
    }, "Part2 example 1";

    subtest {
        my Int @vals =
        [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
         -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
         53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10];
        my Int @phase-seq = [9,7,8,5,6];
        is(run-amplifier(@vals, @phase-seq, True), 18216, "Max signal");        
    }, "Part2 example 2";
   
}
