unit module Intcode;

enum OpType is export <Add Mul Hlt>;

my constant %opcodes = 1 => Add,
                       2 => Mul,
                       99 => Hlt;

my constant %opargs = Add => 3,
                      Mul => 3,
                      Hlt => 0;

my constant %ops = Add => &infix:<+>,
                   Mul => &infix:<*>,
                   Hlt => sub {'exit'}; # is this necessary

enum SystemState is export <Running Halted>;

class SystemI is export {
    has Int @!mem;
    has Int $!ip = 0;
    has SystemState $.state is rw = Running;
    has Bool $.trace is rw = False;
    submethod BUILD(:@mem) {
        @!mem = @mem;
    }

    method !eval_set_bin(&fn, $in_v1, $in_v2, $out_p) {
        @!mem[$out_p] = &fn($in_v1, $in_v2);
    }

    method step() {
        my $current_op = %opcodes{@!mem[$!ip]}; # XXX: what happens if this look up fails?
        my Int $num_args = %opargs{$current_op};
        given $num_args {
            when 0 {
                given ($current_op) {
                    when Hlt { $.state = Halted;}
                }
            }
            when 3 {
                my $in_p1 = @!mem[$!ip+1];
                my $in_v1 = @!mem[$in_p1];
                my $in_p2 = @!mem[$!ip+2];
                my $in_v2 = @!mem[$in_p2];
                my $out_p = @!mem[$!ip+3];
                self!eval_set_bin(%ops{$current_op},
                                  $in_v1, $in_v2, $out_p);
            }
        }
        $!ip = $!ip + $num_args + 1;
    }

     # only use for testing
    multi method get_mem() { return @!mem;}
    multi method get_mem(Int $i) { return @!mem[$i];}
}

sub run-full(SystemI $s) is export {
    until ($s.state == Halted) {
        $s.step();
    }
    return $s;
}
