unit module Intcode;

enum OpType is export <Add Mul StoreIn WriteOut JiT JiF Lt Eq Hlt>;
my constant %opcodes = 1 => Add,
                       2 => Mul,
                       3 => StoreIn,
                       4 => WriteOut,
                       5 => JiT,
                       6 => JiF,
                       7 => Lt,
                       8 => Eq,
                       99 => Hlt;

my constant %opargs = Add => 3,
                      Mul => 3,
                      StoreIn => 1,
                      WriteOut => 1,
                      JiT => 2,
                      JiF => 2,
                      Lt => 3,
                      Eq => 3,
                      Hlt => 0;

sub lt(Int $v1, Int $v2 --> Int) {
    $v1 < $v2 ?? 1 !! 0;
}

sub eq(Int $v1, Int $v2 --> Int) {
    $v1 == $v2 ?? 1 !! 0;
}

my constant %ops = Add => &infix:<+>,
                   Mul => &infix:<*>,
                   Lt => &lt,
                   Eq => &eq;

enum SystemState is export <Running Halted>;

enum PointerType is export(:testing) <Direct Link>;
class Pointer {
    has Int $.val;
    has PointerType $.type;
}

sub parse-opcode(Int $o --> Map) is export(:testing) { 
    my Int @digits = $o.polymod(100, 10, 10); # assuming max args: 3
    my OpType $op = %opcodes{@digits.shift};
    unless $op.defined { die "Could not parse: " ~ $o};
    my @args = @digits.map: -> $d { $d == 0 ?? Link !! Direct}; 
    return Map.new('type' => $op,
                   'args' => @args);
}

class SystemI is export {
    has Int @!mem;
    has Int $!ip = 0; # always a direct pointer
     # should input and output have not-available states?
    has Int $.input is rw = Int; # Indefinite is unavailable for now
    has Int $.output is rw = Int;

    has SystemState $.state is rw = Running;
    has Bool $.trace is rw = False;

    method get-ip() {return $!ip} # only use for testing
    multi method get-mem() { return @!mem;} # only use for testing
    multi method get-mem(Int $i) { return @!mem[$i];}

    method !set-mem(Int $idx, Int $val) { @!mem[$idx] = $val; }
    submethod BUILD(:@mem) { @!mem = @mem; }

    method !eval_set_bin(&fn, $in_v1, $in_v2, $out_p) {
        self!set-mem($out_p, &fn($in_v1, $in_v2));
    }

    method !get-arg(Int $p, PointerType $type) {
        given $type {
            when Direct { $p }
            when Link {@!mem[$p]}
        }
    }

    method step() {
        my $op = parse-opcode @!mem[$!ip];
        my $op-type = $op{"type"};
        my @arg-types = $op{"args"};
        given $op-type {
            when Hlt { $.state = Halted;}
            when StoreIn {
                my $store-add = @!mem[$!ip+1];
                self!set-mem($store-add, $.input);
            }
            when WriteOut {
                $.output = self!get-arg(@!mem[$!ip+1], @arg-types[0]);
            }
            when Add | Mul | Lt | Eq {
                my $in_v1 = self!get-arg(@!mem[$!ip+1], @arg-types[0]);
                my $in_v2 = self!get-arg(@!mem[$!ip+2], @arg-types[1]);
                my $out_p = @!mem[$!ip+3];
                self!eval_set_bin(%ops{$op-type},
                                  $in_v1, $in_v2, $out_p);
            }
            when JiT | JiF {
                my $req = ($op-type == JiT) ?? (-> $a { $a != 0}) !! (-> $a { $a == 0});
                my $arg1 = self!get-arg(@!mem[$!ip+1], @arg-types[0]);
                my $arg2 = self!get-arg(@!mem[$!ip+2], @arg-types[1]);
                if $req($arg1) {
                    $!ip = $arg2;
                    return $op-type;
                }
            }
        }
        # does not default when JiT, JiF fire
        $!ip = $!ip + %opargs{$op-type} + 1;
        return $op-type;
    }
}

sub run-full(SystemI $s) is export {
    until ($s.state == Halted) {
        $s.step();
    }
    return $s;
}

sub run-until-output(SystemI $s, Int $input?) is export {
    if $input.defined {$s.input = $input};
    until ($s.state == Halted) {
        last if $s.step() == WriteOut;
    }
    return $s;
}
