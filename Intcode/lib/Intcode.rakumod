unit module Intcode;

enum OpType is export <Add Mul StoreIn WriteOut JiT JiF Lt Eq BaseAdjust Hlt>;
my constant %opcodes = 1 => Add,
                       2 => Mul,
                       3 => StoreIn,
                       4 => WriteOut,
                       5 => JiT,
                       6 => JiF,
                       7 => Lt,
                       8 => Eq,
                       9 => BaseAdjust,
                       99 => Hlt;

my constant %opargs = Add => 3,
                      Mul => 3,
                      StoreIn => 1,
                      WriteOut => 1,
                      JiT => 2,
                      JiF => 2,
                      Lt => 3,
                      Eq => 3,
                      BaseAdjust => 1,
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

enum SystemState is export <Running InputRequired InputConsumed OutputGenerated Halted>;

enum PointerType is export(:testing) <Direct Link Relative>;

sub parse-opcode(Int $o --> Map) is export(:testing) {
    my Int @digits = $o.polymod(100, 10, 10); # assuming max args: 3
    my OpType $op = %opcodes{@digits.shift};
    unless $op.defined { die "Could not parse: " ~ $o};
    my @args = @digits.map:
               -> $d { given $d {
                             when 0 {Link}
                             when 1 {Direct}
                             when 2 {Relative}}}
    return Map.new('type' => $op,
                   'args' => @args);
}

class SystemI is export {
    has Int @!mem;
    has Int $!ip = 0; # always a direct pointer
    has Int $!bp = 0; # (relative) base pointer
    has Int @.input; #both are queues
    has Int @.output;

    has SystemState $.state is rw = Running;
    has Bool $.trace is rw = False;

    method get-ip() {return $!ip} # only use for testing
    multi method get-mem() { return @!mem;} # only use for testing
    multi method get-mem(Int $i) {
        if $i < @!mem.elems {
            return @!mem[$i];
        } else { # Increase memory size
            @!mem[$i] = 0;
            return 0;
        }
    }

    method !set-mem(Int $idx, Int $val) { @!mem[$idx] = $val; }
    submethod BUILD(:@mem) { @!mem = @mem; }

    method !eval_set_bin(&fn, $in_v1, $in_v2, $out_p) {
        self!set-mem($out_p, &fn($in_v1, $in_v2));
    }

    method !get-add(Int $pointer-address, PointerType $type) {
        given $type {
            when Link {self.get-mem($pointer-address)}
            when Relative {$!bp+self.get-mem($pointer-address)}
            when Direct {die "Tried to deref Direct val"}
        }
    }
    method !get-arg(Int $p, PointerType $type) {
        given $type {
            when Direct { $p }
            when Link {self.get-mem($p)};
            when Relative {self.get-mem($!bp+$p)};
        }
    }

    method step() {
        my $op = parse-opcode @!mem[$!ip]; # this should never have to result in default
        my $op-type = $op{"type"};
        my @arg-types = $op{"args"};
        given $op-type {
            when Hlt { $.state = Halted;}
            when StoreIn {
                if @.input { # input queue has elements
                    my $store-add =
                    @arg-types[0] == Link ?? self.get-mem($!ip+1) !! $!bp + self.get-mem($!ip+1);
                    my $val = @.input.shift;
                    self!set-mem($store-add, $val);
                    $.state = InputConsumed;
                } else { # input queue is empty
                    $.state = InputRequired;
                    return InputRequired; # do not advance in @.input has no elements
                }
            }
            when WriteOut {
                my $val = self!get-arg(self.get-mem($!ip+1), @arg-types[0]);
                @.output.push($val);
                $.state = OutputGenerated;
            }
            when Add | Mul | Lt | Eq {
                my $in_v1 = self!get-arg(self.get-mem($!ip+1), @arg-types[0]);
                my $in_v2 = self!get-arg(self.get-mem($!ip+2), @arg-types[1]);
                my $out_p =
                    @arg-types[2] == Link ?? self.get-mem($!ip+3) !! $!bp + self.get-mem($!ip+3);
                self!eval_set_bin(%ops{$op-type},
                                  $in_v1, $in_v2, $out_p);
                $.state = Running;
            }
            when JiT | JiF {
                $.state = Running;
                my $req = ($op-type == JiT) ?? (-> $a { $a != 0}) !! (-> $a { $a == 0});
                my $arg1 = self!get-arg(self.get-mem($!ip+1), @arg-types[0]);
                my $arg2 = self!get-arg(self.get-mem($!ip+2), @arg-types[1]);
                if $req($arg1) {
                    $!ip = $arg2;
                    return $op-type;
                }
            }
            when BaseAdjust {
                $!bp += self!get-arg(self.get-mem($!ip+1), @arg-types[0]);
                $.state = Running;
            }
        }
        # does not default when JiT, JiF fire
        # OR when @.input needs elements
        $!ip = $!ip + %opargs{$op-type} + 1;
        return $op-type;
    }
}

sub run-full(SystemI $s) is export {
    until ($s.state == Halted | InputRequired) {
        $s.step();
    }
    if ($s.state == Halted) {
        return $s;
    } else {
        die "Input required: could not complete full run";
    }
}

enum SystemEvent is export <Exit Input Output>;

sub run-until-event(SystemI $s, Int $input? --> SystemEvent) is export {
    if $input.defined {$s.input.push($input)};

    repeat {
        $s.step();
    } while ($s.state == Running | InputConsumed);

    given $s.state {
        when Halted { return Exit }
        when InputRequired {return Input}
        when OutputGenerated {return Output}
    }
}
