grammar Input 
{
    rule TOP { <monkey>+ }
    rule monkey { 
        "Monkey" <id> ":"
        "Starting items:" <items>
        "Operation: new =" <operation>
        "Test: divisible by" <test> 
          "If true: throw to monkey" <other-monkey>
          "If false: throw to monkey" <other-monkey>
    }
    
    token id { \d+ }

    rule items { <single-item>+ }
    rule single-item { \d+ ","? }
    
    rule operation { <optoken> <operator> <optoken> }
    token optoken { "old" | \d+ }
    token operator { "*" | "+" }

    token test { \d+ }

    token other-monkey { \d+ }
}

class Input-actions
{
    method items($/) {
        make $/.split(',').map(*.trim).map({val($_)});
    }

    method operation($/) {
        make -> $old {
            my $get = -> $token { $token eq "old" ?? $old !! $token };
            my $apply = -> $a, $op, $b { $op eq '*' ?? $a * $b !! $a + $b };
            $apply($get($/<optoken>[0]), $/<operator>, $get($/<optoken>[1]));
        }
    }
}

class Monkey 
{
    has Int() $.id;
    has Int @.items;
    has &.operation;
    has Int() $.test-value;
    has Int() $.true-id;
    has Int() $.false-id;

    has Int $!inspections = 0;

    method inspect(@monkeys, &worry-adjustment) {
        for @.items -> $item {
            my $new-level = &worry-adjustment.(&.operation.($item));
            my $target = $new-level %% $.test-value ?? $.true-id !! $.false-id;
            @monkeys[$target].receive($new-level);
            ++$!inspections;
        }
        @.items = [];
    }

    method receive($item) {
        @.items.push($item);
    }

    method inspection-count() { $!inspections }
}

sub get-monkeys($filename) {
    my $contents = $filename.IO.slurp;
    my $data = Input.parse($contents, actions => Input-actions.new);

    my @monkeys = $data<monkey>.map(-> $raw {
        Monkey.new(id => $raw<id>,
                   items => $raw<items>.made,
                   operation => $raw<operation>.made,
                   test-value => $raw<test>,
                   true-id => $raw<other-monkey>[0],
                   false-id => $raw<other-monkey>[1]);
    });

    return @monkeys;
}

sub get-monkey-business(@monkeys, $inspections, &worry-adjustment) {
    for ^$inspections {
        for @monkeys -> $monkey {
            $monkey.inspect(@monkeys, &worry-adjustment);
        }
    }
    
    return @monkeys.map(*.inspection-count).sort({ $^b <=> $^a })[0..1].reduce(&infix:<*>);
}

sub part1($filename) {
    my @monkeys = get-monkeys($filename);
    say get-monkey-business(@monkeys, 20, {$_ div 3});
}

sub part2($filename) {
    my @monkeys = get-monkeys($filename);
    my $ceiling = @monkeys.map({$_.test-value}).reduce(&infix:<*>);
    say get-monkey-business(@monkeys, 10000, {$_ % $ceiling});
}

sub MAIN($filename = 'input') {
    part1($filename);
    part2($filename);
}
