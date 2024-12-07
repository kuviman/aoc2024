#!/usr/bin/env kast
module:
use (import "../util.ks").*;
use std.*;

# Should i really participate in "Advent of Cheaters"?
# I wish I had just used rust...
syntax mapM <- 15 = expr "$>" f;
impl syntax mapM = macro (.expr, .f) => `((
    let e = () => $expr;
    $f e
));

syntax let_rec <- 4 = "let" "rec" name "=" value;
impl syntax let_rec = macro (.name, .value) => `(
    let $name = rec( let $name = $value ). $name
);

let collect = forall[T] {
    fn (gen :: () -> () with generator_handler[T]) -> list[T] {
        let mut result = list[];
        for item :: T in gen() {
            result = list_push(result, item);
        };
        result
    }
};

const map = forall[T, U] {
    fn (f :: T -> U) -> _ {
        fn (gen :: () -> () with generator_handler[T]) -> () with generator_handler[U] {
            let outer = current generator_handler[U];
            for item :: T in gen() {
                let mapped = f item;
                outer.handle mapped;
            };
        }
    }
};

const enumerate = forall[T] {
    fn (gen :: () -> ()) -> () with generator_handler[int32, T] {
        let outer = current generator_handler[int32, T];
        let mut i = 0;
        for item :: T in gen() {
            outer.handle (i, item);
            i += 1;
        };
    }
};

const Equation :: type = (.test = int64, .numbers = list[int64]);
const Input :: type = list[Equation];

let parse_equation = fn(s :: string) -> Equation {
    let parts :: list[string] = split_by(s, c => c == ':', .yield_empty = false) $> collect;
    let test = parse (list_get (parts, 0));
    let numbers :: list[int64] = list_get (parts, 1)
        |> split_whitespace
        $> map[_, _] parse[int64]
        $> collect;
    (.test, .numbers)
};

let read_input = fn(path) -> Input {
    let input = read_file path;
    let mut result = list[];
    for line in lines(input) {
        result = list_push (result, parse_equation line);
    };
    result
};

let concat = fn(a :: int64, b :: int64) -> int64 {
    let mut d = b;
    while d > 0 {
        d /= 10;
        a *= 10;
    };
    a + b
};

let strip_suffix = fn(mut a :: int64, mut suffix :: int64) -> Option[int64] {
    unwindable result (
        while suffix > 0 {
            let last_digit = suffix % 10;
            if a % 10 != last_digit then ( unwind result (:None) );
            a /= 10;
            suffix /= 10;
        };
        :Some a
    )
};

let potentially_true = fn(eq :: Equation) -> bool {
    unwindable result (
        let brute = rec (
            let brute = fn(value :: int64, i :: int32) {
                unwindable f (
                    let cur = list_get (eq.numbers, i);
                    if i == 0 then (
                        if value == cur then (
                            unwind result true;
                        );
                        unwind f ();
                    );
                    if value > cur then brute(value - cur, i - 1);
                    if value % cur == 0 then brute(value / cur, i - 1);
                    if strip_suffix (value, cur) is :Some prefix then brute(prefix, i - 1);
                )
            };
        ).brute;
        brute (eq.test, list_length eq.numbers - 1);
        false
    )
};

let solve = fn(path) {
    let input = read_input path;
    print "input read";
    let input = sort_by (input, (a :: Equation, b :: Equation) => (if a.test < b.test then (-1) else (+1)));
    print "sorted";
    let mut answer = 0;
    for i :: int32, eq :: Equation in list_iter input $> enumerate[Equation] {
        # print format!"$i / ${list_len input}";
        dbg i;
        if potentially_true eq then (
            answer += eq.test;
        );
    };
    answer
};

if exec_mode() is :Run then (
    let answer = solve "day07/example.txt";
    print "== ANSWER ==";
    dbg answer;
);

