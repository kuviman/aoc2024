#!/usr/bin/env kast
module:
use (import "../util.ks").*;
use std.*;

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

/*const enumerate = forall[T] {
    fn (gen :: () -> () with generator_handler[T]) -> () with generator_handler[int32, T] {
        let outer = current generator_handler[int32, T];
        let mut i = 0;
        for item :: T in gen() {
            outer.handle (i, item);
            i += 1;
        };
    }
};*/

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

let potentially_true = fn(eq :: Equation) -> bool {
    unwindable result (
        let brute = rec (
            let brute = fn(value :: int64, i :: int32) {
                unwindable f (
                    if value > eq.test then (
                        unwind f ();
                    );
                    if i == list_length eq.numbers then (
                        if value == eq.test then (
                            unwind result true;
                        );
                        unwind f ();
                    );
                    let cur = list_get (eq.numbers, i);
                    brute(value + cur, i + 1);
                    brute(value * cur, i + 1);
                )
            };
        ).brute;
        brute (list_get (eq.numbers, 0), 1);
        false
    )
};

let solve = fn(path) {
    let input = read_input path;
    let mut answer = 0;
    let mut i :: int32 = 0;
    for eq :: Equation in list_iter input {
        dbg i;
        if potentially_true eq then (
            answer += eq.test;
        );
        i += 1;
    };
    answer
};

if exec_mode() is :Run then (
    let answer = solve "day07/input.txt";
    print "== ANSWER ==";
    dbg answer;
);

