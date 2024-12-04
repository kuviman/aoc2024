#!/usr/bin/env kast
module:
use (import "../util.ks").*;
use std.*;

# set_do_log(true);

let solve = fn(path) {
    let input :: string = read_file path;
    let input :: list[char] = (
        let mut s = list[];
        for c :: char in chars(input) {
            s = list_push(s, c);
        };
        s
    );
    let mut pointer :: int32 = 0;
    let peek = () => (
        list_get (input, pointer)
    );
    const failing :: type = (
        .stop = () -> () with (),
    );
    let read_specific = fn (c :: char) with failing {
        if peek() == c then (
            pointer += 1;
        ) else (
            (current failing).stop ();
        )
    };
    let to_digit = fn (c :: char) -> Option[int32] {
        if '0' <= c and c <= '9' then (
            :Some (char_ord(c) - char_ord('0'))
        ) else (
            :None
        )
    };
    let read_number = fn (()) with failing {
        let mut number = 0;
        let mut digits :: int32 = 0;
        while digits < 3 {
            match to_digit <| peek () {
                | :Some digit => (
                    number = number * 10 + digit;
                    digits += 1;
                    pointer += 1;
                )
                | :None => (
                    break;
                )
            }
        };
        if digits == 0 then (
            (current failing).stop ();
        );
        number
    };
    let try_read = forall[T] {
        fn(f :: () -> T with failing) -> Option[T] {
            let old_pointer = pointer;
            unwindable read_block (
                let handler :: failing = (
                    .stop = () => (
                        pointer = old_pointer;
                        unwind read_block (:None)
                    ),
                );
                with handler;
                let value = f();
                :Some value
            )
        }
    };
    let read_string = fn(s :: string) -> () with failing {
        for c in chars(s) {
            read_specific c;
        };
    };
    let read_mul = fn(()) -> (int32, int32) with failing {
        read_string "mul(";
        let a = read_number ();
        read_specific ',';
        let b = read_number ();
        read_specific ')';
        a, b
    };
    let input_len = list_length input;
    let mut do_mul = true;
    let mut answer = 0;
    while pointer < input_len {
        dbg pointer;
        if try_read read_mul is :Some (a, b) then (
            if do_mul then (
                answer += a * b;
            );
            continue;
        );
        if try_read (() => read_string "do()") is :Some () then (
            do_mul = true;
            continue;
        );
        if try_read (() => read_string "don't()") is :Some () then (
            do_mul = false;
            continue;
        );
        pointer += 1;
    };
    answer
};

if exec_mode() is :Run then (
    let answer = solve "day03/input.txt";
    print "== ANSWER ==";
    dbg answer;
);

