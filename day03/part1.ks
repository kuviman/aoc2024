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
    let read_mul = fn(()) -> Option[int32, int32] {
        let old_pointer = pointer;
        unwindable read_block (
            let handler :: failing = (
                .stop = () => (
                    pointer = old_pointer;
                    unwind read_block (:None)
                ),
            );
            with handler;
            read_specific 'm';
            read_specific 'u';
            read_specific 'l';
            read_specific '(';
            let a = read_number ();
            read_specific ',';
            let b = read_number ();
            read_specific ')';
            :Some (a, b)
        )
    };
    let input_len = list_length input;
    let mut answer = 0;
    while pointer < input_len {
        dbg pointer;
        match read_mul() {
            | :Some (a, b) => (
                answer += a * b;
            )
            | :None => (
                pointer += 1;
            )
        };
    };
    answer
};

if exec_mode() is :Run then (
    let answer = solve "day03/input.txt";
    print "== ANSWER ==";
    dbg answer;
);

