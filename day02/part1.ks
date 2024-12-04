#!/usr/bin/env kast
module:
use (import "../util.ks").*;
use std.*;

let solve = fn(path) {
    let input = read_file path;
    let mut line_index :: int32 = 0;
    let mut answer :: int32 = 0;
    for line :: string in lines(input) {
        dbg line_index;
        line_index = line_index + 1;
        let mut report :: list[int32] = list[];
        for x :: string in split_whitespace(line) {
            report = list_push(report, parse x);
        };
        let check = mult => (
            let mut answer = true;
            let mut prev :: Option[int32] = :None;
            for x :: int32 in list_iter report {
                let x = x * mult;
                match prev {
                    | :None => ()
                    | :Some prev => (
                        let diff = x - prev;
                        if diff < 1 or diff > 3 then (
                            answer = false; # TODO return false
                        );
                    )
                };
                prev = :Some x;
            };
            answer
        );
        if check(-1) or check(+1) then (
            answer = answer + 1;
        );
    };
    answer
};

if exec_mode() is :Run then (
    let answer = solve "day02/input.txt";
    dbg answer;
);

