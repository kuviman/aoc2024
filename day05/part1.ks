#!/usr/bin/env kast
module:
use (import "../util.ks").*;
use std.*;

# set_do_log(true);

let read_input = fn(path) {
    let input :: string = read_file path;
    let mut read_rules = false;
    let mut rules = list[];
    let mut updates = list[];
    let mut line_index :: int32 = 0;
    for line :: string in lines(input) {
        dbg line_index;
        line_index += 1;
        if line == "" then (
            read_rules = true;
            continue;
        );
        if read_rules then (
            let mut update :: list[int32] = list[];
            for part :: string in split_by (line, c => c == ',', false) {
                update = list_push(update, parse part);
            };
            updates = list_push(updates, update);
            # dbg update;
        ) else (
            let mut parts :: list[int32] = list[];
            for part :: string in split_by (line, c => c == '|', false) {
                parts = list_push(parts, parse part);
            };
            let a, b = list_get (parts, 0), list_get (parts, 1);
            rules = list_push(rules, (a, b));
            # dbg (a, b);
        );
    };
    print "input read done";
    dbg (list_length rules);
    dbg (list_length updates);
    rules, updates
};

let solve = fn(path) {
    let rules, updates = read_input path;

    let rules = sort[int32, int32] rules;

    let mut answer = 0;
    let mut index :: int32 = 0;
    for update :: list[int32] in list_iter updates {
        dbg index;
        index += 1;
        let n = list_length update;
        let is_correct = unwindable is_correct (
            for i :: int32 in range(0, n) {
                for j :: int32 in range(i + 1, n) {
                    let a = list_get (update, i);
                    let b = list_get (update, j);
                    if binary_search (rules, (b, a)) then (
                        unwind is_correct false;
                    );
                }
            };
            true
        );
        if is_correct then (
            answer += list_get (update, n / 2);
        );
    };
    answer
};

if exec_mode() is :Run then (
    let answer = solve "day05/input.txt";
    print "== ANSWER ==";
    dbg answer;
);

