#!/usr/bin/env kast
module:
use (import "../util.ks").*;
use std.*;

# set_do_log(true);

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
            let check_with_removing = remove_first => (
                log_print "check";
                log_dbg mult;
                log_dbg remove_first;
                let mut answer = true;
                let mut prev :: Option[int32] = :None;
                let mut prev2 :: Option[int32] = :None;
                let mut removed = false;
                for x :: int32 in list_iter report {
                    log_print "item";
                    log_dbg x;
                    let x = x * mult;
                    match prev {
                        | :None => ()
                        | :Some prev => (
                            log_dbg prev;
                            let diff = x - prev;
                            log_dbg diff;
                            if diff < 1 or diff > 3 then (
                                log_print "incorrent diff";
                                if not removed then (
                                    removed = true;
                                    log_print "not removed";
                                    if not remove_first then (
                                        continue;
                                    );
                                    log_print "removed";
                                    log_dbg x;
                                    if prev2 is :Some prev2 then (
                                        let diff = x - prev2;
                                        if diff < 1 or diff > 3 then (
                                            answer = false;
                                        );
                                    );
                                ) else (
                                    answer = false; # TODO return false
                                );
                            );
                            prev2 = :Some prev;
                        )
                    };
                    prev = :Some x;
                };
                log_print "=";
                log_dbg answer;
                answer
            );
            check_with_removing true or check_with_removing false
        );
        if check(-1) or check(+1) then (
            # print "correct";
            answer = answer + 1;
        );
    };
    answer
};

if exec_mode() is :Run then (
    let answer = solve "day02/input.txt";
    print "== ANSWER ==";
    dbg answer;
);

