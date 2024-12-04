#!/usr/bin/env kast
module:
use (import "../util.ks").*;
use std.*;

# set_do_log(true);

let solve = fn(path) {
    let input :: string = read_file path;
    let input :: list[list[char] :: type] = (
        let mut rows = list[];
        for line in lines(input) {
            let mut col = list[];
            for c :: char in chars(line) {
                col = list_push(col, c);
            };
            rows = list_push(rows, col);
        };
        rows
    );
    let width = list_length input;
    let height = list_length (list_get (input, 0));

    const vec2 :: type = (int32, int32);
    let vec2_add = fn(a :: vec2, b :: vec2) -> vec2 {
        let ax, ay = a;
        let bx, by = b;
        (ax + bx, ay + by)
    };

    let find_at = fn(pos :: vec2, dir :: vec2) -> bool {
        let mut found = true;
        let mut pos = pos;
        for c :: char in chars "XMAS" {
            let (x, y) = pos;
            if x < 0 or x >= width or y < 0 or y >= height then (
                found = false; # TODO return
                break;
            );
            if list_get (list_get (input, x), y) != c then (
                found = false;
                break;
            );
            pos = vec2_add(pos, dir);
        };
        found
    };

    let mut answer :: int32 = 0;
    for x :: int32 in range(0, width) {
        dbg (x, width);
        for y :: int32 in range(0, height) {
            for dx :: int32 in range(-1, 2) {
                for dy :: int32 in range(-1, 2) {
                    if dx == 0 and dy == 0 then ( continue );
                    if find_at((x, y), (dx, dy)) then (
                        answer += 1;
                    );
                }
            }
        }
    };
    answer
};

if exec_mode() is :Run then (
    let answer = solve "day04/input.txt";
    print "== ANSWER ==";
    dbg answer;
);

