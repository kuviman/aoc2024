#!/usr/bin/env kast
module:
use (import "../util.ks").*;
use std.*;

syntax method_call <- 200 = obj ".>" method arg;
impl syntax method_call = macro (.obj, .method, .arg) => `(
    $method ($obj, $arg)
);

# set_do_log(true);

const vec2 :: type = (.x = int32, .y = int32);
const dirs :: list[vec2] = list[
    (.x = -1, .y = 0),
    (.x = 0, .y = 1),
    (.x = 1, .y = 0),
    (.x = 0, .y = -1),
];

let read_input = fn(path) {
    let input = read_file path;
    let mut pos :: Option[vec2] = :None;
    let mut row = 0;
    let mut map = list[];
    for line :: string in lines(input) {
        let mut col = 0;
        let mut map_row = list[];
        for c :: char in chars(line) {
            if c == '^' then (pos = :Some (.x = row, .y = col));
            map_row = list_push(map_row, c == '#');
            col += 1;
        };
        map = list_push(map, map_row);
        row += 1;
    };
    let :Some pos = pos;
    map, pos
};

let solve = fn(path) {
    let (map, mut pos) = read_input path;
    print "read input completed";
    let n = list_length map;
    if list_length (list_get (map, 0)) != n then (
        panic "isnt map supposed to be square?";
    );

    let blocked = fn(cell :: vec2) -> bool {
        map .> list_get cell.x .> list_get cell.y
    };

    let mut visited = list[];
    for i :: int32 in range(0, n) {
        let mut row = list[];
        for j :: int32 in range(0, n) {
            let mut a = list[];
            for dir :: int32 in range(0, 4) {
                a = list_push(a, false);
            };
            row = list_push(row, a);
        };
        visited = list_push(visited, row);
    };

    let mut visited_cells = list[];
    for i :: int32 in range(0, n) {
        let mut row = list[];
        for j :: int32 in range(0, n) {
            row = list_push(row, false);
        };
        visited_cells = list_push(visited_cells, row);
    };

    const Pos :: type = (.cell = vec2, .dir = int32);

    let mut pos :: Pos = (.cell = pos, .dir = 0);

    let go = fn(cell :: vec2, dir :: int32) -> vec2 {
        let dir = list_get (dirs, dir);
        (.x = cell.x + dir.x, .y = cell.y + dir.y)
    };

    print "starting to move";
    let mut answer :: int32 = 0;
    let mut step :: int32 = 0;
    loop {
        dbg step;
        step += 1;
        # dbg pos;
        if visited .> list_get pos.cell.x .> list_get pos.cell.y .> list_get pos.dir then (
            break;
        );
        if not (visited_cells .> list_get pos.cell.x .> list_get pos.cell.y) then (
            answer += 1;
            visited_cells = list_set (visited_cells, pos.cell.x, list_set(visited_cells .> list_get pos.cell.x, pos.cell.y, true));
        );
        visited = list_set (visited, pos.cell.x,
            list_set(visited .> list_get pos.cell.x, pos.cell.y,
                list_set(visited .> list_get pos.cell.x .> list_get pos.cell.y, pos.dir, true)
            )
        );
        let front = go (pos.cell, pos.dir);
        if not (0 <= front.x and front.x < n and 0 <= front.y and front.y < n) then (
            break;
        );
        if blocked(front) then (
            pos = (.cell = pos.cell, .dir = (pos.dir + 1) % 4);
        ) else (
            pos = (.cell = front, .dir = pos.dir);
        );
    };
    answer
};

if exec_mode() is :Run then (
    let answer = solve "day06/input.txt";
    print "== ANSWER ==";
    dbg answer;
);

