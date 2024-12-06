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

let init_list = forall[T] {
    fn(n :: int32, elem :: T) -> list[T] {
        let mut result = list[];
        for _ :: int32 in range(0, n) {
            result = list_push(result, elem);
        };
        result
    }
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

    let go = fn(cell :: vec2, dir :: int32) -> vec2 {
        let dir = list_get (dirs, dir);
        (.x = cell.x + dir.x, .y = cell.y + dir.y)
    };

    print "calculating long jumps";
    let mut found = init_list(n, init_list(n, false));
    let mut visited = init_list(n, init_list(n, init_list(4, false)));
    let mut long_jumps = init_list(n, init_list(n, init_list(4, -1 :: int32)));
    (for x :: int32 in range(0, n) {
        dbg x;
        for dir :: int32 in range(0, 4) {
            let opposite_dir = (dir + 2) % 4;
            let start = d => (
                if d == 0 then x else ( if d < 0 then (n - 1) else 0 )
            );
            let dir_vec :: vec2 = list_get (dirs, dir);
            let mut pos :: vec2 = (.x = start(dir_vec.x), .y = start(dir_vec.y));
            let mut distance = 0;
            loop {
                distance += 1;
                if blocked(pos) then (
                    distance = 0;
                );
                long_jumps = list_set (long_jumps, pos.x,
                    list_set(long_jumps .> list_get pos.x, pos.y,
                        list_set(long_jumps .> list_get pos.x .> list_get pos.y, opposite_dir, distance)
                    )
                );
                let front = go (pos, dir);
                if not (0 <= front.x and front.x < n and 0 <= front.y and front.y < n) then (
                    break;
                );
                pos = front;
            };
        };
    });
    print "LONG JUMP DONE";

    const Pos :: type = (.cell = vec2, .dir = int32);

    let mut pos :: Pos = (.cell = pos, .dir = 0);

    print "starting to move";
    let mut answer :: int32 = 0;
    let mut step :: int32 = 0;
    let can_loop = fn (obstruction :: vec2) -> bool {
        let mut pos :: Pos = pos;
        let mut visited = visited;
        visited = list_set (visited, pos.cell.x,
            list_set(visited .> list_get pos.cell.x, pos.cell.y,
                list_set(visited .> list_get pos.cell.x .> list_get pos.cell.y, pos.dir, false)
            )
        );
        # print "checking";
        # dbg pos;
        unwindable result (
            loop {
                if visited .> list_get pos.cell.x .> list_get pos.cell.y .> list_get pos.dir then (
                    # print "found loop";
                    # dbg pos;
                    unwind result true;
                );
                visited = list_set (visited, pos.cell.x,
                    list_set(visited .> list_get pos.cell.x, pos.cell.y,
                        list_set(visited .> list_get pos.cell.x .> list_get pos.cell.y, pos.dir, true)
                    )
                );
                let mut jump = long_jumps .> list_get pos.cell.x .> list_get pos.cell.y .> list_get pos.dir;
                let to_obstruction :: vec2 = (.x = obstruction.x - pos.cell.x, .y = obstruction.y - pos.cell.y);
                let dir_vec = list_get (dirs, pos.dir);
                if to_obstruction.x == 0 and dir_vec.x == 0 and (to_obstruction.y > 0) == (dir_vec.y > 0) then (
                    jump = min(jump, abs(to_obstruction.y));
                );
                if to_obstruction.y == 0 and dir_vec.y == 0 and (to_obstruction.x > 0) == (dir_vec.x > 0) then (
                    jump = min(jump, abs(to_obstruction.x));
                );
                let front :: vec2 = (.x = pos.cell.x + dir_vec.x * jump, .y = pos.cell.y + dir_vec.y * jump);
                if not (0 <= front.x and front.x < n and 0 <= front.y and front.y < n) then (
                    unwind result false;
                );
                jump -= 1;
                let front :: vec2 = (.x = pos.cell.x + dir_vec.x * jump, .y = pos.cell.y + dir_vec.y * jump);
                pos = (.cell = front, .dir = (pos.dir + 1) % 4);
            };
            panic "hmm";
            true
        )
    };
    loop {
        dbg step;
        step += 1;
        # dbg pos;
        found = list_set(found, pos.cell.x, list_set(found .> list_get pos.cell.x, pos.cell.y, true));
        if visited .> list_get pos.cell.x .> list_get pos.cell.y .> list_get pos.dir then (
            panic "wut";
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
            if not(found .> list_get front.x .> list_get front.y) and can_loop(front) then (
                found = list_set(found, front.x, list_set(found .> list_get front.x, front.y, true));
                # print "can loop at";
                # dbg front;
                answer += 1;
            );
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
