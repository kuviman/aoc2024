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

const Pos :: type = (.x = int32, .y = int32);
const Input :: type = (
    .antennas = HashMap[char, list[Pos]],
    .size = Pos,
) as type;

const Default = forall[Self] { (.default = () -> Self) as type };
const or_default = forall[T] {
    fn(value :: Option[T]) -> T {
        match value {
            | :Some value => value
            | :None => (T as Default).default ()
        }
    }
};

impl list[Pos] :: type as Default = (.default = () => list[]);

let read_input = fn(path) -> Input {
    let input = read_file path;
    let mut antennas = HashMap_new[_, _] ();
    let mut max_x, mut max_y = 0, 0;
    for x :: int32, line :: string in lines(input) $> enumerate[string] {
        for y :: int32, c :: char in chars(line) $> enumerate[char] {
            if c != '.' then (
                let cur = HashMap_get[_, list[Pos]] (antennas, c) |> or_default[list[Pos]];
                antennas = HashMap_insert[_, _] (antennas, c, list_push(cur, (.x, .y)));
            );
            max_y = y;
        };
        max_x = x;
    };
    (.antennas, .size = (.x = max_x + 1, .y = max_y + 1))
};

let solve = fn(path) {
    let input = read_input path;
    let mut unique_antinodes = HashMap_new[_, _] ();
    let add_antinode = fn(pos :: Pos) {
        if 0 <= pos.x and pos.x < input.size.x and 0 <= pos.y and pos.y < input.size.y then (
            unique_antinodes = HashMap_insert[_, _] (unique_antinodes, pos, ());
        );
    };
    for c :: char, positions :: list[Pos] in HashMap_iter[_, _] input.antennas {
        dbg c;
        for i in range(0, list_length positions) {
            let a = list_get (positions, i);
            for j in range(0, i) {
                let b = list_get (positions, j);
                add_antinode(.x = 2 * a.x - b.x, .y = 2 * a.y - b.y);
                add_antinode(.x = 2 * b.x - a.x, .y = 2 * b.y - a.y);
            }
        };
    };
    HashMap_size[_, _] unique_antinodes
};

if exec_mode() is :Run then (
    let answer = solve "day08/input.txt";
    print "== ANSWER ==";
    dbg answer;
);

