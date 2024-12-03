module:

use std.*;

let lines = fn(s) {
    split_by(s, c => c == '\n', true)
};

let split_whitespace = fn(s) {
    split_by(s, c => c == ' ', false)
};

let split_by = fn(s :: string, f :: char -> bool, yield_empty :: bool) {
    let mut line = "";
    for c in chars(s) {
        if f(c) then (
            if yield_empty or line != "" then (
                yield line;
            );
            line = "";
        ) else (
            line = push_char(line, c);
        )
    };
    if line != "" then (
        yield line;
    );
};

let sort = fn(x :: list[int32]) -> list[int32] {
    if list_length x <= 1 then (
        x
    ) else (
        let pivot = list_get (x, 0);
        let first_half = sort <| list_filter (x, elem => elem < pivot);
        let equals = list_filter (x, elem => elem == pivot);
        let second_half = sort <| list_filter (x, elem => elem > pivot);
        let mut result = first_half;
        for elem in list_iter equals {
            result = list_push (result, elem);
        };
        for elem in list_iter second_half {
            result = list_push (result, elem);
        };
        result
    )
};

let abs = fn (x :: int32) -> int32 {
    if x < 0 then -x else +x
};

let list_filter = ( const T = int32; #forall[T] {
    fn(x :: list[T], f :: T -> bool) -> list[T] {
        let mut result = list[];
        for elem in list_iter[T] x {
            if f elem then (
                result = list_push[T] (result, elem);
            );
        };
        result
    }
); #};
