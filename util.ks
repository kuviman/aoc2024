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

const Ordering = newtype :Less | :Equal | :Greater;

const Ord = forall[Self] {
    .cmp = (Self, Self) -> int32,
};

impl int32 as Ord = (.cmp = (a, b) => a - b);
impl (int32, int32) :: type as Ord = (
    .cmp = ((a0, a1), (b0, b1)) => (
        if a0 != b0 then (
            a0 - b0
        ) else (
            a1 - b1
        )
    ),
);

let sort = forall[T] {
    fn(x :: list[T]) -> list[T] {
        sort_by(x, (T as Ord).cmp)
    }
};

let min = forall[T] {
    fn(a :: T, b :: T) -> T {
        if a < b then a else b
    }
};

let sort_by = forall[T] {
    fn(x :: list[T], cmp :: (T, T) -> int32) -> list[T] {
        if list_length x <= 1 then (
            x
        ) else (
            let pivot = list_get (x, 0);
            let first_half = sort_by (list_filter (x, elem => cmp (elem, pivot) < 0), cmp);
            let equals = list_filter (x, elem => cmp (elem, pivot) == 0);
            let second_half = sort_by (list_filter (x, elem => cmp (elem, pivot) > 0), cmp);
            let mut result = first_half;
            for elem in list_iter equals {
                result = list_push (result, elem);
            };
            for elem in list_iter second_half {
                result = list_push (result, elem);
            };
            result
        )
    }
};

let binary_search = forall[T] {
    fn (a :: list[T], x :: T) -> bool {
        let mut lf, mut rg = 0, list_length a;
        while rg - lf >= 2 {
            let mid = (lf + rg) / 2;
            let cmp = (T as Ord).cmp (x, list_get (a, mid));
            if cmp < 0 then (
                rg = mid;
            ) else (
                lf = mid;
            );
        };
        (T as Ord).cmp (x, list_get (a, lf)) == 0
    }
};

let abs = fn (x :: int32) -> int32 {
    if x < 0 then -x else +x
};

let list_filter = forall[T] {
    fn(x :: list[T], f :: T -> bool) -> list[T] {
        let mut result = list[];
        for elem in list_iter[T] x {
            if f elem then (
                result = list_push[T] (result, elem);
            );
        };
        result
    }
};

let mut do_log = false;

let set_do_log = @"do" => ( do_log = @"do" );

let log_print = s => (
    if do_log then (
        print s;
    );
);

let log_dbg = forall[T] {
    fn (x :: T) {
        if do_log then (
            dbg x;
        );
    }
};

let range = fn(from :: int32, to :: int32) -> () with loop_context[int32] {
    let mut i = from;
    while i < to {
        yield i;
        i += 1;
    };
};
