use import "../util.ks".*;
use std.*;

syntax while <- 10 = "while" cond "{" body "}";
impl syntax @"while" = macro (.cond, .body) => `(
    loop {
        if $cond then $body else (break);
    }
);

let input = read_file "day01/input.txt";

let mut a :: list[int32] = list[];
let mut b :: list[int32] = list[];
for (line :: string) in lines(input) {
    let mut i :: int32 = 0;
    #dbg line;
    for (item :: string) in split_whitespace(line) {
        if i == 0 then (
            a = list_push(a, parse item);
        ) else (
            b = list_push(b, parse item);
        );
        i = i + 1;
    }
};

print "read input";

# a = sort a;
b = sort b;

print "sorted input";

# returns first index where elem >= x
let binary_search = fn(x :: int32, xs :: list[int32]) -> int32 {
    let mut lf, mut rg = -1, list_length xs;
    while (rg - lf) >= 2 {
        let mid = (lf + rg) / 2;
        if list_get (xs, mid) >= x then (
            rg = mid;
        ) else (
            lf = mid;
        );
    };
    rg
};

let occurences = fn(x :: int32) -> int32 {
    binary_search(x + 1, b) - binary_search(x, b)
};

let mut result = 0;
for x :: int32 in list_iter a {
    let n = occurences(x);
    #dbg x;
    #print "occurences = ";
    #dbg n;
    result = result + x * n;
};
dbg result
