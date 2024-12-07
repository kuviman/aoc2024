use std.*;
use (import "./util.ks").*;

let mut a = list[];
for i :: int32 in range(0, 1000) {
    a = list_push(a, i);
};
print "made";
sort a
