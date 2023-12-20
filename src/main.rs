fn incr(n: &mut i32) {
    *n += 1;
}
fn main() {
    let mut n = 1;
    incr(&mut n);
    println!("{n}");
}

fn first(strings: &Vec<String>) -> &String {
    let s_ref = &strings[0];
    return s_ref;
}

// fn first_or(strings: &Vec<String>, default: &String) -> &String {
//     if strings.len() > 0 {
//         &strings[0]
//     } else {
//         default
//     }
// }

// fn return_a_string() -> &String {
//     let s = String::from("Hello");
//     let s_ref = &s;
//     s_ref;
// }

fn printo() -> () {
    let s = String::from("hi");
    println!("{s}")
}

fn give_and_take(v: &Vec<i32>, n: i32) -> i32 {
    v.push(n);
    v.remove(0)
}
