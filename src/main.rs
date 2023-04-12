fn main() {
    let m1 = String::from("Hello");
    let m2 = String::from("world");
    greet_ref(&m1, &m2);
    println!("{} {}", m1, m2);
}

fn greet_ref(g1: &String, g2: &String) {
    println!("{} {}!", g1, g2);
}

// fn greet(g1: String, g2: String) {
//     println!("{} {}!", g1, g2);
// }

// fn greet2(g1: String, g2: String) -> (String, String) {
//     println!("{} {}!", g1, g2);
//     return (g1, g2);
// }


