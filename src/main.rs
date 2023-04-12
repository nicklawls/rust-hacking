fn main() {
    let mut vec: Vec<i32> = vec![1, 2, 3];
    let num = &mut vec[2];

    *num += 1;
    println!("Third element is now {}", *num);

    println!("Vector is now {:?}", vec);
}
