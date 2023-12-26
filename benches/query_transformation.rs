use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rustbook::{fibonacci, QueryBoxed};

pub fn criterion_benchmark(c: &mut Criterion) {
    let data = r#"
        {
            "operator": "OR",
            "operand": [
                {
                    "operator": "AND",
                    "operand": [
                        {
                            "operator": "=",
                            "field": "a",
                            "operand": "1"
                        },
                        {
                            "operator": "=",
                            "field": "b",
                            "operand": "2"
                        },
                        {
                            "operator": "=",
                            "field": "c",
                            "operand": "3"
                        }
                    ]
                },
                {
                    "operator": "=",
                    "field": "a",
                    "operand": "2"
                }
            ]
        }
    "#;

    if let Ok(query) = serde_json::from_str::<QueryBoxed>(data) {
        c.bench_function("to_sql_ref", |b| b.iter(|| black_box(&query).to_sql_ref()));
    }

    c.bench_function("fib 20", |b| b.iter(|| fibonacci(black_box(20))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
