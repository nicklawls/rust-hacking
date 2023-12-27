use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rustbook::QueryBoxed;

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

    let mut group = c.benchmark_group("Query Transformation");

    group.bench_function("to_sql_ref", |b| {
        b.iter_batched(
            || serde_json::from_str::<QueryBoxed>(data.clone()).unwrap(),
            |val| black_box(&val).to_sql_ref(),
            criterion::BatchSize::SmallInput,
        )
    });

    group.bench_function("to_sql", |b| {
        b.iter_batched(
            || serde_json::from_str::<QueryBoxed>(data.clone()).unwrap(),
            |val| black_box(val).to_sql(),
            criterion::BatchSize::SmallInput,
        )
    });

    group.bench_function("to_sql_direct_rec", |b| {
        b.iter_batched(
            || serde_json::from_str::<QueryBoxed>(data.clone()).unwrap(),
            |val| black_box(&val).to_sql_direct_rec(),
            criterion::BatchSize::SmallInput,
        )
    });

    group.bench_function("stack-full", |b| {
        b.iter_batched(
            || serde_json::from_str::<QueryBoxed>(data.clone()).unwrap(),
            |val| black_box(&val).to_sql_direct_stack(),
            criterion::BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
