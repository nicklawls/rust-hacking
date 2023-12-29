use std::thread::sleep;

use rustbook::QueryBoxed;

fn main() {
    for _x in 0..20 {
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
        if let Ok(example2) = serde_json::from_str::<QueryBoxed>(data) {
            let stacky = example2.to_sql_direct_stack();
            // because to_sql takes a reference, we can use the result from flat
            // after. But tradeoff, we have to copy the Strings over, they can't
            // change ownership / move.
            // Is there a way to have the QueryFlat be a guaranteed immutable "view"
            // into the same underlying data?
            // remember that String is a "smart pointer", copying it is a "move" rather
            // than a deep copy. So it is more like the "view" idea than I thought.
            // println!("{:#?}", example2); // this errors@!
            // J.K. Clone is a deep copy after all. So this newer version that takes
            // ownership
            println!("{:#?}", stacky.unwrap());
        }
        sleep(std::time::Duration::new(1, 0))
    }
}
