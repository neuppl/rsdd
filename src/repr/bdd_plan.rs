//! Represent an arbitrary logical formula as an abstract syntax tree

use super::var_label::VarLabel;
use serde::{Serialize, Deserialize};
use egg::{*, rewrite as rw};


define_language! {
    pub enum BddPlan {
        "true" = True,
        "false" = False,
        "and" = And([Id; 2]),
        "or" = Or([Id; 2]),
        "not" = Not([Id; 1]),
        Symbol(Symbol),
    }
}

pub fn optimize(r: &RecExpr<BddPlan>) -> RecExpr<BddPlan> {
    let rules: &[Rewrite<BddPlan, ()>] = &[
        rw!("commute-and"; "(and ?x ?y)" => "(and ?y ?x)"),
        rw!("commute-or"; "(or ?x ?y)" => "(or ?y ?x)"),

        rw!("and-f"; "(and ?x false)" => "false"),
        rw!("and-t"; "(and ?x true)" => "?x"),
        rw!("or-f"; "(or ?x false)" => "?x"),
        rw!("or-t"; "(or ?x true)" => "true"),


        rw!("and-incon"; "(and ?x (not ?x))" => "false"),
        rw!("and-con"; "(and ?x ?x)" => "?x"),

        rw!("or-incon"; "(or ?x (not ?x))" => "true"),
        rw!("or-con"; "(or ?x ?x))" => "?x"),

        rw!("dist-and"; "(and ?a (or ?b ?c))" => "(or (and ?a ?b) (and ?a ?c))"),
        
    ];
    // That's it! We can run equality saturation now.
    let runner = Runner::default().with_expr(r).run(rules);

    // Extractors can take a user-defined cost function,
    // we'll use the egg-provided AstSize for now
    let extractor = Extractor::new(&runner.egraph, AstSize);

    // We want to extract the best expression represented in the
    // same e-class as our initial expression, not from the whole e-graph.
    // Luckily the runner stores the eclass Id where we put the initial expression.
    let (best_cost, best_expr) = extractor.find_best(runner.roots[0]);

    return best_expr;
}