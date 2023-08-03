use std::collections::{HashMap, HashSet};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum LogicalSExpr {
    True,
    False,
    Var(String),
    Not(Box<LogicalSExpr>),
    Or(Box<LogicalSExpr>, Box<LogicalSExpr>),
    And(Box<LogicalSExpr>, Box<LogicalSExpr>),
    Iff(Box<LogicalSExpr>, Box<LogicalSExpr>),
    Xor(Box<LogicalSExpr>, Box<LogicalSExpr>),
    Ite(Box<LogicalSExpr>, Box<LogicalSExpr>, Box<LogicalSExpr>),
}

impl LogicalSExpr {
    /// ```
    /// use rsdd::serialize::LogicalSExpr;
    ///
    /// let expr =
    /// serde_sexpr::from_str::<LogicalSExpr>("(Or (Var X) (Or (Not (Var X)) (Var Y)))").unwrap();
    /// let vars = expr.unique_variables();

    /// assert!(vars.len() == 2);
    /// assert!(vars.contains(&String::from("X")));
    /// assert!(vars.contains(&String::from("Y")));
    /// ```
    pub fn unique_variables(&self) -> HashSet<&String> {
        match self {
            LogicalSExpr::True | LogicalSExpr::False => HashSet::new(),
            LogicalSExpr::Var(s) => HashSet::from([s]),
            LogicalSExpr::Not(l) => l.unique_variables(),
            LogicalSExpr::Or(a, b)
            | LogicalSExpr::And(a, b)
            | LogicalSExpr::Iff(a, b)
            | LogicalSExpr::Xor(a, b) => a
                .unique_variables()
                .union(&b.unique_variables())
                .cloned()
                .collect::<HashSet<&String>>(),
            LogicalSExpr::Ite(a, b, c) => a
                .unique_variables()
                .union(&b.unique_variables())
                .cloned()
                .collect::<HashSet<&String>>()
                .union(&c.unique_variables())
                .cloned()
                .collect::<HashSet<&String>>(),
        }
    }

    /// ```
    /// use rsdd::serialize::LogicalSExpr;
    ///
    /// let expr =
    /// serde_sexpr::from_str::<LogicalSExpr>("(Or (Var X) (Or (Not (Var X)) (Var Y)))").unwrap();
    /// let vars = expr.unique_variables();
    /// let mapping = expr.variable_mapping();
    ///
    /// assert_eq!(*mapping.get(&String::from("X")).unwrap(), 0);
    /// assert_eq!(*mapping.get(&String::from("Y")).unwrap(), 1);
    /// assert_eq!(mapping.get(&String::from("Z")), None);
    /// ```
    pub fn variable_mapping(&self) -> HashMap<&String, usize> {
        let mut v: Vec<_> = self.unique_variables().into_iter().collect();
        v.sort();
        HashMap::from_iter(v.into_iter().enumerate().map(|(index, val)| (val, index)))
    }
}

#[test]
fn logical_expression_deserialization_base_cases() {
    assert_eq!(
        serde_sexpr::from_str::<LogicalSExpr>("True").unwrap(),
        LogicalSExpr::True
    );

    assert_eq!(
        serde_sexpr::from_str::<LogicalSExpr>("False").unwrap(),
        LogicalSExpr::False
    );

    assert_eq!(
        serde_sexpr::from_str::<LogicalSExpr>("(Var x)").unwrap(),
        LogicalSExpr::Var(String::from("x"))
    );

    assert_eq!(
        serde_sexpr::from_str::<LogicalSExpr>("(Var X)").unwrap(),
        LogicalSExpr::Var(String::from("X"))
    );

    assert_eq!(
        serde_sexpr::from_str::<LogicalSExpr>("(Var 1)").unwrap(),
        LogicalSExpr::Var(String::from("1"))
    );
}

#[test]
fn logical_expression_deserialization_boxed() {
    assert_eq!(
        serde_sexpr::from_str::<LogicalSExpr>("(Not True)").unwrap(),
        LogicalSExpr::Not(Box::new(LogicalSExpr::True))
    );

    assert_eq!(
        serde_sexpr::from_str::<LogicalSExpr>("(Not False)").unwrap(),
        LogicalSExpr::Not(Box::new(LogicalSExpr::False))
    );

    assert_eq!(
        serde_sexpr::from_str::<LogicalSExpr>("(Not (Var X))").unwrap(),
        LogicalSExpr::Not(Box::new(LogicalSExpr::Var(String::from("X"))))
    );

    assert_eq!(
        serde_sexpr::from_str::<LogicalSExpr>("(Or (Var X) (Var Y))").unwrap(),
        LogicalSExpr::Or(
            Box::new(LogicalSExpr::Var(String::from("X"))),
            Box::new(LogicalSExpr::Var(String::from("Y")))
        )
    );

    assert_eq!(
        serde_sexpr::from_str::<LogicalSExpr>("(And (Var X) (Var Y))").unwrap(),
        LogicalSExpr::And(
            Box::new(LogicalSExpr::Var(String::from("X"))),
            Box::new(LogicalSExpr::Var(String::from("Y")))
        )
    );

    assert_eq!(
        serde_sexpr::from_str::<LogicalSExpr>("(Iff (Var X) (Var Y))").unwrap(),
        LogicalSExpr::Iff(
            Box::new(LogicalSExpr::Var(String::from("X"))),
            Box::new(LogicalSExpr::Var(String::from("Y")))
        )
    );

    assert_eq!(
        serde_sexpr::from_str::<LogicalSExpr>("(Xor (Var X) (Var Y))").unwrap(),
        LogicalSExpr::Xor(
            Box::new(LogicalSExpr::Var(String::from("X"))),
            Box::new(LogicalSExpr::Var(String::from("Y")))
        )
    );

    assert_eq!(
        serde_sexpr::from_str::<LogicalSExpr>("(Ite (Var X) (Var Y) (Var Z))").unwrap(),
        LogicalSExpr::Ite(
            Box::new(LogicalSExpr::Var(String::from("X"))),
            Box::new(LogicalSExpr::Var(String::from("Y"))),
            Box::new(LogicalSExpr::Var(String::from("Z")))
        )
    );
}

#[test]
fn logical_expression_unique_variables_trivial() {
    let expr = serde_sexpr::from_str::<LogicalSExpr>("(Var X)").unwrap();
    let vars = expr.unique_variables();

    assert!(vars.len() == 1);
    assert!(vars.contains(&String::from("X")));
}

#[test]
fn logical_expression_unique_variables_handles_duplicates_and_nesting() {
    let expr =
        serde_sexpr::from_str::<LogicalSExpr>("(Or (Var X) (Or (Not (Var X)) (Xor (Iff (Var X) (Var Y)) (Ite (Var Y) (Not (Var X)) (Not (Var Y))))))").unwrap();
    let vars = expr.unique_variables();

    assert!(vars.len() == 2);
    assert!(vars.contains(&String::from("X")));
    assert!(vars.contains(&String::from("Y")));
}

#[test]
fn logical_expression_variable_mapping_is_lexicographic() {
    let expr =
        serde_sexpr::from_str::<LogicalSExpr>("(Or (Var X) (Or (Not (Var X)) (Var Y)))").unwrap();
    let mapping = expr.variable_mapping();

    assert_eq!(*mapping.get(&String::from("X")).unwrap(), 0);
    assert_eq!(*mapping.get(&String::from("Y")).unwrap(), 1);
    assert_eq!(mapping.get(&String::from("Z")), None);
}
