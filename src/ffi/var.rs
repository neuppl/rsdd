use crate::repr::{Literal, VarLabel};

#[no_mangle]
pub extern "C" fn var_label_new(label: u64) -> VarLabel {
    VarLabel::new(label)
}

#[no_mangle]
pub extern "C" fn literal_new(label: VarLabel, polarity: bool) -> Literal {
    Literal::new(label, polarity)
}
