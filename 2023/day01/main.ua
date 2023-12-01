F ← parse⊔⊢↘1regex"^[^\\d]*(\\d)"
L ← parse⊔⊢↘1regex"(\\d)[^\\d]*$"
/+⊜(+×10:⊃L F)≠@\n.&fras"input"
