pub struct Program<Statement> {
    pub source: String,
    pub model: Vec<Statement>,
    pub has_errors: bool
}
