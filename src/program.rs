pub struct Program<T> {
    pub source: String,
    pub model: Vec<T>,
    pub has_errors: bool
}
