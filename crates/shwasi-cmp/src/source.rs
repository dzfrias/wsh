pub trait Source {
    fn suggest(&self) -> Vec<String>;
}
