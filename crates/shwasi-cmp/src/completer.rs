use crate::{source::Source, trie::Trie};

pub struct Completer {
    source: Box<dyn Source>,
}

impl Completer {
    pub fn new(default: Box<dyn Source>) -> Self {
        Self { source: default }
    }

    pub fn suggest(&self, current_word: &str) -> Vec<String> {
        let trie = self.source.suggest().into_iter().collect::<Trie>();
        trie.prefix_list(current_word)
    }
}
