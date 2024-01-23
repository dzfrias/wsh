use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct Trie {
    root: Node,
}

#[derive(Debug, Default)]
struct Node {
    terminal: bool,
    children: HashMap<char, Node>,
}

impl Trie {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, word: &str) {
        let mut node = &mut self.root;
        for c in word.chars() {
            node = node.children.entry(c).or_default();
        }
        node.terminal = true;
    }

    pub fn is_empty(&self) -> bool {
        self.root.children.is_empty()
    }

    pub fn lookup(&self, search: &str) -> bool {
        let mut current = &self.root;
        for c in search.chars() {
            if let Some(node) = current.children.get(&c) {
                current = node;
            } else {
                return false;
            }
        }
        true
    }

    pub fn prefix_list(&self, prefix: &str) -> Vec<String> {
        // This node will eventually be the root of a subtree containing all matches to `prefix`
        let mut prefix_node = &self.root;
        for c in prefix.chars() {
            if let Some(node) = prefix_node.children.get(&c) {
                prefix_node = node;
            } else {
                return vec![];
            }
        }

        let mut words = vec![];
        let mut stack = vec![(prefix_node, prefix.to_owned())];
        while let Some((node, word)) = stack.pop() {
            if node.terminal {
                words.push(word.clone());
            }
            for (c, child) in &node.children {
                let mut word = word.clone();
                word.push(*c);
                stack.push((child, word));
            }
        }
        words.sort();

        words
    }
}

impl FromIterator<String> for Trie {
    fn from_iter<T: IntoIterator<Item = String>>(iter: T) -> Self {
        let mut trie = Self::new();
        for word in iter {
            trie.insert(&word);
        }
        trie
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn suggest_words_with_common_prefix() {
        let mut trie = Trie::new();
        trie.insert("hello");
        trie.insert("hell");
        trie.insert("help");
        trie.insert("helping");
        trie.insert("her");

        assert_eq!(
            vec![
                "hell".to_owned(),
                "hello".to_owned(),
                "help".to_owned(),
                "helping".to_owned()
            ],
            trie.prefix_list("hel"),
        );
        assert_eq!(vec!["hello".to_owned()], trie.prefix_list("hello"));
        assert_eq!(
            vec![
                "hell".to_owned(),
                "hello".to_owned(),
                "help".to_owned(),
                "helping".to_owned(),
                "her".to_owned(),
            ],
            trie.prefix_list("he")
        );
    }

    #[test]
    fn trie_no_suggestions_on_irrelevant() {
        let mut trie = Trie::new();
        trie.insert("nice");

        assert!(trie.prefix_list("hello").is_empty());
    }

    #[test]
    fn empty_trie() {
        let trie = Trie::new();
        assert!(trie.prefix_list("hello").is_empty());
    }
}
