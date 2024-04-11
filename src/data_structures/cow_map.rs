use std::{
    collections::{BTreeMap, HashSet},
    fmt::{Debug, Formatter},
    hash::Hash,
};

/// A copy-on-write map. It can be forked into another map cheaply, and the forked map can be
/// modified without affecting the original map.
#[derive(Default)]
pub struct CowMap<K, V> {
    parent: Option<Box<CowMap<K, V>>>,
    map: BTreeMap<K, V>,
}

impl<K, V> CowMap<K, V>
where
    K: Ord + Clone + Hash + Default,
    V: Clone + Default,
{
    #[cfg(test)]
    pub fn new() -> Self {
        Self {
            parent: None,
            map: BTreeMap::new(),
        }
    }

    pub fn is_root(&self) -> bool {
        self.parent.is_none()
    }

    pub fn fork(&mut self) {
        self.fork_inner(BTreeMap::new())
    }

    fn fork_inner(&mut self, map: BTreeMap<K, V>) {
        let me = std::mem::take(self);

        *self = Self {
            parent: Some(Box::new(me)),
            map,
        };
    }

    pub fn unfork(&mut self) -> BTreeMap<K, V> {
        let CowMap { parent, map } = std::mem::take(self);
        let parent = parent.expect("Cannot join() a CowMap without having called fork()");

        *self = *parent;

        map
    }

    fn ancestry_iter<'a>(&'a self) -> impl Iterator<Item = &'a BTreeMap<K, V>> {
        std::iter::successors(Some(self), |map| map.parent.as_deref()).map(|map| &map.map)
    }

    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Ord + Hash,
    {
        self.ancestry_iter().find_map(|map| map.get(key))
    }
    pub fn get_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Ord + Hash,
    {
        match self.map.get_mut(key) {
            Some(v) => Some(v),
            None => self.parent.as_mut().and_then(|parent| parent.get_mut(key)),
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.map.insert(key, value);
    }

    pub(crate) fn into_iter(self) -> impl Iterator<Item = (K, V)> {
        CowMapIntoIter {
            map: Some(self),
            seen: HashSet::new(),
        }
    }
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        CowMapIter {
            current_key: None,
            map: Some(self),
            seen: HashSet::new(),
        }
    }
}

struct IntoAncestryIter<'a, K, V> {
    map: Option<&'a CowMap<K, V>>,
}
impl<'a, K, V> Iterator for IntoAncestryIter<'a, K, V> {
    type Item = &'a CowMap<K, V>;

    fn next(&mut self) -> Option<Self::Item> {
        let me = self.map.take()?;

        self.map = Some(me.parent.as_ref()?.as_ref());
        Some(me)
    }
}

struct CowMapIntoIter<K, V> {
    map: Option<CowMap<K, V>>,
    seen: HashSet<K>,
}
impl<K, V> Iterator for CowMapIntoIter<K, V>
where
    K: Ord + Hash + Clone + Sized,
{
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        let Some(ref mut map) = &mut self.map else {
            return None;
        };

        if let Some(removed) = map.map.pop_first() {
            if self.seen.contains(&removed.0) {
                self.next()
            } else {
                self.seen.insert(removed.0.clone());
                Some(removed)
            }
        } else {
            self.map = match std::mem::take(&mut map.parent) {
                Some(bx) => Some(*bx),
                None => None,
            };
            self.next()
        }
    }
}

struct CowMapIter<'a, K, V> {
    current_key: Option<&'a K>,
    map: Option<&'a CowMap<K, V>>,
    seen: HashSet<K>,
}

impl<'a, K, V> Iterator for CowMapIter<'a, K, V>
where
    K: Ord + Hash + Clone + Sized,
{
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        let map = self.map?;

        let next = if let Some(current_key) = self.current_key {
            let mut range_it = map.map.range(current_key.clone()..);
            range_it.next(); // Skip current key
            range_it.next()
        } else {
            map.map.iter().next()
        };

        if let Some(next) = next {
            self.current_key = Some(next.0);

            if self.seen.contains(next.0) {
                self.next()
            } else {
                self.seen.insert(next.0.clone());
                Some(next)
            }
        } else {
            // Go to parent
            self.map = match &map.parent {
                Some(bx) => Some(bx.as_ref()),
                None => None,
            };
            self.current_key = None;
            return self.next();
        }
    }
}

impl<K, V> Debug for CowMap<K, V>
where
    K: Ord + Hash + Clone + Debug + Default,
    V: Clone + Debug + Default,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "CowMap ")?;
        f.debug_map().entries(self.iter()).finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn some_map() -> CowMap<String, String> {
        let mut map = CowMap::new();

        map.insert("root_a".to_string(), "root_a".to_string());
        map.insert("root_b".to_string(), "root_b".to_string());
        map.insert("changed".to_string(), "changed".to_string());

        map.fork_inner(
            vec![("bumped_in".to_string(), "bumped_in".to_string())]
                .into_iter()
                .collect(),
        );

        map.insert("changed".to_string(), "changed_again".to_string());

        map
    }

    #[test]
    fn test_cow_map() {
        let mut map = some_map();

        assert_eq!(map.get("root_a"), Some(&"root_a".to_string()));
        assert_eq!(map.get("changed"), Some(&"changed_again".to_string()));

        map.insert("changed".to_string(), "changed_yet_again".to_string());
        assert_eq!(map.get("changed"), Some(&"changed_yet_again".to_string()));
    }

    #[test]
    fn test_cow_map_debug() {
        let mut m = some_map();
        insta::assert_debug_snapshot!(m, @r###"
        CowMap {
            "bumped_in": "bumped_in",
            "changed": "changed_again",
            "root_a": "root_a",
            "root_b": "root_b",
        }
        "###);

        let head = m.unfork();
        insta::assert_debug_snapshot!(head, @r###"
        {
            "bumped_in": "bumped_in",
            "changed": "changed_again",
        }
        "###);
        insta::assert_debug_snapshot!(m, @r###"
        CowMap {
            "changed": "changed",
            "root_a": "root_a",
            "root_b": "root_b",
        }
        "###);
    }

    #[test]
    fn test_cow_map_iter() {
        let map_1 = some_map();
        let m: Vec<_> = map_1.iter().map(|(k, v)| format!("{k}: {v}")).collect();
        insta::assert_debug_snapshot!(m, @r###"
        [
            "bumped_in: bumped_in",
            "changed: changed_again",
            "root_a: root_a",
            "root_b: root_b",
        ]
        "###);

        let map_1 = some_map();
        let m: Vec<_> = map_1
            .into_iter()
            .map(|(k, v)| format!("{k}: {v}"))
            .collect();
        insta::assert_debug_snapshot!(m, @r###"
        [
            "bumped_in: bumped_in",
            "changed: changed_again",
            "root_a: root_a",
            "root_b: root_b",
        ]
        "###);

        let mut map_1 = some_map();
        map_1.unfork();
        let m: Vec<_> = map_1.iter().map(|(k, v)| format!("{k}: {v}")).collect();
        insta::assert_debug_snapshot!(m, @r###"
        [
            "changed: changed",
            "root_a: root_a",
            "root_b: root_b",
        ]
        "###);

        let mut map_1 = some_map();
        map_1.unfork();
        let m: Vec<_> = map_1
            .into_iter()
            .map(|(k, v)| format!("{k}: {v}"))
            .collect();
        insta::assert_debug_snapshot!(m, @r###"
        [
            "changed: changed",
            "root_a: root_a",
            "root_b: root_b",
        ]
        "###);
    }
}
