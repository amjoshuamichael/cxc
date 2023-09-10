#[derive(Debug)]
pub struct UniqueVec<T: PartialEq + Eq>(Vec<T>);

impl<T: PartialEq + Eq> std::ops::Deref for UniqueVec<T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Self::Target { &self.0 }
}

impl<T: PartialEq + Eq> std::ops::DerefMut for UniqueVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
}

impl<T: PartialEq + Eq> Default for UniqueVec<T> {
    fn default() -> Self {
        Self(Vec::default())
    }
}

impl<T: PartialEq + Eq> IntoIterator for UniqueVec<T> {
    type Item = T;
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T: PartialEq + Eq> IntoIterator for &'a UniqueVec<T> {
    type Item = &'a T;
    type IntoIter = <&'a Vec<T> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<T: PartialEq + Eq> UniqueVec<T> {
    pub fn insert(&mut self, val: T) {
        if self.contains(&val) {
            return; 
        }

        self.0.push(val)
    }

    pub fn remove(&mut self, val: &T) {
        if let Some(index) = self.0.iter().position(|v| v == val) {
            self.0.remove(index);
        }
    }
    
    pub fn new() -> Self { Self::default() }
}


