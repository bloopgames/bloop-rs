pub trait SplitExt<A, B>: IntoIterator<Item = (A, B)> + Clone {
    fn split(self) -> (impl Iterator<Item = A>, impl Iterator<Item = B>) {
        (
            self.clone().into_iter().map(|(a, _)| a),
            self.into_iter().map(|(_, b)| b),
        )
    }
}

impl<I, A, B> SplitExt<A, B> for I where I: IntoIterator<Item = (A, B)> + Clone {}
