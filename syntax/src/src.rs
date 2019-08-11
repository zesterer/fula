use std::fmt;

#[derive(Copy, Clone)]
pub struct SrcLoc(usize);

impl SrcLoc {
    pub fn start() -> Self {
        Self(0)
    }

    pub fn min(self, other: Self) -> Self {
        Self(self.0.min(other.0))
    }

    pub fn max(self, other: Self) -> Self {
        Self(self.0.max(other.0))
    }
}

impl fmt::Debug for SrcLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl From<usize> for SrcLoc {
    fn from(pos: usize) -> Self {
        Self(pos)
    }
}

#[derive(Copy, Clone)]
pub enum SrcRef {
    None,
    Range(SrcLoc, SrcLoc),
    From(SrcLoc),
    Until(SrcLoc),
}

impl SrcRef {
    pub fn none() -> Self {
        SrcRef::None
    }

    pub fn range(from: SrcLoc, until: SrcLoc) -> SrcRef {
        if from.0 < until.0 {
            SrcRef::Range(from, until)
        } else {
            SrcRef::None
        }
    }

    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (SrcRef::None, _) => SrcRef::None,
            (_, SrcRef::None) => SrcRef::None,
            (SrcRef::Range(from_a, until_a), SrcRef::Range(from_b, until_b)) =>
                SrcRef::Range(from_a.min(from_b), until_a.max(until_b)),
            (SrcRef::Range(from_a, _), SrcRef::From(from_b)) => SrcRef::From(from_a.min(from_b)),
            (SrcRef::From(from_a), SrcRef::Range(from_b, _)) => SrcRef::From(from_a.min(from_b)),
            (SrcRef::Range(_, until_a), SrcRef::Until(until_b)) => SrcRef::Until(until_a.max(until_b)),
            (SrcRef::Until(until_a), SrcRef::Range(_, until_b)) => SrcRef::Until(until_a.max(until_b)),
            (SrcRef::From(from_a), SrcRef::From(from_b)) => SrcRef::From(from_a.min(from_b)),
            (SrcRef::Until(until_a), SrcRef::Until(until_b)) => SrcRef::Until(until_a.max(until_b)),
            (SrcRef::From(from_a), SrcRef::Until(until_b)) => SrcRef::range(from_a, until_b),
            (SrcRef::Until(until_a), SrcRef::From(from_b)) => SrcRef::range(from_b, until_a),
        }
    }
}

impl fmt::Debug for SrcRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SrcRef::None => write!(f, "<none>"),
            SrcRef::Range(from, to) => write!(f, "{:?}:{:?}", from, to),
            SrcRef::From(from) => write!(f, "{:?}:", from),
            SrcRef::Until(until) => write!(f, ":{:?}", until),
        }
    }
}

impl From<usize> for SrcRef {
    fn from(pos: usize) -> Self {
        SrcRef::Range(SrcLoc::from(pos), SrcLoc::from(pos + 1))
    }
}

impl From<(usize, usize)> for SrcRef {
    fn from((from, to): (usize, usize)) -> Self {
        SrcRef::Range(SrcLoc::from(from), SrcLoc::from(to))
    }
}
