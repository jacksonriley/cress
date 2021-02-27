//! Structs and utilities associated with board geometry

use super::Player;

/// That's right, it's a square
#[derive(Copy, Clone, PartialEq, Eq, druid::Data, Debug, Hash)]
pub struct Square {
    /// This square's file
    pub file: File,
    /// This square's rank
    pub rank: Rank,
}

impl Square {
    /// Get the unique index of this square. Runs from 0 for A1 to 63 for H8.
    ///
    /// ```
    /// use cress::engine::{Square, File, Rank};
    /// assert_eq!(Square{file: File::A, rank: Rank::R1}.get_idx(), 0);
    /// assert_eq!(Square{file: File::B, rank: Rank::R1}.get_idx(), 1);
    /// assert_eq!(Square{file: File::H, rank: Rank::R8}.get_idx(), 63);
    /// ```
    pub fn get_idx(self) -> usize {
        let file_idx = self.file.get_idx();
        let rank_idx = self.rank.get_idx();
        8 * rank_idx + file_idx
    }

    /// Construct a square given its file and rank indices.
    ///
    /// ```
    /// use cress::engine::{Square, File, Rank};
    /// assert_eq!(Square::from_idxs(2, 3), Some(Square{file: File::C, rank: Rank::R4}));
    /// assert_eq!(Square::from_idxs(2, 31), None);
    /// ```
    pub fn from_idxs(file_idx: usize, rank_idx: usize) -> Option<Self> {
        if file_idx > 7 || rank_idx > 7 {
            // Out of bounds
            return None;
        }
        let file = File::from_idx(file_idx);
        let rank = Rank::from_idx(rank_idx);
        if let Some(file) = file {
            if let Some(rank) = rank {
                Some(Square { file, rank })
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Construct a square given the unique index.
    ///
    /// ```
    /// use cress::engine::{Square, File, Rank};
    /// assert_eq!(Square::from_idx(1), Some(Square{file: File::B, rank: Rank::R1}));
    /// assert_eq!(Square::from_idx(70), None);
    /// ```
    pub fn from_idx(idx: usize) -> Option<Self> {
        if idx > 63 {
            // Out of bounds
            return None;
        }
        let file_idx = idx % 8;
        let rank_idx = (idx - file_idx) / 8;
        let file = File::from_idx(file_idx);
        let rank = Rank::from_idx(rank_idx);
        if let Some(file) = file {
            if let Some(rank) = rank {
                Some(Square { file, rank })
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Gets the square a certain number of steps to the right (from White's
    /// perspective), if it exists.
    /// ```
    /// use cress::engine::{Square, File, Rank};
    /// assert_eq!(Square{file: File::B, rank: Rank::R3}.right(2),
    ///            Some(Square{file: File::D, rank: Rank::R3}));
    /// assert_eq!(Square{file: File::H, rank: Rank::R3}.right(1), None);
    /// ```
    pub fn right(&self, num_steps: usize) -> Option<Square> {
        &Vec2 {
            delta_f: num_steps as isize,
            delta_r: 0,
        } + self
    }

    /// Gets the square a certain number of steps to the left (from White's
    /// perspective), if it exists.
    /// ```
    /// use cress::engine::{Square, File, Rank};
    /// assert_eq!(Square{file: File::F, rank: Rank::R7}.left(3),
    ///            Some(Square{file: File::C, rank: Rank::R7}));
    /// assert_eq!(Square{file: File::A, rank: Rank::R3}.left(1), None);
    /// ```
    pub fn left(&self, num_steps: usize) -> Option<Square> {
        &Vec2 {
            delta_f: -(num_steps as isize),
            delta_r: 0,
        } + self
    }

    fn up(&self, num_steps: usize) -> Option<Square> {
        &Vec2 {
            delta_f: 0,
            delta_r: num_steps as isize,
        } + self
    }

    fn down(&self, num_steps: usize) -> Option<Square> {
        &Vec2 {
            delta_f: 0,
            delta_r: -(num_steps as isize),
        } + self
    }
}

impl std::fmt::Display for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.file, self.rank)
    }
}

/// No way, it's all the different files
#[derive(Copy, Clone, Debug, PartialEq, Eq, druid::Data, Hash)]
#[allow(missing_docs)]
pub enum File {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
}

impl File {
    /// Get the unique index for this File. Runs from 0 for A to 7 for H.
    ///
    /// ```
    /// use cress::engine::File;
    /// assert_eq!(File::B.get_idx(), 1);
    /// assert_eq!(File::H.get_idx(), 7);
    /// ```
    pub fn get_idx(self) -> usize {
        use File::*;
        match self {
            A => 0,
            B => 1,
            C => 2,
            D => 3,
            E => 4,
            F => 5,
            G => 6,
            H => 7,
        }
    }

    /// If passed an index in the range 0..8, constructs the corresponding File
    ///
    /// ```
    /// use cress::engine::File;
    /// assert_eq!(File::from_idx(2), Some(File::C));
    /// assert_eq!(File::from_idx(10), None);
    /// ```
    pub fn from_idx(idx: usize) -> Option<Self> {
        use File::*;
        match idx {
            0 => Some(A),
            1 => Some(B),
            2 => Some(C),
            3 => Some(D),
            4 => Some(E),
            5 => Some(F),
            6 => Some(G),
            7 => Some(H),
            _ => None,
        }
    }
}

impl std::fmt::Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let letter: char = (self.get_idx() as u8 + b'A') as char;
        write!(f, "{}", letter)
    }
}

/// You'd better believe these bad boys are all the possible ranks
#[derive(Copy, Clone, Debug, PartialEq, Eq, druid::Data, Hash)]
#[allow(missing_docs)]
pub enum Rank {
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
}

impl Rank {
    /// Get the unique index for this Rank. Runs from 0 for R1 to 7 for R8.
    /// That's never going to be confusing, right?
    ///
    /// ```
    /// use cress::engine::Rank;
    /// assert_eq!(Rank::R2.get_idx(), 1);
    /// assert_eq!(Rank::R8.get_idx(), 7);
    /// ```
    pub fn get_idx(self) -> usize {
        use Rank::*;
        match self {
            R1 => 0,
            R2 => 1,
            R3 => 2,
            R4 => 3,
            R5 => 4,
            R6 => 5,
            R7 => 6,
            R8 => 7,
        }
    }

    /// If passed an index in the range 0..8, constructs the corresponding Rank
    /// ```
    /// use cress::engine::Rank;
    /// assert_eq!(Rank::from_idx(2), Some(Rank::R3));
    /// assert_eq!(Rank::from_idx(10), None);
    /// ```
    pub fn from_idx(idx: usize) -> Option<Self> {
        use Rank::*;
        match idx {
            0 => Some(R1),
            1 => Some(R2),
            2 => Some(R3),
            3 => Some(R4),
            4 => Some(R5),
            5 => Some(R6),
            6 => Some(R7),
            7 => Some(R8),
            _ => None,
        }
    }

    /// Construct the concrete Rank from either player's perspective.
    ///
    /// ```
    /// use cress::engine::{Rank, Player};
    /// let closest_rank = Rank::R1;
    /// assert_eq!(closest_rank.get_from_perspective(&Player::Black), Rank::R8);
    /// assert_eq!(closest_rank.get_from_perspective(&Player::White), Rank::R1);
    /// ```
    pub fn get_from_perspective(self, player: &Player) -> Rank {
        match player {
            Player::White => self,
            Player::Black => Self::from_idx(7 - self.get_idx()).unwrap(),
        }
    }
}

impl std::fmt::Display for Rank {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_idx() + 1)
    }
}

/// A vector on the board, as specified by a signed number of file and rank
/// steps
#[derive(Copy, Clone, Debug)]
pub struct Vec2 {
    /// How many ranks upwards
    pub delta_r: isize,
    /// How many files right
    pub delta_f: isize,
}

impl std::ops::Add<&Vec2> for &Vec2 {
    type Output = Vec2;

    fn add(self, other: &Vec2) -> Vec2 {
        Vec2 {
            delta_r: self.delta_r + other.delta_r,
            delta_f: self.delta_f + other.delta_f,
        }
    }
}

impl std::ops::Add<&Square> for &Vec2 {
    type Output = Option<Square>;

    fn add(self, other: &Square) -> Option<Square> {
        let file_idx = other.file.get_idx() as isize + self.delta_f;
        let rank_idx = other.rank.get_idx() as isize + self.delta_r;
        if file_idx < 0 || rank_idx < 0 {
            None
        } else {
            Square::from_idxs(file_idx as usize, rank_idx as usize)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_get_idx_from_idx() {
        for idx in 0..64 {
            assert_eq!(idx, Square::from_idx(idx).unwrap().get_idx());
        }
        for idx in 0..8 {
            assert_eq!(idx, File::from_idx(idx).unwrap().get_idx());
            assert_eq!(idx, Rank::from_idx(idx).unwrap().get_idx());
        }
    }
}
