
//! Structures and types used in the main chess engine.

/// All non-graphical game state
pub struct ChessState {
    /// An array, running from A1 (index 0) to H8 (index 63), of [`Option<Piece>`]
    pub pieces: [Option<Piece>; 64],
    /// Which player's turn it is
    pub player_turn: Player,
    /// What castling rights are available to each player ([White, Black])
    pub castling_rights: [CastlingRights; 2],
    /// If `Some`, the file on which a pawn may be taken en passant
    pub en_passant_file: Option<File>,
    /// How many moves have been taken
    pub move_number: u16,
}

impl ChessState {
    /// Creates an instance of [`ChessState`] corresponding to the initial game
    /// state
    pub fn new_game() -> Self {
        let mut pieces = [None; 64];
        for player in Player::iter() {
            for &rank_perspective in &[Rank::R1, Rank::R2] {
                let rank = rank_perspective.get_from_perspective(&player);
                use File::*;
                use PieceKind::*;
                for &(file, piece_kind) in &[
                    (A, Rook),
                    (B, Knight),
                    (C, Bishop),
                    (D, Queen),
                    (E, King),
                    (F, Bishop),
                    (G, Knight),
                    (H, Rook),
                ] {
                    let square = Square { rank, file };
                    let kind = if rank_perspective == Rank::R1 {
                        piece_kind
                    } else {
                        PieceKind::Pawn
                    };
                    pieces[square.get_idx()] = Some(Piece {
                        player: *player,
                        kind,
                    });
                }
            }
        }

        Self {
            pieces,
            player_turn: Player::White,
            castling_rights: [CastlingRights::Both, CastlingRights::Both],
            en_passant_file: None,
            move_number: 0,
        }
    }
}

/// What rights a player has to castle
pub enum CastlingRights {
    /// No rights :(
    None,
    /// Can only castle kingside
    Kingside,
    /// Can only castle queenside
    Queenside,
    /// Can castle either side
    Both,
}

/// The representation of a piece
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Piece {
    /// Which player owns this piece
    pub player: Player,
    /// What kind of piece this is
    pub kind: PieceKind,
}

/// All of the possible kinds of pieces (colour-independent)
#[derive(Clone, Copy, PartialEq, Eq, Debug, druid::Data)]
#[allow(missing_docs)]
pub enum PieceKind {
    Pawn,
    Knight,
    Bishop,
    Rook,
    King,
    Queen,
}

/// That's right, it's a square
#[derive(Copy, Clone, PartialEq, Eq, druid::Data)]
pub struct Square {
    /// This square's file
    #[data(same_fn = "PartialEq::eq")]
    pub file: File,
    /// This square's rank
    #[data(same_fn = "PartialEq::eq")]
    pub rank: Rank,
}

impl Square {
    /// Get the unique index of this square. Runs from 0 for A1 to 63 for H8.
    pub fn get_idx(self) -> usize {
        let file_idx = self.file.get_idx();
        let rank_idx = self.rank.get_idx();
        8 * rank_idx + file_idx
    }
}

/// No way, it's all the different files
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

/// You'd better believe these bad boys are all the possible ranks
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

/// The two opposing colours
#[derive(druid::Data, Clone, PartialEq, Copy, Eq)]
#[allow(missing_docs)]
pub enum Player {
    White,
    Black,
}

impl Player {
    /// Yield an iterator visiting each Player
    pub fn iter() -> impl std::iter::Iterator<Item = &'static Player> {
        static PLAYERS: [Player; 2] = [Player::White, Player::Black];
        PLAYERS.iter()
    }
}
