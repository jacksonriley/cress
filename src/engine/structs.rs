//! Structures and types used in the main chess engine.

use super::constants::{DIAGONALS, KING_MOVES, KNIGHT_MOVES, NON_DIAGONALS};
use std::collections::HashSet;
use std::hash::Hash;

/// All non-graphical game state
#[derive(Clone, druid::Data)]
pub struct ChessState {
    /// An array, running from A1 (index 0) to H8 (index 63), of [`Option<Piece>`]
    #[data(same_fn = "PartialEq::eq")]
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

    /// If passed a valid move, update the state accordingly
    ///
    /// Return whether or not the move was valid.
    pub fn make_move(&mut self, chess_move: &Move) -> bool {
        if self.is_move_valid(&chess_move) {
            let piece = self.pieces[chess_move.from.get_idx()]
                .expect("Move is valid so the from square must be a piece");
            self.pieces[chess_move.to.get_idx()] = Some(piece);
            self.pieces[chess_move.from.get_idx()] = None;
            self.player_turn = self.player_turn.swap();
            true
        } else {
            false
        }
    }

    /// Calculate whether or not this move is valid, given the current game
    /// state.
    fn is_move_valid(&self, chess_move: &Move) -> bool {
        // The moving piece must be of the same colour as the player_turn
        let piece = match self.pieces[chess_move.from.get_idx()] {
            None => return false,
            Some(piece) => {
                if piece.player != self.player_turn {
                    return false;
                } else {
                    piece
                }
            }
        };

        // Check that this move is valid for this given piece (includes
        // checking for self-captures)
        if !self.is_move_valid_for_piece(chess_move, &piece) {
            return false;
        }
        // TODO: Check if the player is in check, or making this move would put
        // the player in check.
        // TODO: Check castling rights/allow castling
        // TODO: allow en passant
        // TODO: promotion!

        // If these conditions are satisfied, this is a legal move
        true
    }

    /// Check that a given move is valid for a given piece
    /// 
    /// Includes
    /// - checking for self-captures
    /// - all pawn moves (promotion and en passant TODO)
    fn is_move_valid_for_piece(&self, chess_move: &Move, piece: &Piece) -> bool {
        piece
            .all_moves(&self, &chess_move.from)
            .iter()
            .any(|m| m == chess_move)
    }
}

/// What rights a player has to castle
#[derive(Clone, druid::Data, PartialEq)]
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
#[derive(Clone, Copy, PartialEq, Eq, druid::Data)]
pub struct Piece {
    /// Which player owns this piece
    pub player: Player,
    /// What kind of piece this is
    pub kind: PieceKind,
}

impl Piece {
    /// Get all moves that this piece can take
    ///
    /// - TODO: En passant
    /// - TODO: Castling
    /// - TODO: Checks (should maybe be done before?)
    fn all_moves(&self, state: &ChessState, position: &Square) -> HashSet<Move> {
        match self.kind {
            PieceKind::Pawn => {
                // White pawns move up, black pawns move down
                let direction: isize = if self.player == Player::White { 1 } else { -1 };
                let mut moves = vec![Vec2 {
                    delta_f: 0,
                    delta_r: direction,
                }];

                // If on the second (white) or seventh (black) rank, can move 2 squares.
                if position.rank == Rank::R2 && self.player == Player::White
                    || position.rank == Rank::R7 && self.player == Player::Black
                {
                    moves.push(Vec2 {
                        delta_f: 0,
                        delta_r: 2 * direction,
                    });
                }

                // Pawns capture one square diagonally
                for possible_capture in [
                    Vec2 {
                        delta_f: -1,
                        delta_r: direction,
                    },
                    Vec2 {
                        delta_f: 1,
                        delta_r: direction,
                    },
                ]
                .iter()
                {
                    if let Some(sq) = possible_capture + position {
                        if state.pieces[sq.get_idx()].is_some()
                            && state.pieces[sq.get_idx()].unwrap().player != self.player
                        {
                            moves.push(*possible_capture)
                        }
                    }
                }

                // TODO: en passant

                gen_moves(position, &moves, false, &state.pieces, &self.player)
            }
            PieceKind::Bishop => gen_moves(position, &DIAGONALS, true, &state.pieces, &self.player),
            PieceKind::Knight => {
                gen_moves(position, &KNIGHT_MOVES, false, &state.pieces, &self.player)
            }
            PieceKind::Rook => {
                gen_moves(position, &NON_DIAGONALS, true, &state.pieces, &self.player)
            }
            PieceKind::King => gen_moves(position, &KING_MOVES, false, &state.pieces, &self.player),
            PieceKind::Queen => gen_moves(position, &KING_MOVES, true, &state.pieces, &self.player),
        }
    }
}

/// Generate all moves possible given
/// - a piece position and colour,
/// - the set of "single" moves the piece can make (directions)
/// - whether or not the piece can move "repeatedly" (i.e. queen, bishop, rook)
///   or just once (pawn, king, knight)
/// - the board state
fn gen_moves(
    position: &Square,
    directions: &[Vec2],
    repeat: bool,
    pieces: &[Option<Piece>],
    player: &Player,
) -> HashSet<Move> {
    let mut all_moves = HashSet::new();
    for direction in directions.iter() {
        if let Some(to) = direction + position {
            // Square is inside the board
            let capture =
                pieces[to.get_idx()].is_some() && pieces[to.get_idx()].unwrap().player != *player;
            if pieces[to.get_idx()].is_none() || capture {
                all_moves.insert(Move {
                    from: *position,
                    to,
                });
                if capture {
                    // Can't keep going after a capture
                    continue;
                }
            } else {
                // Can't take our own coloured piece
                continue;
            }
        } else {
            // Square outside the board
            continue;
        }

        if repeat {
            // Keep going, until we can't anymore in this direction
            let mut new_pos = (direction + position).unwrap();
            while let Some(to) = direction + &new_pos {
                let capture = pieces[to.get_idx()].is_some()
                    && pieces[to.get_idx()].unwrap().player != *player;
                if pieces[to.get_idx()].is_none() || capture {
                    all_moves.insert(Move {
                        from: *position,
                        to,
                    });
                    if capture {
                        // Can't keep going past a capture
                        break;
                    }
                    new_pos = to;
                } else {
                    break;
                }
            }
        }
    }
    all_moves
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
#[derive(Copy, Clone, PartialEq, Eq, druid::Data, Debug, Hash)]
pub struct Square {
    /// This square's file
    pub file: File,
    /// This square's rank
    pub rank: Rank,
}

impl Square {
    /// Get the unique index of this square. Runs from 0 for A1 to 63 for H8.
    pub fn get_idx(self) -> usize {
        let file_idx = self.file.get_idx();
        let rank_idx = self.rank.get_idx();
        8 * rank_idx + file_idx
    }

    /// Construct a square given the unique index.
    pub fn from_idxs(file_idx: isize, rank_idx: isize) -> Option<Self> {
        if file_idx < 0 || file_idx > 7 || rank_idx < 0 || rank_idx > 7 {
            // Out of bounds
            return None;
        }
        let file = File::from_idx(file_idx as usize);
        let rank = Rank::from_idx(rank_idx as usize);
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

    /// Yield the other colour from `self`
    pub fn swap(&self) -> Player {
        match self {
            Player::White => Player::Black,
            Player::Black => Player::White,
        }
    }
}

/// Representation of a chess move
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Move {
    /// The square that the piece has been moved from
    pub from: Square,
    /// The square that the piece is moving to
    pub to: Square,
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
        Square::from_idxs(file_idx, rank_idx)
    }
}

// ----------------------------------------------------------------------------
// Tests
// Possibly worth writing more of these
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_queen_moves() {
        let mut state = ChessState::new_game();
        state.pieces = [None; 64];
        let white_queen = Piece {
            player: Player::White,
            kind: PieceKind::Queen,
        };
        let black_queen = Piece {
            player: Player::Black,
            kind: PieceKind::Queen,
        };
        let d5 = Square {
            file: File::D,
            rank: Rank::R5,
        };
        let b3 = Square {
            file: File::B,
            rank: Rank::R3,
        };
        state.pieces[d5.get_idx()] = Some(white_queen);
        state.pieces[b3.get_idx()] = Some(black_queen);

        assert_eq!(white_queen.all_moves(&state, &d5).len(), 26);
        assert_eq!(black_queen.all_moves(&state, &b3).len(), 20);
    }
}
