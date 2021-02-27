//! Structures and types used in the main chess engine.

use super::constants::{DIAGONALS, KING_MOVES, KNIGHT_MOVES, NON_DIAGONALS};
use std::collections::HashSet;
use std::hash::{Hash, Hasher};

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
    /// If `Some`, the square on which a pawn may be taken en passant
    pub en_passant_square: Option<Square>,
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
            en_passant_square: None,
            move_number: 0,
        }
    }

    /// If passed a valid move, update the state accordingly
    ///
    /// Return the MoveResult.
    pub fn make_move(&mut self, chess_move: &Move) -> MoveResult {
        let move_type = self.get_move_type(&chess_move);
        if move_type != MoveType::Illegal {
            self.make_move_unchecked(&Move {
                from: chess_move.from,
                to: chess_move.to,
                move_type,
            });
            if self.generate_all_legal_moves(&self.player_turn).is_empty() {
                // The oppposing player cannot make any moves
                if self.is_player_in_check(&self.player_turn) != 0 {
                    // Checkmate
                    MoveResult::Win(self.player_turn.swap())
                } else {
                    // Stalemate
                    MoveResult::Stalemate
                }
            } else {
                MoveResult::Continue
            }
        } else {
            MoveResult::Illegal
        }
    }

    /// Update the state following a move, without checking whether or not the
    /// move is legal first.
    fn make_move_unchecked(&mut self, chess_move: &Move) {
        let piece = self.pieces[chess_move.from.get_idx()]
            .expect("Move is pseudo-legal so the from square must be a piece");
        self.pieces[chess_move.to.get_idx()] = Some(piece);
        self.pieces[chess_move.from.get_idx()] = None;
        if chess_move.move_type == MoveType::EnPassant {
            // Also need to remove the pawn taken en passant
            self.pieces[Square {
                file: chess_move.to.file,
                rank: Rank::R5.get_from_perspective(&piece.player),
            }
            .get_idx()] = None;
        }

        self.en_passant_square = get_en_passant_square(chess_move, &piece);

        self.player_turn = self.player_turn.swap();
    }

    /// Get the MoveType for a move of unknown type
    ///
    /// Includes
    /// - checking for self-captures
    /// - all pawn moves (promotion TODO)
    fn get_move_type(&self, chess_move: &Move) -> MoveType {
        println!(
            "All legal moves: {:?}",
            self.generate_all_legal_moves(&self.player_turn)
        );
        if let Some(categorised_move) = self
            .generate_all_legal_moves(&self.player_turn)
            .iter()
            .find(|m| m == &chess_move)
        {
            categorised_move.move_type
        } else {
            MoveType::Illegal
        }
    }

    /// Whether or not the player is in check
    ///
    /// Returns the number of checking pieces
    fn is_player_in_check(&self, player: &Player) -> usize {
        // Find the king - (TODO keep map of pieces to locations to make this
        // faster)
        let king_idx = self
            .pieces
            .iter()
            .position(|&p| {
                p == Some(Piece {
                    kind: PieceKind::King,
                    player: *player,
                })
            })
            .expect("There must be a king!");
        let king_square = Square::from_idx(king_idx)
            .expect("self.pieces has a length of 64 so all indices are valid");

        // If any of the moves that the other player can make are a capture of
        // the king, the king is in check.
        self.generate_all_pseudo_moves(&player.swap())
            .iter()
            .filter(|m| m.to == king_square)
            .count()
    }

    /// Generate all of the moves that a player can make, disregarding whether
    /// or not that move would put the player into check.
    // TODO: speed this up by keeping map of piece -> location
    fn generate_all_pseudo_moves(&self, player: &Player) -> HashSet<Move> {
        let mut all_pseudo_moves = HashSet::new();
        for (idx, maybe_piece) in self.pieces.iter().enumerate() {
            if let Some(piece) = maybe_piece {
                if &piece.player == player {
                    all_pseudo_moves.extend(piece.all_moves(&self, &Square::from_idx(idx).unwrap()))
                }
            }
        }
        all_pseudo_moves
    }

    /// Generate all of the legal moves that a player can make.
    // This can definitely be a lot faster!
    fn generate_all_legal_moves(&self, player: &Player) -> HashSet<Move> {
        let mut all_legal_moves = HashSet::new();
        let all_pseudo_moves = self.generate_all_pseudo_moves(player);
        for chess_move in all_pseudo_moves.into_iter() {
            let mut new_state = self.clone();
            new_state.make_move_unchecked(&chess_move);
            if new_state.is_player_in_check(player) == 0 {
                // The king is not in check - this is a legal move
                all_legal_moves.insert(chess_move);
            }
        }
        all_legal_moves
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
#[derive(Clone, Copy, PartialEq, Eq, druid::Data, Debug, Hash)]
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
                let mut non_capture_moves = vec![Vec2 {
                    delta_f: 0,
                    delta_r: direction,
                }];

                // If on the second (white) or seventh (black) rank, can move 2
                // squares, as long as there's no piece in between
                if position.rank == Rank::R2
                    && self.player == Player::White
                    && state.pieces[Square {
                        file: position.file,
                        rank: Rank::R3,
                    }
                    .get_idx()]
                    .is_none()
                    || position.rank == Rank::R7
                        && self.player == Player::Black
                        && state.pieces[Square {
                            file: position.file,
                            rank: Rank::R6,
                        }
                        .get_idx()]
                        .is_none()
                {
                    non_capture_moves.push(Vec2 {
                        delta_f: 0,
                        delta_r: 2 * direction,
                    });
                }

                // Pawns capture one square diagonally
                let mut capture_moves = HashSet::new();
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
                    // Must be a capture to be permitted
                    if let Some(sq) = possible_capture + position {
                        if state.pieces[sq.get_idx()].is_some()
                            && state.pieces[sq.get_idx()].unwrap().player != self.player
                        {
                            // Normal capture
                            capture_moves.insert(Move {
                                from: *position,
                                to: sq,
                                move_type: MoveType::Capture(state.pieces[sq.get_idx()].unwrap()),
                            });
                        } else if let Some(en_passant_square) = state.en_passant_square {
                            if en_passant_square == sq {
                                // Capture en passant
                                capture_moves.insert(Move {
                                    from: *position,
                                    to: sq,
                                    move_type: MoveType::EnPassant,
                                });
                            }
                        }
                    }
                }

                let mut all_moves = gen_moves(
                    position,
                    &non_capture_moves,
                    false,
                    false,
                    &state.pieces,
                    &self.player,
                );
                all_moves.extend(capture_moves);

                all_moves
            }
            PieceKind::Bishop => gen_moves(
                position,
                &DIAGONALS,
                true,
                true,
                &state.pieces,
                &self.player,
            ),
            PieceKind::Knight => gen_moves(
                position,
                &KNIGHT_MOVES,
                false,
                true,
                &state.pieces,
                &self.player,
            ),
            PieceKind::Rook => gen_moves(
                position,
                &NON_DIAGONALS,
                true,
                true,
                &state.pieces,
                &self.player,
            ),
            PieceKind::King => gen_moves(
                position,
                &KING_MOVES,
                false,
                true,
                &state.pieces,
                &self.player,
            ),
            PieceKind::Queen => gen_moves(
                position,
                &KING_MOVES,
                true,
                true,
                &state.pieces,
                &self.player,
            ),
        }
    }
}

/// Generate all moves possible given
/// - a piece position and colour,
/// - the set of "single" moves the piece can make (directions)
/// - whether or not the piece can move "repeatedly" (i.e. queen, bishop, rook)
///   or just once (pawn, king, knight)
/// - whether or not the piece can capture (allows generation of forward moves
///   for pawns)
/// - the board state
fn gen_moves(
    position: &Square,
    directions: &[Vec2],
    repeat: bool,
    can_capture: bool,
    pieces: &[Option<Piece>],
    player: &Player,
) -> HashSet<Move> {
    let mut all_moves = HashSet::new();
    for direction in directions.iter() {
        if let Some(to) = direction + position {
            // Square is inside the board
            let capture =
                pieces[to.get_idx()].is_some() && pieces[to.get_idx()].unwrap().player != *player;
            if pieces[to.get_idx()].is_none() || capture && can_capture {
                all_moves.insert(Move {
                    from: *position,
                    to,
                    move_type: if capture {
                        MoveType::Capture(pieces[to.get_idx()].unwrap())
                    } else {
                        MoveType::Simple
                    },
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
                if pieces[to.get_idx()].is_none() || capture && can_capture {
                    all_moves.insert(Move {
                        from: *position,
                        to,
                        move_type: if capture {
                            MoveType::Capture(pieces[to.get_idx()].unwrap())
                        } else {
                            MoveType::Simple
                        },
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
#[derive(Clone, Copy, PartialEq, Eq, Debug, druid::Data, Hash)]
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

    /// Construct a square given its file and rank indices.
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

    /// Construct a square given the unique index.
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

impl std::fmt::Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let letter: char = (self.get_idx() as u8 + 'A' as u8) as char;
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

impl std::fmt::Display for Rank {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_idx() + 1)
    }
}

/// The two opposing colours
#[derive(druid::Data, Clone, PartialEq, Copy, Eq, Debug, Hash)]
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

/// The type of move
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum MoveType {
    /// A (possibly illegal) human move
    Unknown,
    /// An illegal move
    Illegal,
    /// A non-capturing CPU move
    Simple,
    /// A CPU move which captures a piece
    Capture(Piece),
    /// A pawn capture en-passant
    EnPassant,
    /// A castle
    Castle,
    /// Promotion
    Promotion,
}

/// Representation of a chess move
#[derive(Debug)]
pub struct Move {
    /// The square that the piece has been moved from
    pub from: Square,
    /// The square that the piece is moving to
    pub to: Square,
    /// The type of move
    pub move_type: MoveType,
}

impl std::fmt::Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.from, self.to)
    }
}

impl PartialEq for Move {
    fn eq(&self, other: &Self) -> bool {
        self.from == other.from && self.to == other.to
    }
}

impl Eq for Move {}

impl Hash for Move {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.from.hash(state);
        self.to.hash(state);
    }
}

/// The result of a move
pub enum MoveResult {
    /// The move was not legal
    Illegal,
    /// The game is now a stalemate
    Stalemate,
    /// Checkmate! Specify the player who won
    Win(Player),
    /// Nothing special, the game continues
    Continue,
}

impl std::fmt::Display for MoveResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use MoveResult::*;
        let iden = match self {
            Illegal => "That move was illegal",
            Stalemate => "The game is now a stalemate",
            Win(colour) => match colour {
                Player::White => "Checkmate! The game is a win for white",
                Player::Black => "Checkmate! The game is a win for black",
            },
            Continue => "",
        };
        write!(f, "{}", iden)
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
        Square::from_idxs(file_idx, rank_idx)
    }
}

// ----------------------------------------------------------------------------
// Helper functions

/// If the move is a double pawn move, i.e. one that would allow the pawn to be
/// taken en passant, return the square on which the capture can take place.
/// Otherwise, return None
fn get_en_passant_square(chess_move: &Move, piece: &Piece) -> Option<Square> {
    if piece.kind == PieceKind::Pawn
        && (chess_move.from.rank.get_from_perspective(&piece.player) == Rank::R2
            && chess_move.to.rank.get_from_perspective(&piece.player) == Rank::R4)
    {
        Some(Square {
            file: chess_move.from.file,
            rank: Rank::R3.get_from_perspective(&piece.player),
        })
    } else {
        None
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

    fn generate_moves(depth: u8, state: &ChessState) -> u64 {
        if depth == 0 {
            1
        } else {
            let mut sum = 0;
            for chess_move in state.generate_all_legal_moves(&state.player_turn) {
                let mut new_state = state.clone();
                // Make move
                new_state.make_move_unchecked(&chess_move);
                sum += generate_moves(depth - 1, &new_state);
            }
            sum
        }
    }
    #[test]
    fn test_initial_moves() {
        let state = ChessState::new_game();
        assert_eq!(generate_moves(1, &state), 20);
        println!("Done with 1");
        assert_eq!(generate_moves(2, &state), 400);
        println!("Done with 2");
        assert_eq!(generate_moves(3, &state), 8902);
        println!("Done with 3");
        assert_eq!(generate_moves(4, &state), 197_281);
        println!("Done with 4");
        assert_eq!(generate_moves(5, &state), 4_865_609);
    }
}
