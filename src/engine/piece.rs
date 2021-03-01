//! Representation of pieces and moves, as well as logic about how the pieces
//! move

use super::constants::{DIAGONALS, KING_MOVES, KNIGHT_MOVES, NON_DIAGONALS};
use super::square::{Rank, Square, Vec2};
use super::structs::{CastlingRights, ChessState, Player};

/// The representation of a piece
#[derive(Clone, Copy, PartialEq, Eq, druid::Data, Debug)]
pub struct Piece {
    /// Which player owns this piece
    pub player: Player,
    /// What kind of piece this is
    pub kind: PieceKind,
}

impl Piece {
    /// Get all moves that this piece can take
    pub fn all_moves(&self, state: &ChessState, position: &Square) -> Vec<Move> {
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
                if position.rank == Rank::R2.get_from_perspective(&self.player)
                    && state.pieces[Square {
                        file: position.file,
                        rank: Rank::R3.get_from_perspective(&self.player),
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
                let mut capture_moves = Vec::new();
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
                    // Must be a capture to be permitted, so either the
                    // diagonal square must have a piece of the opposing
                    // colour, or it must be the en passant square
                    if let Some(sq) = possible_capture + position {
                        if state.pieces[sq.get_idx()].is_some()
                            && state.pieces[sq.get_idx()].unwrap().player != self.player
                        {
                            // Normal capture
                            capture_moves.push(Move {
                                from: *position,
                                to: sq,
                                move_type: MoveType::Capture(state.pieces[sq.get_idx()].unwrap()),
                            });
                        } else if let Some(en_passant_square) = state.en_passant_square {
                            if en_passant_square == sq {
                                // Capture en passant
                                capture_moves.push(Move {
                                    from: *position,
                                    to: sq,
                                    move_type: MoveType::EnPassant,
                                });
                            }
                        }
                    }
                }

                let mut all_moves = generate_moves(
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
            PieceKind::Bishop => generate_moves(
                position,
                &DIAGONALS,
                true,
                true,
                &state.pieces,
                &self.player,
            ),
            PieceKind::Knight => generate_moves(
                position,
                &KNIGHT_MOVES,
                false,
                true,
                &state.pieces,
                &self.player,
            ),
            PieceKind::Rook => generate_moves(
                position,
                &NON_DIAGONALS,
                true,
                true,
                &state.pieces,
                &self.player,
            ),
            PieceKind::King => {
                let mut all_simple_moves = generate_moves(
                    position,
                    &KING_MOVES,
                    false,
                    true,
                    &state.pieces,
                    &self.player,
                );

                // Check castling
                let rights = match self.player {
                    Player::White => &state.white_castling_rights,
                    Player::Black => &state.black_castling_rights,
                };
                if rights == &CastlingRights::Queenside || rights == &CastlingRights::Both {
                    // Check if we can castle queenside
                    let no_check_squares = [
                        *position,
                        position.left(1).unwrap(),
                        position.left(2).unwrap(),
                    ];

                    let empty_squares = [
                        position.left(1).unwrap(),
                        position.left(2).unwrap(),
                        position.left(3).unwrap(),
                    ];

                    if empty_squares
                        .iter()
                        .all(|sq| state.pieces[sq.get_idx()].is_none())
                        && no_check_squares
                            .iter()
                            .all(|sq| state.square_is_attacked(sq, &self.player) == 0)
                    {
                        all_simple_moves.push(Move {
                            from: *position,
                            to: position.left(2).unwrap(),
                            move_type: MoveType::Castle,
                        });
                    }
                }
                if rights == &CastlingRights::Kingside || rights == &CastlingRights::Both {
                    // Check if we can castle kingside
                    let no_check_squares = [
                        *position,
                        position.right(1).unwrap(),
                        position.right(2).unwrap(),
                    ];

                    let empty_squares = [position.right(1).unwrap(), position.right(2).unwrap()];

                    if empty_squares
                        .iter()
                        .all(|sq| state.pieces[sq.get_idx()].is_none())
                        && no_check_squares
                            .iter()
                            .all(|sq| state.square_is_attacked(sq, &self.player) == 0)
                    {
                        all_simple_moves.push(Move {
                            from: *position,
                            to: position.right(2).unwrap(),
                            move_type: MoveType::Castle,
                        });
                    }
                }
                all_simple_moves
            }
            PieceKind::Queen => generate_moves(
                position,
                &KING_MOVES,
                true,
                true,
                &state.pieces,
                &self.player,
            ),
        }
    }

    /// Construct a Piece from the FEN descriptor
    ///
    /// ```
    /// use cress::engine::{Piece, PieceKind, Player};
    /// assert_eq!(Piece::from_fen_char(&'N'), Some(Piece{kind: PieceKind::Knight, player: Player::White}));
    /// assert_eq!(Piece::from_fen_char(&'q'), Some(Piece{kind: PieceKind::Queen, player: Player::Black}));
    /// assert_eq!(Piece::from_fen_char(&'w'), None);
    /// ```
    pub fn from_fen_char(c: &char) -> Option<Self> {
        PieceKind::from_fen_char(c).map(|kind| Piece {
            kind,
            player: if c.is_ascii_uppercase() {
                Player::White
            } else {
                Player::Black
            },
        })
    }
}

/// Generate all moves possible given
/// - a piece position and colour,
/// - the set of "single" moves the piece can make (directions)
/// - whether or not the piece can move "repeatedly" (i.e. queen, bishop, rook)
///   or just once (pawn, king, knight)
/// - whether or not the piece can capture (allows generation of forward moves
///   for pawns)
/// - the piece positions
///
/// This is basic logic that doesn't take discovered checks into account, and
/// only produces moves of type [`MoveType::Simple`] and [`MoveType::Capture`]
fn generate_moves(
    position: &Square,
    directions: &[Vec2],
    repeat: bool,
    can_capture: bool,
    pieces: &[Option<Piece>],
    player: &Player,
) -> Vec<Move> {
    let mut all_moves = Vec::new();
    for direction in directions.iter() {
        if let Some(to) = direction + position {
            // Square is inside the board
            let capture =
                pieces[to.get_idx()].is_some() && pieces[to.get_idx()].unwrap().player != *player;
            if pieces[to.get_idx()].is_none() || capture && can_capture {
                all_moves.push(Move {
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
                    all_moves.push(Move {
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

impl PieceKind {
    /// Construct a PieceKind from the FEN descriptor (case-insensitive)
    ///
    /// ```
    /// use cress::engine::PieceKind;
    /// assert_eq!(PieceKind::from_fen_char(&'N'), Some(PieceKind::Knight));
    /// assert_eq!(PieceKind::from_fen_char(&'r'), Some(PieceKind::Rook));
    /// assert_eq!(PieceKind::from_fen_char(&'x'), None);
    /// ```
    pub fn from_fen_char(c: &char) -> Option<PieceKind> {
        match c.to_ascii_uppercase() {
            'P' => Some(PieceKind::Pawn),
            'N' => Some(PieceKind::Knight),
            'B' => Some(PieceKind::Bishop),
            'R' => Some(PieceKind::Rook),
            'K' => Some(PieceKind::King),
            'Q' => Some(PieceKind::Queen),
            _ => None,
        }
    }
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


/// The type of move
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum MoveType {
    /// A (possibly illegal) move attempted by a non-computer player. During
    /// move validation, a move of this type is first converted into a move of
    /// some other [`MoveType`] before being applied to the board.
    Unknown,
    /// An illegal move
    Illegal,
    /// A non-capturing move
    Simple,
    /// A move which captures a piece
    Capture(Piece),
    /// A pawn capture en-passant
    EnPassant,
    /// A castle
    Castle,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::File;
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
