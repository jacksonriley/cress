//! Structures and types used in the main chess engine.

use super::piece::{Move, MoveType, Piece, PieceKind};
use super::square::{File, Rank, Square};
use std::str::FromStr;

/// All non-graphical game state
#[derive(Clone, druid::Data, Debug, PartialEq)]
pub struct ChessState {
    /// An array, running from A1 (index 0) to H8 (index 63), of [`Option<Piece>`]
    #[data(same_fn = "PartialEq::eq")]
    pub pieces: [Option<Piece>; 64],
    /// Which player's turn it is
    pub player_turn: Player,
    /// What castling rights are available to White
    pub white_castling_rights: CastlingRights,
    /// What castling rights are available to Black
    pub black_castling_rights: CastlingRights,
    /// If `Some`, the square on which a pawn may be taken en passant
    pub en_passant_square: Option<Square>,
    /// How many total full-moves have been taken
    pub full_move_number: u16,
    /// How many half-moves have been taken since the last capture or pawn
    /// advance
    pub half_move_number: u8,
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
            white_castling_rights: CastlingRights::Both,
            black_castling_rights: CastlingRights::Both,
            en_passant_square: None,
            half_move_number: 0,
            full_move_number: 1,
        }
    }

    /// Construct a ChessState from a FEN string, if in valid format.
    ///
    /// For example, the initial position is represented as:
    /// rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
    ///  - lowercase letters for Black pieces
    ///  - uppercase letters for White pieces
    ///  - slash for next rank down
    ///  - number to skip that number of files
    ///
    ///  - which player is to move
    ///
    ///  - castling rights - uppercase for White, lowercase for Black, a hyphen
    ///    if neither side can castle
    ///
    ///  - the en passant square, if there is one. If not, a hyphen
    ///
    ///  - the halfmove clock (number of halfmoves since the last capture or
    ///    pawn advance)
    ///
    /// - the fullmove number (starts at 1, incremented after Black's move)
    pub fn from_fen(fen: &str) -> Result<Self, String> {
        let toks: Vec<&str> = fen.split(' ').collect();
        if toks.len() != 6 {
            return Err("Invalid FEN format".to_string());
        }

        // Piece positions
        let pieces: Vec<char> = toks[0].chars().collect();
        let mut piece_array = [None; 64];
        // The pieces are ordered from A8 -> H8, A7-> H7 etc.
        let mut file = File::A;
        let mut rank = Rank::R8;
        for c in pieces {
            match c {
                '1'..='8' => {
                    // Skip this number of files
                    let digit = c
                        .to_digit(10)
                        .ok_or(format!("Could not parse {} as digit", c))?
                        as isize;
                    if file.get_idx() + digit as usize == 8 {
                        // Skip the rest of the rank - the next character should be '/'
                        continue;
                    }
                    file = (&file + &digit).ok_or(format!("Invalid file skip length: {}", c))?
                }
                '/' => {
                    // Next rank down
                    rank = (&rank + &-1).ok_or("Went out of bounds when parsing FEN: /")?;
                    file = File::A;
                }
                _ => {
                    // Anything else should be a piece
                    piece_array[Square { file, rank }.get_idx()] = Some(
                        Piece::from_fen_char(&c)
                            .ok_or(format!("{} is not a valid piece character", c))?,
                    );
                    // Increment the file (not if at the right of the board)
                    if file != File::H {
                        file = (&file + &1)
                            .ok_or(format!("Went out of bounds when parsing FEN: {}", c))?;
                    }
                }
            }
        }

        // Player turn
        let player_turn: Player = match toks[1] {
            "w" => Player::White,
            "b" => Player::Black,
            _ => return Err(format!("Got invalid player: {}", toks[1])),
        };

        // Castling rights
        let mut white_castling_rights = CastlingRights::None;
        let mut black_castling_rights = CastlingRights::None;
        if toks[2].contains("K") {
            white_castling_rights.add_kingside();
        }
        if toks[2].contains("Q") {
            white_castling_rights.add_queenside();
        }
        if toks[2].contains("k") {
            black_castling_rights.add_kingside();
        }
        if toks[2].contains("q") {
            black_castling_rights.add_queenside();
        }

        // En passant square
        let en_passant_square;
        if toks[3] == "-" {
            en_passant_square = None;
        } else {
            en_passant_square = Some(Square::from_str(toks[3])?);
        }

        // Half-move number
        let half_move_number = toks[4]
            .parse::<u8>()
            .map_err(|_| format!("Invalid half-move number: {}", toks[4]))?;

        // Full-move number
        let full_move_number = toks[5]
            .parse::<u16>()
            .map_err(|_| format!("Invalid full-move number: {}", toks[5]))?;

        Ok(ChessState {
            pieces: piece_array,
            player_turn,
            white_castling_rights,
            black_castling_rights,
            en_passant_square,
            full_move_number,
            half_move_number,
        })
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
    pub fn make_move_unchecked(&mut self, chess_move: &Move) {
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
        } else if chess_move.move_type == MoveType::Castle {
            // Need to move the rook as well
            if chess_move.to.file == File::G {
                // Kingside - move the H rook
                let rook = self.pieces[chess_move.to.right(1).unwrap().get_idx()].take();
                self.pieces[chess_move.to.left(1).unwrap().get_idx()] = rook;
            } else {
                // Queenside - move the A rook
                let rook = self.pieces[chess_move.to.left(2).unwrap().get_idx()].take();
                self.pieces[chess_move.to.right(1).unwrap().get_idx()] = rook;
            }
        }

        if piece.kind == PieceKind::Pawn
            && chess_move.to.rank.get_from_perspective(&self.player_turn) == Rank::R8
        {
            // Promote to a queen for now
            // TODO: allow choice of promotion piece
            self.pieces[chess_move.to.get_idx()] = Some(Piece {
                kind: PieceKind::Queen,
                player: self.player_turn,
            });
        }

        self.modify_castling_rights(&chess_move, &piece);

        self.en_passant_square = get_en_passant_square(chess_move, &piece);

        self.player_turn = self.player_turn.swap();
    }

    /// Get the MoveType for a move of unknown type
    ///
    /// Includes
    /// - checking for self-captures
    /// - all pawn moves (promotion TODO)
    fn get_move_type(&self, chess_move: &Move) -> MoveType {
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

        self.square_is_attacked(&king_square, &player)
    }

    /// Whether or not a square is attacked
    ///
    /// Returns the number of attackers
    pub fn square_is_attacked(&self, square: &Square, player: &Player) -> usize {
        // If any of the moves that the other player can make are a capture of
        // this square, it is attacked.
        // In order to check pawn threats, make sure there's a piece on the
        // square in question (doesn't matter what kind)
        let mut cloned_state = self.clone();
        cloned_state.pieces[square.get_idx()] = Some(Piece {
            kind: PieceKind::King,
            player: *player,
        });

        // Bit of a hack to avoid an infinite loop - don't consider castling as
        // you can't capture by castling.
        // The infinite loop arises when checking if castling can be included
        // in the pseudo moves, as then you need to check if any of the
        // castling squares are attacked.
        cloned_state.white_castling_rights = CastlingRights::None;
        cloned_state.black_castling_rights = CastlingRights::None;

        cloned_state
            .generate_all_pseudo_moves(&player.swap())
            .iter()
            .filter(|m| m.to == *square)
            .count()
    }

    /// Generate all of the moves that a player can make, disregarding whether
    /// or not that move would put the player into check.
    // TODO: speed this up by keeping map of piece -> location
    fn generate_all_pseudo_moves(&self, player: &Player) -> Vec<Move> {
        let mut all_pseudo_moves = Vec::new();
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
    pub fn generate_all_legal_moves(&self, player: &Player) -> Vec<Move> {
        let mut all_legal_moves = Vec::new();
        let all_pseudo_moves = self.generate_all_pseudo_moves(player);
        for chess_move in all_pseudo_moves.into_iter() {
            let mut new_state = self.clone();
            new_state.make_move_unchecked(&chess_move);
            if new_state.is_player_in_check(player) == 0 {
                // The king is not in check - this is a legal move
                all_legal_moves.push(chess_move);
            }
        }
        all_legal_moves
    }

    /// Update castling rights following a move
    ///
    /// Relevant scenarios are:
    ///  - a king move from the starting square
    ///  - a rook move from a starting square
    ///  - the capture of a rook on a starting square
    fn modify_castling_rights(&mut self, chess_move: &Move, piece: &Piece) {
        // King move
        if piece.kind == PieceKind::King {
            match self.player_turn {
                Player::White => self.white_castling_rights = CastlingRights::None,
                Player::Black => self.black_castling_rights = CastlingRights::None,
            }
        } else if piece.kind == PieceKind::Rook {
            // Rook move
            if chess_move.from
                == (Square {
                    file: File::A,
                    rank: Rank::R1.get_from_perspective(&self.player_turn),
                })
            {
                match self.player_turn {
                    Player::White => self.white_castling_rights.no_queenside(),
                    Player::Black => self.black_castling_rights.no_queenside(),
                };
            } else if chess_move.from
                == (Square {
                    file: File::H,
                    rank: Rank::R1.get_from_perspective(&self.player_turn),
                })
            {
                match self.player_turn {
                    Player::White => self.white_castling_rights.no_kingside(),
                    Player::Black => self.black_castling_rights.no_kingside(),
                };
            }
        }

        // Rook capture
        if chess_move.move_type
            == MoveType::Capture(Piece {
                kind: PieceKind::Rook,
                player: self.player_turn.swap(),
            })
        {
            if chess_move.to
                == (Square {
                    file: File::A,
                    rank: Rank::R8.get_from_perspective(&self.player_turn),
                })
            {
                match self.player_turn {
                    Player::White => self.black_castling_rights.no_queenside(),
                    Player::Black => self.white_castling_rights.no_queenside(),
                };
            } else if chess_move.to
                == (Square {
                    file: File::H,
                    rank: Rank::R8.get_from_perspective(&self.player_turn),
                })
            {
                match self.player_turn {
                    Player::White => self.black_castling_rights.no_kingside(),
                    Player::Black => self.white_castling_rights.no_kingside(),
                };
            }
        }
    }
}

/// What rights a player has to castle
#[derive(Clone, druid::Data, PartialEq, Debug)]
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

impl CastlingRights {
    /// Remove queenside castling rights
    fn no_queenside(&mut self) {
        use CastlingRights::*;
        *self = match self {
            Both => Kingside,
            Queenside => None,
            Kingside => Kingside,
            None => None,
        };
    }

    /// Remove kingside castling rights
    fn no_kingside(&mut self) {
        use CastlingRights::*;
        *self = match self {
            Both => Queenside,
            Queenside => Queenside,
            Kingside => None,
            None => None,
        };
    }

    /// Add queenside castling rights
    fn add_queenside(&mut self) {
        use CastlingRights::*;
        *self = match self {
            Both => Both,
            Queenside => Queenside,
            Kingside => Both,
            None => Queenside,
        };
    }

    /// Add kingside castling rights
    fn add_kingside(&mut self) {
        use CastlingRights::*;
        *self = match self {
            Both => Both,
            Queenside => Both,
            Kingside => Kingside,
            None => Kingside,
        };
    }
}

/// The two opposing colours
#[derive(druid::Data, Clone, PartialEq, Copy, Eq, Debug)]
#[allow(missing_docs)]
pub enum Player {
    White,
    Black,
}

impl Player {
    /// Yield an iterator visiting each Player
    fn iter() -> impl std::iter::Iterator<Item = &'static Player> {
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
            Illegal => "That move is illegal",
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
        // Test the number of moves possible from the starting position.
        // Compared against https://www.chessprogramming.org/Perft_Results#cite_note-1
        let state = ChessState::new_game();
        assert_eq!(generate_moves(1, &state), 20);
        assert_eq!(generate_moves(2, &state), 400);
        assert_eq!(generate_moves(3, &state), 8902);
        assert_eq!(generate_moves(4, &state), 197_281);
        // Takes about a minute with the current code
        // assert_eq!(generate_moves(5, &state), 4_865_609);
    }

    #[test]
    fn test_position_two() -> Result<(), String> {
        let state = ChessState::from_fen(
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
        )?;
        assert_eq!(generate_moves(1, &state), 48);
        assert_eq!(generate_moves(2, &state), 2039);
        assert_eq!(generate_moves(3, &state), 97_862);
        // TODO: Wrong because of promotions (nothing other than queen done yet)
        // assert_eq!(generate_moves(4, &state), 4_085_603);
        Ok(())
    }

    #[test]
    fn test_position_three() -> Result<(), String> {
        let state = ChessState::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")?;
        assert_eq!(generate_moves(1, &state), 14);
        assert_eq!(generate_moves(2, &state), 191);
        assert_eq!(generate_moves(3, &state), 2812);
        assert_eq!(generate_moves(4, &state), 43_238);
        // Correct but slow
        // assert_eq!(generate_moves(5, &state), 674_624);
        Ok(())
    }

    #[test]
    fn test_position_four() -> Result<(), String> {
        let state = ChessState::from_fen(
            "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
        )?;
        assert_eq!(generate_moves(1, &state), 6);
        // TODO: Wrong (promotions)
        // assert_eq!(generate_moves(2, &state), 264);
        // assert_eq!(generate_moves(3, &state), 9467);
        // assert_eq!(generate_moves(4, &state), 422_333);
        Ok(())
    }

    #[test]
    fn test_position_five() -> Result<(), String> {
        let state =
            ChessState::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")?;
        // TODO: Wrong (promotions)
        // assert_eq!(generate_moves(1, &state), 44);
        // assert_eq!(generate_moves(2, &state), 1486);
        // assert_eq!(generate_moves(3, &state), 62379);
        Ok(())
    }

    #[test]
    fn test_initial_fen() {
        // Test that the initial state FEN is parsed correctly
        let initial_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        assert_eq!(
            ChessState::from_fen(initial_fen),
            Ok(ChessState::new_game())
        );

        // Test that the FEN after 1.e4 is parsed correctly
        let mut after_e4 = ChessState::new_game();
        let pawn = after_e4.pieces[Square {
            file: File::E,
            rank: Rank::R2,
        }
        .get_idx()]
        .take();
        after_e4.pieces[Square {
            file: File::E,
            rank: Rank::R4,
        }
        .get_idx()] = pawn;
        after_e4.en_passant_square = Some(Square {
            file: File::E,
            rank: Rank::R3,
        });
        after_e4.player_turn = Player::Black;
        let after_e4_fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1";
        assert_eq!(ChessState::from_fen(after_e4_fen), Ok(after_e4));
    }
}
