//! Simple heuristic evaluation function
// Uses https://www.chessprogramming.org/Simplified_Evaluation_Function

use super::piece::PieceKind;
use super::structs::{ChessState, Player};

pub trait Evaluate {
    fn evaluate(&self) -> i32;
}

impl Evaluate for ChessState {
    fn evaluate(&self) -> i32 {
        let endgame = is_endgame(&self);

        let mut eval = 0;
        for idx in 0..64 {
            if let Some(piece) = self.pieces[idx] {
                let mut piece_eval = piece.kind.get_material_value();
                piece_eval += piece.get_positional_bonus(idx, endgame);
                if piece.player == Player::Black {
                    piece_eval = -piece_eval;
                }
                eval += piece_eval;
            }
        }
        eval
    }
}

/// Is the current game in the endgame or not
fn is_endgame(state: &ChessState) -> bool {
    // Judge endgame to be where any side with a queen has at most one other
    // major or minor piece
    let mut white_has_queen = false;
    let mut black_has_queen = false;
    let mut white_additional_pieces = 0;
    let mut black_additional_pieces = 0;
    for idx in 0..64 {
        if let Some(piece) = state.pieces[idx] {
            match piece.player {
                Player::White => match piece.kind {
                    PieceKind::Queen => white_has_queen = true,
                    PieceKind::Rook => white_additional_pieces += 1,
                    PieceKind::Bishop => white_additional_pieces += 1,
                    PieceKind::Knight => white_additional_pieces += 1,
                    _ => {}
                },
                Player::Black => match piece.kind {
                    PieceKind::Queen => black_has_queen = true,
                    PieceKind::Rook => black_additional_pieces += 1,
                    PieceKind::Bishop => black_additional_pieces += 1,
                    PieceKind::Knight => black_additional_pieces += 1,
                    _ => {}
                },
            }
        }
    }

    let mut endgame = true;
    if white_has_queen && white_additional_pieces > 1 {
        endgame = false;
    }
    if black_has_queen && black_additional_pieces > 1 {
        endgame = false;
    }

    endgame
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::Square;
    use std::str::FromStr;
    #[test]
    fn test_basic_eval() {
        let mut state = ChessState::new_game();
        assert_eq!(state.evaluate(), 0.0);

        // Remove the black queen
        state.pieces[Square::from_str("d8").unwrap().get_idx()] = None;
        assert_eq!(state.evaluate(), 895.0);

        // Advance a pawn to a better location
        let pawn = state.pieces[Square::from_str("e2").unwrap().get_idx()].take();
        state.pieces[Square::from_str("e4").unwrap().get_idx()] = pawn;
        assert_eq!(state.evaluate(), 935.0);
    }
}
