use super::structs::ChessState;
use crate::engine::evaluate::Evaluate;

pub fn minimax(state: &ChessState, depth: u8, maximise: bool) -> f64 {
    if depth == 0 {
        return state.evaluate();
    }
    let mut board_value;
    if maximise {
        board_value = f64::MIN;
        for chess_move in state.generate_all_legal_moves(&state.player_turn) {
            let mut new_state = state.clone();
            // Make move
            new_state.make_move_unchecked(&chess_move);
            let child_value = minimax(&new_state, depth - 1, !maximise);
            if child_value > board_value {
                board_value = child_value;
            }
        }
    } else {
        board_value = f64::MAX;
        for chess_move in state.generate_all_legal_moves(&state.player_turn) {
            let mut new_state = state.clone();
            // Make move
            new_state.make_move_unchecked(&chess_move);
            let child_value = minimax(&new_state, depth - 1, !maximise);
            if child_value < board_value {
                board_value = child_value;
            }
        }
    }

    board_value
}
