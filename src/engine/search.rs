use super::structs::ChessState;
use crate::engine::evaluate::Evaluate;
use std::cmp;

pub fn minimax(
    state: &ChessState,
    depth: u8,
    maximise: bool,
    mut alpha: i32,
    mut beta: i32,
) -> i32 {
    if depth == 0 {
        return state.evaluate();
    }
    let mut board_value;
    if maximise {
        board_value = i32::MIN;
        for chess_move in state.generate_all_legal_moves(&state.player_turn) {
            let mut new_state = state.clone();
            // Make move
            new_state.make_move_unchecked(&chess_move);
            let child_value = minimax(&new_state, depth - 1, !maximise, alpha, beta);
            if child_value > board_value {
                board_value = child_value;
            }
            alpha = cmp::max(board_value, alpha);
            if alpha >= beta {
                break;
            }
        }
    } else {
        board_value = i32::MAX;
        for chess_move in state.generate_all_legal_moves(&state.player_turn) {
            let mut new_state = state.clone();
            // Make move
            new_state.make_move_unchecked(&chess_move);
            let child_value = minimax(&new_state, depth - 1, !maximise, alpha, beta);
            if child_value < board_value {
                board_value = child_value;
            }
            beta = cmp::min(board_value, beta);
            if alpha >= beta {
                break;
            }
        }
    }

    board_value
}
