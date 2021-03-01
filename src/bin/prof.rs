//! Binary to use with flamegraph to figure out the slow bits of move generation

use cress::engine::ChessState;

fn main() {
    let state = ChessState::new_game();
    // Hyperfine - 1.882s
    println!("{}", generate_moves(4, &state));
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
