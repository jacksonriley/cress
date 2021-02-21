//! Constants for use in the core engine

use super::structs::Vec2;

/// The one-square diagonal moves
pub const DIAGONALS: [Vec2; 4] = [
    Vec2 {
        delta_f: 1,
        delta_r: 1,
    },
    Vec2 {
        delta_f: 1,
        delta_r: -1,
    },
    Vec2 {
        delta_f: -1,
        delta_r: 1,
    },
    Vec2 {
        delta_f: -1,
        delta_r: -1,
    },
];

// TODO: better name for this lol
/// The non-diagonal one-square moves
pub const NON_DIAGONALS: [Vec2; 4] = [
    Vec2 {
        delta_f: 1,
        delta_r: 0,
    },
    Vec2 {
        delta_f: -1,
        delta_r: 0,
    },
    Vec2 {
        delta_f: 0,
        delta_r: 1,
    },
    Vec2 {
        delta_f: 0,
        delta_r: -1,
    },
];

/// The Knight moves
pub const KNIGHT_MOVES: [Vec2; 8] = [
    Vec2 {
        delta_f: 2,
        delta_r: 1,
    },
    Vec2 {
        delta_f: 2,
        delta_r: -1,
    },
    Vec2 {
        delta_f: -2,
        delta_r: 1,
    },
    Vec2 {
        delta_f: -2,
        delta_r: -1,
    },
    Vec2 {
        delta_f: 1,
        delta_r: 2,
    },
    Vec2 {
        delta_f: -1,
        delta_r: 2,
    },
    Vec2 {
        delta_f: 1,
        delta_r: -2,
    },
    Vec2 {
        delta_f: -1,
        delta_r: -2,
    },
];

// TODO: better way to define this?
/// All one-square moves - i.e. those that the King can make
pub const KING_MOVES: [Vec2; 8] = [
    Vec2 {
        delta_f: 1,
        delta_r: 0,
    },
    Vec2 {
        delta_f: -1,
        delta_r: 0,
    },
    Vec2 {
        delta_f: 0,
        delta_r: 1,
    },
    Vec2 {
        delta_f: 0,
        delta_r: -1,
    },
    Vec2 {
        delta_f: 1,
        delta_r: 1,
    },
    Vec2 {
        delta_f: 1,
        delta_r: -1,
    },
    Vec2 {
        delta_f: -1,
        delta_r: 1,
    },
    Vec2 {
        delta_f: -1,
        delta_r: -1,
    },
];
