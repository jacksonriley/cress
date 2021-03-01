//! Run a simple chess GUI using [`druid`]. No real options yet, just call
//! [`main`].

use druid::{
    kurbo::TranslateScale,
    widget::{Align, SvgData},
    AppLauncher, BoxConstraints, Color, Data, Env, Event, EventCtx, LayoutCtx, LifeCycle,
    LifeCycleCtx, PaintCtx, Point, Rect, RenderContext, Size, UpdateCtx, Widget, WindowDesc,
};

use crate::engine::{
    ChessState, File, Move, MoveResult, MoveType, Piece, PieceKind, Player, Rank, Square,
};
use std::rc::Rc;
use std::str::FromStr;

// Constants
/// RGB colour of the white squares
const WHITE_SQUARE_COLOUR: Color = Color::rgb8(200, 200, 255);
/// RGB colour of the black squares - hand-picked by Kimbo
const BLACK_SQUARE_COLOUR: Color = Color::rgb8(18, 95, 128);
/// Scale of the board compared to the minimum side length of the window
const BOARD_SCALE: f64 = 0.9;

/// Main GUI function.
///
/// Creates the window, loads the chess piece svg files, and launches the app.
pub fn main(mode: Mode) {
    let main_window = WindowDesc::new(ui_builder)
        .title("Cress")
        .window_size((700.0, 700.0));

    let svg = |src| SvgData::from_str(src).unwrap();
    let piece_svgs = Rc::new([
        svg(include_str!("img/Chess_plt45.svg")),
        svg(include_str!("img/Chess_nlt45.svg")),
        svg(include_str!("img/Chess_blt45.svg")),
        svg(include_str!("img/Chess_rlt45.svg")),
        svg(include_str!("img/Chess_klt45.svg")),
        svg(include_str!("img/Chess_qlt45.svg")),
        svg(include_str!("img/Chess_pdt45.svg")),
        svg(include_str!("img/Chess_ndt45.svg")),
        svg(include_str!("img/Chess_bdt45.svg")),
        svg(include_str!("img/Chess_rdt45.svg")),
        svg(include_str!("img/Chess_kdt45.svg")),
        svg(include_str!("img/Chess_qdt45.svg")),
    ]);

    let state = ChessState::new_game();

    // TODO: choose starting player randomly or via CLI arg
    let player = Player::White;

    let game = ChessGame {
        state,
        piece_svgs,
        player,
        mode,
        last_clicked: None,
    };

    AppLauncher::with_window(main_window)
        .launch(game)
        .expect("launch failed");
}

/// The associated data for our [`ChessBoard`] widget.
///
/// This holds all state required to draw the board and pieces
#[derive(Clone, Data)]
struct ChessGame {
    /// All of the non-graphical game state
    state: ChessState,
    /// The [`SvgData`] for each type of piece
    piece_svgs: Rc<[SvgData; 12]>,
    /// From which player's perspective the board should be drawn
    player: Player,
    /// The game mode which is being played
    mode: Mode,
    /// The last square that was clicked, if any.
    last_clicked: Option<Square>,
}

/// The root widget - implements [`Widget`]<[`ChessGame`]>
///
/// All [`Widget`] methods are called on this struct:
/// - [event](ChessBoard::event): Handles a user event
/// - [lifecycle](ChessBoard::lifecycle): Handles a lifecycle notification (unused)
/// - [update](ChessBoard::update): Updates the board appearance in response to a change in game state
/// - [layout](ChessBoard::layout): Sets the size of the chessboard
/// - [paint](ChessBoard::paint): Draw the board
#[derive(Default)]
struct ChessBoard {
    /// The width/height of the board, in pixels. This is dynamically
    /// determined by the window size in [`ChessBoard::layout`].
    ///
    /// Ideally this would be a field on ChessGame, but [`ChessBoard::layout`]
    /// takes an immutable reference to ChessGame. Sad times.
    board_length: f64,
}

impl Widget<ChessGame> for ChessBoard {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, game: &mut ChessGame, _env: &Env) {
        // TODO: for Mode::Computer, call into the engine to get the response
        if let Event::MouseDown(mouse) = event {
            if let Some(square_clicked) = get_square(mouse.pos, self.board_length, &game.player) {
                if let Some(from) = game.last_clicked {
                    // Make the move
                    let chess_move = Move {
                        from,
                        to: square_clicked,
                        move_type: MoveType::Unknown,
                    };
                    let result = game.state.make_move(&chess_move);
                    println!("{}: {}", chess_move, result);
                    match result {
                        MoveResult::Continue => {
                            // Reorient board for two players
                            if game.mode == Mode::TwoPlayer {
                                game.player = game.player.swap();
                            } else {
                                // Find the best move for the computer
                                let best_response = game.state.get_best_move(3);
                                let result = game.state.make_move(&best_response);
                                println!("Computer: {}: {}", best_response, result);
                            }

                            // Redraw the board
                            ctx.request_paint();
                            game.last_clicked = None;
                        }
                        MoveResult::Stalemate | MoveResult::Win(_) => {
                            // Redraw the board
                            ctx.request_paint();
                        }
                        MoveResult::Illegal => {
                            game.last_clicked = Some(square_clicked);
                        }
                    }
                } else {
                    game.last_clicked = Some(square_clicked);
                }
            }
        }
    }

    fn layout(
        &mut self,
        _ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        _game: &ChessGame,
        _env: &Env,
    ) -> Size {
        let max_x = bc.max().width;
        let max_y = bc.max().height;
        if max_x > max_y {
            self.board_length = max_y * BOARD_SCALE;
        } else {
            self.board_length = max_x * BOARD_SCALE;
        }
        bc.max()
    }

    fn lifecycle(
        &mut self,
        _ctx: &mut LifeCycleCtx,
        _event: &LifeCycle,
        _data: &ChessGame,
        _env: &Env,
    ) {
    }

    fn update(
        &mut self,
        _ctx: &mut UpdateCtx,
        _old_data: &ChessGame,
        _data: &ChessGame,
        _env: &Env,
    ) {
    }

    fn paint(&mut self, ctx: &mut PaintCtx, game: &ChessGame, _env: &Env) {
        // Draw the chessboard
        draw_board(ctx, self.board_length);

        // Draw the pieces
        for mut i in 0..8 {
            for mut j in 0..8 {
                let chess_sq = Square {
                    rank: Rank::from_idx(i).unwrap(),
                    file: File::from_idx(j).unwrap(),
                };

                match game.player {
                    // Flip i so that we draw from 1 to 8
                    Player::White => i = 7 - i,
                    // Flip j so that we draw from A to H
                    Player::Black => j = 7 - j,
                }

                let piece = game.state.pieces[chess_sq.get_idx()];
                if let Some(piece) = piece {
                    draw_piece(
                        &game.piece_svgs[svg_idx(piece)],
                        ctx,
                        origin(i, j, self.board_length / 8.0),
                        self.board_length / 8.0,
                    );
                }
            }
        }
    }
}

/// The modes that the GUI supports
#[derive(Clone, Data, PartialEq, Debug)]
pub enum Mode {
    /// Two-player mode - the board swaps orientation after each move
    TwoPlayer,
    /// Computer mode - you play against the computer!
    Computer,
}

impl FromStr for Mode {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "2p" => Ok(Mode::TwoPlayer),
            "comp" => Ok(Mode::Computer),
            _ => Err(format!("Got an unexpected string for the mode: {}", s)),
        }
    }
}

//-----------------------------------------------------------------------------
// Helper functions

/// Build the initial view
fn ui_builder() -> impl Widget<ChessGame> {
    Align::centered(ChessBoard::default())
}

/// Get the index of the svg array corresponding to the passed-in piece
fn svg_idx(piece: Piece) -> usize {
    let idx_piece = match piece.kind {
        PieceKind::Pawn => 0,
        PieceKind::Knight => 1,
        PieceKind::Bishop => 2,
        PieceKind::Rook => 3,
        PieceKind::King => 4,
        PieceKind::Queen => 5,
    };
    match piece.player {
        Player::White => idx_piece,
        Player::Black => idx_piece + 6,
    }
}

/// Get the origin (top left corner) of the square in the i'th row, j'th column
fn origin(i: usize, j: usize, square_length: f64) -> Point {
    // Centre the board by shifting the origin
    let pad = ((1.0 - BOARD_SCALE) / 2.0) * (square_length * 8.0) / BOARD_SCALE;
    Point::new(
        j as f64 * square_length + pad,
        i as f64 * square_length + pad,
    )
}

/// Get a [`Rect`] corresponding to the square on the i'th row, j'th column
fn sq_rect(i: usize, j: usize, square_length: f64) -> Rect {
    let square_size = Size::new(square_length, square_length);
    Rect::from_origin_size(origin(i, j, square_length), square_size)
}

/// Draw the chessboard squares
fn draw_board(ctx: &mut PaintCtx, board_length: f64) {
    // Colour squares alternately black and white
    for i in 0..8 {
        for j in 0..8 {
            if (i + j) % 2 == 1 {
                ctx.fill(sq_rect(i, j, board_length / 8.0), &BLACK_SQUARE_COLOUR);
            } else {
                ctx.fill(sq_rect(i, j, board_length / 8.0), &WHITE_SQUARE_COLOUR);
            }
        }
    }
}

/// Draw a single piece, on the square identified by the passed-in origin
fn draw_piece(svg: &SvgData, ctx: &mut PaintCtx, origin: Point, size: f64) {
    // Translate the piece to the correct square, and scale to the correct size
    let transform = TranslateScale::new(origin.to_vec2(), size / 45.0).into();
    svg.to_piet(transform, ctx);
}

/// Figure out what square a click corresponds to
fn get_square(mut click_pos: Point, board_length: f64, player: &Player) -> Option<Square> {
    // If Black, the origin is H1, if White, it's A8
    let square_length = board_length / 8.0;
    let origin = origin(0, 0, square_length);
    let x_max = origin.x + board_length;
    let y_max = origin.y + board_length;

    // First, transform the Point to be White-orientated
    if player == &Player::Black {
        // If the coordinates from Black's perspective are (x, y), the
        // coordinates from White's perspective are
        // (x_max - x + origin.x, y_max - y + origin.y)
        click_pos = Point::new(
            x_max - click_pos.x + origin.x,
            y_max - click_pos.y + origin.y,
        );
    }

    // Check bounds
    if origin.x <= click_pos.x
        && x_max > click_pos.x
        && origin.y <= click_pos.y
        && y_max > click_pos.y
    {
        let file_idx = ((click_pos.x - origin.x) / square_length).floor() as usize;
        let rank_idx = ((click_pos.y - origin.y) / square_length).floor() as usize;
        let file = File::from_idx(file_idx).unwrap_or_else(|| {
            panic!(
                "Already checked bounds so {} should be a valid file",
                file_idx
            )
        });
        let rank = Rank::from_idx(7 - rank_idx).unwrap_or_else(|| {
            panic!(
                "Already checked bounds so {} should be a valid rank",
                rank_idx
            )
        });
        Some(Square { file, rank })
    } else {
        None
    }
}

// ----------------------------------------------------------------------------
// Tests
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_get_square() {
        let board_length = 100.0 * BOARD_SCALE;
        let square_length = board_length / 8.0;
        let mut origin = origin(0, 0, square_length);
        origin = Point::new(origin.x + 0.01, origin.y + 0.01);

        // Outside the board
        assert_eq!(
            get_square(Point::new(0.0, 0.0), board_length, &Player::White),
            None
        );
        assert_eq!(
            get_square(Point::new(100.0, 100.0), board_length, &Player::White),
            None
        );

        // The origin
        assert_eq!(
            get_square(origin, board_length, &Player::White),
            Some(Square {
                file: File::A,
                rank: Rank::R8
            })
        );
        assert_eq!(
            get_square(origin, board_length, &Player::Black),
            Some(Square {
                file: File::H,
                rank: Rank::R1
            })
        );

        // C3/F6 is 2.5 across, 5.5 down
        let point = Point::new(
            origin.x + 2.5 * square_length,
            origin.y + 5.5 * square_length,
        );
        assert_eq!(
            get_square(point, board_length, &Player::White),
            Some(Square {
                file: File::C,
                rank: Rank::R3
            })
        );
        assert_eq!(
            get_square(point, board_length, &Player::Black),
            Some(Square {
                file: File::F,
                rank: Rank::R6
            })
        );

        // Test within top left square
        for point in [
            Point::new(
                origin.x + square_length - 1.0,
                origin.y + square_length - 1.0,
            ),
            Point::new(origin.x + 1.0, origin.y + 1.0),
            Point::new(origin.x + square_length - 1.0, origin.y + 1.0),
            Point::new(origin.x + 1.0, origin.y + square_length - 1.0),
        ]
        .iter()
        {
            assert_eq!(
                get_square(*point, board_length, &Player::White),
                Some(Square {
                    file: File::A,
                    rank: Rank::R8
                })
            );
            assert_eq!(
                get_square(*point, board_length, &Player::Black),
                Some(Square {
                    file: File::H,
                    rank: Rank::R1
                })
            );
        }
    }
}
