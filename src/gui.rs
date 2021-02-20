//! Run a simple chess GUI using [`druid`]. No real options yet, just call
//! [`main`].


use druid::{
    kurbo::TranslateScale,
    widget::{Align, SvgData},
    AppLauncher, BoxConstraints, Color, Data, Env, Event, EventCtx, LayoutCtx, LifeCycle,
    LifeCycleCtx, PaintCtx, Point, Rect, RenderContext, Size, UpdateCtx, Widget, WindowDesc,
};

use std::rc::Rc;
use std::str::FromStr;

use crate::engine::{ChessState, File, Piece, PieceKind, Player, Rank, Square};

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
pub fn main() {
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
        state: Rc::new(state),
        piece_svgs,
        player,
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
    state: Rc<ChessState>,
    /// The [`SvgData`] for each type of piece
    piece_svgs: Rc<[SvgData; 12]>,
    /// From which player's perspective the board should be drawn
    player: Player,
}

/// The root widget - implements [`Widget`]<[`ChessGame`]>
///
/// All [`Widget`] methods are called on this struct:
/// - [event](ChessBoard::event): Handles a user event
/// - [lifecycle](ChessBoard::lifecycle): Handles a lifecycle notification (unused)
/// - [update](ChessBoard::update): Updates the board appearance in response to a change in game state
/// - [layout](ChessBoard::layout): Sets the size of the chessboard
/// - [paint](ChessBoard::paint): Draw the board
#[derive(Debug, Default)]
struct ChessBoard {
    /// The width/height of the board, in pixels. This is dynamically
    /// determined by the window size in [`ChessBoard::layout`]
    board_length: f64,
}

impl Widget<ChessGame> for ChessBoard {
    fn event(&mut self, _ctx: &mut EventCtx, event: &Event, _game: &mut ChessGame, _env: &Env) {
        // TODO - handle clicks and figure out the move
        // Call into the engine to get the response
        // Maybe need to call `ctx.request_paint()` after that returns?
        if let Event::MouseDown(mouse) = event {
            dbg!(mouse);
        }
    }

    fn layout(
        &mut self,
        _ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        _data: &ChessGame,
        _env: &Env,
    ) -> Size {
        let max_x = bc.max().width;
        let max_y = bc.max().height;
        if max_x > max_y {
            self.board_length = max_y * BOARD_SCALE;
        } else {
            self.board_length = max_x * BOARD_SCALE;
        }
        dbg!(self);
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

    fn paint(&mut self, ctx: &mut PaintCtx, data: &ChessGame, _env: &Env) {
        // Draw the chessboard
        draw_board(ctx, self.board_length);

        // Draw the pieces
        for mut i in 0..8 {
            for mut j in 0..8 {
                let chess_sq = Square {
                    rank: Rank::from_idx(i).unwrap(),
                    file: File::from_idx(j).unwrap(),
                };

                match data.player {
                    // Flip i so that we draw from 1 to 8
                    Player::White => i = 7 - i,
                    // Flip j so that we draw from A to H
                    Player::Black => j = 7 - j,
                }

                let piece = data.state.pieces[chess_sq.get_idx()];
                if let Some(piece) = piece {
                    draw_piece(
                        &data.piece_svgs[svg_idx(piece)],
                        ctx,
                        origin(i, j, self.board_length / 8.0),
                        self.board_length / 8.0,
                    );
                }
            }
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
