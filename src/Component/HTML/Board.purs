module Component.HTML.Board where


import Prelude
import Logic.Domain as D
import Data.Route as DR
import Affjax as AX
import Component.HTML.Header
import Conduit.Component.HTML.Footer
import Data.Newtype
import Slug (Slug)
import Halogen.Store.Connect (Connected, connect)
import Utils
import Data.Newtype as DN
import Halogen.HTML.CSS
import Halogen.Store.Select (selectEq)
import Affjax.ResponseFormat as AXRF
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Effect.Class.Console (log)
import Web.Event.Event as Event
import BoardFactory
import MapUtils as MU
import Data.Map.Internal as M
import Data.List as L
import Data.Maybe
import Logic.Domain
import CSS as C
import CSS.Common
import Halogen.HTML.CSS as HC
import Data.Array
import Logic.Game
import Control.Plus
import Data.Int
import Prelude
import Debug
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen.HTML.Properties as HP
import Logic.Domain
import Logic.Domain as D
import Data.Newtype as DN
import Data.Array
import CSS as C
import CSS.Common
import Halogen.HTML.CSS as HC
import Data.List as L
import Data.Map.Internal as M
import MapUtils as MU
import Debug
import Data.Either (hush)
import Halogen.HTML.Events as HE
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Component.BoardComponent
import Halogen.Store.Select (selectEq)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))
import Data.Route
import Data.Profile
import Data.Maybe
import Component.HTML.Utils (css, maybeElem, safeHref, whenElem)

board =
  let
    game = unwrap (makeGame)
    currentBoard = game.currentBoard
    groupedByRank = MU.groupByKeys (\(D.Position p) -> p.rank) currentBoard.tiles
    sortedRanks = L.reverse $ L.sort $ M.keys groupedByRank
    getUnderRank = \r -> maybe [] identity $ M.lookup r groupedByRank
  in
    HH.form
      [ HC.style $ (C.display C.block) *> (C.marginLeft auto) *> (C.marginRight auto), css "gtc-board" ]
      [ HH.div
          [ HC.style $ (C.display C.block) ]
          [ ( HH.table
                [ HC.style $ (C.borderSpacing (C.nil)) *> (C.marginLeft auto) *> (C.marginRight auto) ]
                $ map ((\a -> renderRank a Nothing game.turn) <<< getUnderRank)
                $ fromFoldable sortedRanks
            )
          ]
      ]

renderRank :: forall m action. Array D.Tile -> Maybe Position -> Turn -> H.ComponentHTML action () m
renderRank tiles selected turn = HH.tr [ HC.style (C.borderSpacing (C.nil)) ] $ map (\t -> renderTile t selected turn) (sort tiles)

renderTile (D.Tile tile) selected turn =
  let
    color = D.tileColor $ DN.wrap tile
    isHisMove = case turn of
      WhiteTurn -> maybe false (\(Piece p) -> p.color == WhitePiece) tile.currentPiece
      BlackTurn -> maybe false (\(Piece p) -> p.color == BlackPiece) tile.currentPiece
    boardTileColor =
      if (maybe false (eq tile.position) selected) && isHisMove then trace (show selected) \_ -> Selected
      else case color of
        WhiteTile -> White
        BlackTile -> Black
  in
    HH.td [ (square boardTileColor) ] [ maybeImage (wrap tile) ]

data BoardTileColor = White | Black | Selected

maybeImage (Tile tile) =
  let
    piece = tile.currentPiece
  in
    maybe (HH.text "") identity $ map (\p -> HH.img [ (pieceImage p), pieceStyle ]) piece

pieceImage (Piece p) = HP.src ("pieces/" <> (imageName { pieceType: p.pieceType, color: p.color }))
pieceStyle = HC.style $ C.width (C.pct (toNumber 85)) *> C.display C.block

imageName :: { pieceType :: PieceType, color :: PieceColor } -> String
imageName = case _ of
  { pieceType: Pawn, color: WhitePiece } -> "whitePawn.png"
  { pieceType: Bishop, color: WhitePiece } -> "whiteBishop.png"
  { pieceType: Knight, color: WhitePiece } -> "whiteKnight.png"
  { pieceType: Rook, color: WhitePiece } -> "whiteRook.png"
  { pieceType: Queen, color: WhitePiece } -> "whiteQueen.png"
  { pieceType: King, color: WhitePiece } -> "whiteKing.png"
  { pieceType: Pawn, color: BlackPiece } -> "blackPawn.png"
  { pieceType: Bishop, color: BlackPiece } -> "blackBishop.png"
  { pieceType: Knight, color: BlackPiece } -> "blackKnight.png"
  { pieceType: Rook, color: BlackPiece } -> "blackRook.png"
  { pieceType: Queen, color: BlackPiece } -> "blackQueen.png"
  { pieceType: King, color: BlackPiece } -> "blackKing.png"

square = case _ of
  White -> HC.style $ C.height (C.px $ toNumber 30) *> C.width (C.px $ toNumber 30) *> C.backgroundColor (C.rgb 245 245 245) *> C.display C.inlineBlock
  Black -> HC.style $ C.height (C.px $ toNumber 30) *> C.width (C.px $ toNumber 30) *> C.backgroundColor (C.rgb 32 32 32) *> C.display C.inlineBlock
  Selected -> HC.style $ C.height (C.px $ toNumber 30) *> C.width (C.px $ toNumber 30) *> C.backgroundColor (C.rgb 0 127 255) *> C.display C.inlineBlock
