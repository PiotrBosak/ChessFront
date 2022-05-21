module Component.HTML.Board where

import BoardFactory
import Component.HTML.Header
import Conduit.Component.HTML.Footer
import Control.Plus
import Data.Array
import Data.Int
import Data.Maybe
import Data.Newtype
import Data.Profile
import Data.Route
import Debug
import Halogen.HTML.CSS
import Logic.Domain
import Logic.Game
import Prelude
import Utils

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import CSS as C
import CSS.Common (auto)
import Component.HTML.Utils (css, maybeElem, safeHref, whenElem)
import Data.Either (hush)
import Data.Foldable (elem)
import Data.List as L
import Data.Map.Internal as M
import Data.Newtype as DN
import Data.Route as DR
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Halogen.VDom.Driver (runUI)
import Logic.Domain as D
import MapUtils as MU
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Slug (Slug)
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.Event as Event

board :: forall action m. H.ComponentHTML action () m
board =
  let
    game = unwrap (makeGame)
    currentBoard = game.currentBoard
    groupedByRank = MU.groupByKeys (\(D.Position p) -> p.rank) currentBoard.tiles
    sortedRanks = L.reverse $ L.sort $ M.keys groupedByRank
    getUnderRank = \r -> maybe [] identity $ M.lookup r groupedByRank
  in
    HH.div [ HC.style $ C.minWidth (C.px $ toNumber 400) <> C.minHeight (C.px $ toNumber 400) ]
      [ HH.form
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
      ]

renderRank :: forall m action. Array D.Tile -> Maybe Position -> Turn -> H.ComponentHTML action () m
renderRank tiles selected turn = HH.tr [ HC.style (C.borderSpacing (C.nil)) ] $ map (\t -> renderTile t selected turn) (sort tiles)

renderTile :: forall action m. Tile -> Maybe Position -> Turn -> H.ComponentHTML action () m
renderTile (D.Tile tile) selected turn =
  let
    color = D.tileColor $ DN.wrap tile
    isHisMove = case turn of
      WhiteTurn -> maybe false (\(Piece p) -> p.color == WhitePiece) tile.currentPiece
      BlackTurn -> maybe false (\(Piece p) -> p.color == BlackPiece) tile.currentPiece
    boardTileColor =
      if (maybe false (eq tile.position) selected) && isHisMove then Selected
      else case color of
        WhiteTile -> White
        BlackTile -> Black
  in
    HH.td [ (square boardTileColor) ] [ maybeImage (wrap tile) ]

data BoardTileColor = White | Black | Selected

maybeImage :: forall action m. Tile -> H.ComponentHTML action () m
maybeImage (Tile tile) =
  let
    piece = tile.currentPiece
  in
    maybe (HH.text "") identity $ map (\p -> HH.img [ (pieceImage p), pieceStyle ]) piece

pieceImage :: forall t123 t124. Piece -> HP.IProp (src :: String | t123) t124
pieceImage (Piece p) = HP.src ("pieces/" <> (imageName { pieceType: p.pieceType, color: p.color }))

pieceStyle :: forall t64 t65. HP.IProp (style :: String | t65) t64
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

square :: forall t6 t7. BoardTileColor -> HP.IProp (style :: String | t7) t6
square = case _ of
  White -> HC.style $ C.height (C.px $ toNumber 30) *> C.width (C.px $ toNumber 30) *> C.backgroundColor (C.rgb 245 245 245) *> C.display C.inlineBlock
  Black -> HC.style $ C.height (C.px $ toNumber 30) *> C.width (C.px $ toNumber 30) *> C.backgroundColor (C.rgb 32 32 32) *> C.display C.inlineBlock
  Selected -> HC.style $ C.height (C.px $ toNumber 30) *> C.width (C.px $ toNumber 30) *> C.backgroundColor (C.rgb 0 127 255) *> C.display C.inlineBlock
