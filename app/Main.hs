{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Connect4

import Data.Char
import Data.List
import Data.Word
import Data.Maybe
import Control.Monad
import Text.Read (readMaybe)
import Data.Text (Text, pack, unpack)

import Brick
import Brick.Forms
import Brick.Focus
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Brick.Widgets.Center

import Lens.Micro
import Graphics.Vty
import Lens.Micro.TH


{------------------------
  Menu form
 ------------------------}

data EditChar = EditChar Char

instance Show EditChar where
  show (EditChar c) = [show c !! 1]

instance Read EditChar where
  readsPrec _ (c:cs)
    | isAscii c && isLetter c = [(EditChar $ toUpper c, cs)]
    | otherwise               = []
  readsPrec _ _               = []

innerChar :: EditChar -> Char
innerChar (EditChar c) = c

data Options =
  Options { _redInit    :: EditChar
          , _yellowInit :: EditChar
          , _starts     :: Piece
          , _state      :: Text
          }
makeLenses ''Options

data Name = RedInitField
          | YellowInitField
          | RedStartsField
          | YellowStartsField
          | StateField
          deriving (Eq, Ord, Show)

colourName :: Piece -> String
colourName Red    = "Red"
colourName Yellow = "Blue"

mkForm :: Options -> Form Options e Name
mkForm =
  newForm [ label (colourName Red    <> " initial") @@= editShowableField redInit    RedInitField
          , label (colourName Yellow <> " initial") @@= editShowableField yellowInit YellowInitField
          , label "Starting player" @@=
            radioField starts [ (Red,    RedStartsField,    pack $ colourName Red)
                              , (Yellow, YellowStartsField, pack $ colourName Yellow)
                              ]
          , label "Or enter state: " @@= editTextField state StateField (Just 1)
          ]
  where
    label s w = padBottom (Pad 1) $ (vLimit 1 . hLimit 17 $ str s <+> fill ' ') <+> w

formAttrMap :: AttrMap
formAttrMap = attrMap defAttr
  [ (editAttr,             white `on` black)
  , (editFocusedAttr,      black `on` blue)
  , (invalidFormInputAttr, white `on` red)
  , (focusedFormInputAttr, black `on` blue)
  ]

formDraw :: Form Options e Name -> [Widget Name]
formDraw f = [center form]
  where
    form = border . padLeftRight 2 . padTopBottom 1 $ (hLimit 45 $ renderForm f) <=> str help
    help = "(shift)tab to change field, space to select, enter to proceed"

handleOptionsEvent :: BrickEvent Name e -> EventM Name (Form Options e Name) ()
 -- signal there was a `C-c` by setting a field invalid: `C-c` is the only way the user could proceed with an invalid field
handleOptionsEvent (VtyEvent (EvKey (KChar 'c') ((MCtrl `elem`) -> True))) =
  modify (setFieldValid False RedInitField) *> halt
handleOptionsEvent (VtyEvent (EvKey KEnter [])) = get >>= \st -> if allFieldsValid st then halt else pure ()
handleOptionsEvent ev = do
  handleFormEvent ev
  st <- gets formState
  let validState = st^.state == "" || isJust (readMaybe $ unpack (st^.state) :: Maybe Word64)
  modify $ setFieldValid validState StateField

optionsTUI :: App (Form Options e Name) e Name
optionsTUI = App
  { appDraw         = formDraw
  , appHandleEvent  = handleOptionsEvent
  , appChooseCursor = focusRingCursor formFocus
  , appStartEvent   = pure ()
  , appAttrMap      = const formAttrMap
  }

defaultOptions :: Options
defaultOptions = Options
  { _redInit    = EditChar 'A'
  , _yellowInit = EditChar 'B'
  , _starts     = Red
  , _state      = ""
  }

getOptions :: IO (Form Options e Name)
getOptions = defaultMain optionsTUI $ mkForm defaultOptions


{------------------------
  Connect 4 UI
 ------------------------}

colour :: String -> Widget () -> Widget ()
colour = withAttr . attrName

piece :: String -> Widget ()
piece attr = border . colour attr $ str  "     \n     "

numberKeyStr :: Int -> Widget ()
numberKeyStr i = str $ "  [" <> show i <> "]"

columnToWidget :: Column -> [Widget ()]
columnToWidget c = (replicate numEmpties $ piece "empty") <> reverse (piece . ("bg" <>) . show <$> c)
  where numEmpties = 6 - length c

boardWidget :: Word64 -> Widget ()
boardWidget w = hBox $ zipWith (\c i -> vBox $ columnToWidget c <> [numberKeyStr i]) (board w) [1..7]

handleConnect4Event :: BrickEvent n e -> EventM n Word64 ()
handleConnect4Event (VtyEvent (EvKey (KChar 'c') ((MCtrl `elem`) -> True))) = halt
handleConnect4Event (VtyEvent (EvKey (KChar 'q') _)) = gets winner >>= \w -> if isJust w then halt else pure ()
handleConnect4Event (VtyEvent (EvKey (KChar  c ) _)) = modify . play $ ord c - 49
handleConnect4Event _ = pure ()

littlePad :: Widget() -> Widget ()
littlePad = padTop (Pad 1) . padLeftRight 10

nameAndScore :: Char -> Int -> Widget ()
nameAndScore c i = littlePad . str $ [c] <> " " <> show i

-- only colour in one of the initials, to show whose turn it is
-- also colour in the winning player's initial at the end of the game
topContent :: Word64 -> Widget ()
topContent w = redTopContent <+> yellowTopContent
  where
    redTopContent    = redColour    . nameAndScore (redChar w)    $ redPoints    w
    yellowTopContent = yellowColour . nameAndScore (yellowChar w) $ yellowPoints w
    redColour        = colour $ if redColoured     then "fg" <> show Red    else "empty"
    yellowColour     = colour $ if not redColoured then "fg" <> show Yellow else "empty"
    redColoured      = winner w == Just Red || turn w == Red

stateWidget :: Word64 -> Widget ()
stateWidget = littlePad . str . show

renderConnect4 :: Word64 -> Widget ()
renderConnect4 w = case winner w of
  Nothing -> content
  Just p  -> content <=> winnerWidget
    where
      winnerWidget  = hCenter . littlePad . colour ("fg" <> show p) . str $ winnerInitial:" wins!"
      winnerInitial = (if p == Red then redChar else yellowChar) w
  where
    content = vBox $ hCenter <$> [topContent w, boardWidget w, stateWidget w]

connect4AttrMap :: AttrMap
connect4AttrMap = attrMap defAttr
  [ (attrName $ "bg" <> show Red,    bg red)
  , (attrName $ "fg" <> show Red,    fg red)
  , (attrName $ "bg" <> show Yellow, bg blue)
  , (attrName $ "fg" <> show Yellow, fg blue)
  , (attrName "empty", defAttr)
  ]

connect4TUI :: App Word64 () ()
connect4TUI = App
  { appDraw         = singleton . vCenter . renderConnect4
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleConnect4Event
  , appStartEvent   = return ()
  , appAttrMap      = const connect4AttrMap
  }

runConnect4 :: Options -> IO ()
runConnect4 fields = defaultMain connect4TUI game *> pure ()
  where
    game        = case _state fields of
      "" -> newGame redStarts' redChar' yellowChar'
      s  -> read $ unpack s
    redStarts'  = _starts fields == Red
    redChar'    = innerChar $ _redInit fields
    yellowChar' = innerChar $ _yellowInit fields

main :: IO ()
main = do
  form <- getOptions
  -- if a field is invalid, it means the user `C-c`ed, so halt
  when (allFieldsValid form) . runConnect4 $ formState form
