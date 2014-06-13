{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Events where

import           Control.Lens
import           Data.Data
import           Data.Hashable
import           Data.Time
import           Data.Typeable
import           GHC.Generics  hiding (to)

import           DomEff

--------------------------------------------------------------------------------

class KeyModifiable p where
  altKey   :: p -> Bool
  ctrlKey  :: p -> Bool
  metaKey  :: p -> Bool
  shiftKey :: p -> Bool

class ScreenLocatable p where
  screenX :: p -> Int
  screenY :: p -> Int
  clientX :: p -> Int
  clientY :: p -> Int
  pageX   :: p -> Int
  pageY   :: p -> Int

--------------------------------------------------------------------------------

data ClipboardEventType
  = Copy
  | Cut
  | Paste
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Enum, Bounded )
instance Hashable ClipboardEventType

data ClipboardEventProperties
  = ClipboardEventProperties
    -- { clipboardData :: DomTransferKindOfThing }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic )
instance Hashable ClipboardEventProperties

data KeyboardEventType
  = KeyDown
  | KeyPress
  | KeyUp
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Enum, Bounded )
instance Hashable KeyboardEventType

data KeyboardEventProperties
  = KeyboardEventProperties
    { _ke_altKey   :: Bool
    , _ke_ctrlKey  :: Bool
    , charCode     :: Int
    , key          :: String
    , keyCode      :: Int
    , locale       :: String
    , location     :: Int
    , _ke_metaKey  :: Bool
    , repeat       :: Bool
    , _ke_shiftKey :: Bool
    , which        :: Int
    }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic )
instance Hashable KeyboardEventProperties

instance KeyModifiable KeyboardEventProperties where
  altKey   = _ke_altKey
  ctrlKey  = _ke_ctrlKey
  metaKey  = _ke_metaKey
  shiftKey = _ke_shiftKey

data FocusEventType
  = Focus
  | Blur
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Enum, Bounded )
instance Hashable FocusEventType

data FocusEventProperties
  = FocusEventProperties
--    { relatedTarget :: DomElementOfSomeKind }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic )
instance Hashable FocusEventProperties

data FormEventType
  = Change
  | Input
  | Submit
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Enum, Bounded )
instance Hashable FormEventType

data FormEventProperties = FormEventProperties
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic )
instance Hashable FormEventProperties

data MouseEventType
  = Click
  | DoubleClick
  | Drag
  | DragEnd
  | DragEnter
  | DragExit
  | DragLeave
  | DragOver
  | DragStart
  | Drop
  | MouseDown
  | MouseEnter
  | MouseLeave
  | MouseMove
  | MouseOut
  | MouseOver
  | MouseUp
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Enum, Bounded )
instance Hashable MouseEventType

data MouseEventProperties
  = MouseEventProperties
    { _me_altKey   :: Bool
    , button       :: Int
    , buttons      :: Int
    , _me_clientX  :: Int
    , _me_clientY  :: Int
    , _me_ctrlKey  :: Bool
    , _me_metaKey  :: Bool
    , _me_pageX    :: Int
    , _me_pageY    :: Int
--    , relatedTarget :: DOMEventTarget
    , _me_screenX  :: Int
    , _me_screenY  :: Int
    , _me_shiftKey :: Bool
    }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic )
instance Hashable MouseEventProperties

instance KeyModifiable MouseEventProperties where
  altKey   = _me_altKey
  ctrlKey  = _me_ctrlKey
  metaKey  = _me_metaKey
  shiftKey = _me_shiftKey

instance ScreenLocatable MouseEventProperties where
  screenX = _me_screenX
  screenY = _me_screenY
  pageX   = _me_pageX
  pageY   = _me_pageY
  clientX = _me_clientX
  clientY = _me_clientY

data TouchEventType
  = TouchCancel
  | TouchEnd
  | TouchMove
  | TouchStart
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Enum, Bounded )
instance Hashable TouchEventType

data Touch
  = Touch
    { touchIdentifier :: Int
    -- , touchTarget     :: DomTargetThing
    , _t_screenX      :: Int
    , _t_screenY      :: Int
    , _t_clientX      :: Int
    , _t_clientY      :: Int
    , _t_pageX        :: Int
    , _t_pageY        :: Int
    }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic )
instance Hashable Touch

instance ScreenLocatable Touch where
  screenX = _t_screenX
  screenY = _t_screenY
  pageX   = _t_pageX
  pageY   = _t_pageY
  clientX = _t_clientX
  clientY = _t_clientY

data TouchEventProperties
  = TouchEventProperties
    { _te_altKey     :: Bool
    , _te_ctrlKey    :: Bool
    , _te_metaKey    :: Bool
    , _te_shiftKey   :: Bool
    , changedTouches :: [Touch]
    , targetTouches  :: [Touch]
    , touches        :: [Touch]
    }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic )
instance Hashable TouchEventProperties

instance KeyModifiable TouchEventProperties where
  altKey   = _te_altKey
  ctrlKey  = _te_ctrlKey
  metaKey  = _te_metaKey
  shiftKey = _te_shiftKey

data UIEventType
  = Scroll
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Enum, Bounded )
instance Hashable UIEventType

data UIEventProperties
  = UIEventProperties
    { detail :: Int
    -- , view :: DomAbstractView
    }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic )
instance Hashable UIEventProperties

data WheelEventType
  = Wheel
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Enum, Bounded )
instance Hashable WheelEventType

data DeltaMode
  = Pixel
  | Line
  | Page
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic )
instance Hashable DeltaMode

data WheelEventProperties
  = WheelEventProperties
    { deltaX    :: Int
    , deltaY    :: Int
    , deltaZ    :: Int
    , deltaMode :: DeltaMode
    }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic )
instance Hashable WheelEventProperties

deltaModeEnum :: Prism' Int DeltaMode
deltaModeEnum = prism' up dn where
  up :: DeltaMode -> Int
  up et = case et of
    Pixel -> 0
    Line  -> 1
    Page  -> 2
  {-# INLINE up #-}

  dn :: Int -> Maybe DeltaMode
  dn i = case i of
    0 -> Just Pixel
    1 -> Just Line
    2 -> Just Page
    _ -> Nothing
  {-# INLINE dn #-}
{-# INLINE deltaModeEnum #-}

data EventType
  = ClipboardEvent ClipboardEventType ClipboardEventProperties
  | KeyboardEvent  KeyboardEventType  KeyboardEventProperties
  | FocusEvent     FocusEventType     FocusEventProperties
  | FormEvent      FormEventType      FormEventProperties
  | MouseEvent     MouseEventType     MouseEventProperties
  | TouchEvent     TouchEventType     TouchEventProperties
  | UIEvent        UIEventType        UIEventProperties
  | WheelEvent     WheelEventType     WheelEventProperties
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic )
instance Hashable EventType
makePrisms ''EventType

eventTypeName :: EventType -> String
eventTypeName et = case et of
  ClipboardEvent Copy _    -> "copy"
  ClipboardEvent Cut _     -> "cut"
  ClipboardEvent Paste _   -> "paste"
  KeyboardEvent KeyDown _  -> "keydown"
  KeyboardEvent KeyPress _ -> "keypress"
  KeyboardEvent KeyUp _    -> "keyup"
  FocusEvent Focus _       -> "focus"
  FocusEvent Blur _        -> "blur"
  FormEvent Change _       -> "change"
  FormEvent Input _        -> "input"
  FormEvent Submit _       -> "submit"
  MouseEvent Click _       -> "click"
  MouseEvent DoubleClick _ -> "doubleclick"
  MouseEvent Drag _        -> "drag"
  MouseEvent DragEnd _     -> "dragend"
  MouseEvent DragEnter _   -> "dragenter"
  MouseEvent DragExit _    -> "dragexit"
  MouseEvent DragLeave _   -> "dragleave"
  MouseEvent DragOver _    -> "dragover"
  MouseEvent DragStart _   -> "dragstart"
  MouseEvent Drop _        -> "drop"
  MouseEvent MouseDown _   -> "mousedown"
  MouseEvent MouseEnter _  -> "mouseenter"
  MouseEvent MouseLeave _  -> "mouseleave"
  MouseEvent MouseMove _   -> "mousemove"
  MouseEvent MouseOut _    -> "mouseout"
  MouseEvent MouseOver _   -> "mouseover"
  MouseEvent MouseUp _     -> "mouseup"
  TouchEvent TouchCancel _ -> "touchcancel"
  TouchEvent TouchEnd _    -> "touchend"
  TouchEvent TouchMove _   -> "touchmove"
  TouchEvent TouchStart _  -> "touchstart"
  UIEvent Scroll _         -> "scroll"
  WheelEvent Wheel _       -> "wheel"

class Event e where
  bubbles          :: e -> Bool
  cancelable       :: e -> Bool
  defaultPrevented :: e -> Bool
  eventPhase       :: e -> Int
  isTrusted        :: e -> Bool
  timeStamp        :: e -> UTCTime
  eventType        :: e -> EventType

clipboardEvent :: Event e => e -> Maybe (ClipboardEventType, ClipboardEventProperties)
clipboardEvent = preview (to eventType . _ClipboardEvent)

keyboardEvent :: Event e => e -> Maybe (KeyboardEventType, KeyboardEventProperties)
keyboardEvent = preview (to eventType . _KeyboardEvent)

focusEvent :: Event e => e -> Maybe (FocusEventType, FocusEventProperties)
focusEvent = preview (to eventType . _FocusEvent)

formEvent :: Event e => e -> Maybe (FormEventType, FormEventProperties)
formEvent = preview (to eventType . _FormEvent)

mouseEvent :: Event e => e -> Maybe (MouseEventType, MouseEventProperties)
mouseEvent = preview (to eventType . _MouseEvent)

touchEvent :: Event e => e -> Maybe (TouchEventType, TouchEventProperties)
touchEvent = preview (to eventType . _TouchEvent)

uiEvent :: Event e => e -> Maybe (UIEventType, UIEventProperties)
uiEvent = preview (to eventType . _UIEvent)

wheelEvent :: Event e => e -> Maybe (WheelEventType, WheelEventProperties)
wheelEvent = preview (to eventType . _WheelEvent)

class DomEff e m => DomEvent ev e m | m -> ev where
  preventDefault   :: ev -> m ()
  stopPropagation  :: ev -> m ()
  currentTarget    :: ev -> m e
  target           :: ev -> m e
