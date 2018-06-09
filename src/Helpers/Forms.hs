module Helpers.Forms where

import Import.NoFoundation

import qualified Data.Text as T
import qualified Text.Email.Validate as TEV

named :: Text -> FieldSettings master -> FieldSettings master
named t f =
    f
    { fsName = Just t
    , fsId = Just t
    }

labelled
    :: forall master.
       SomeMessage master -> FieldSettings master -> FieldSettings master
labelled t f = f { fsLabel = t }

placeholder :: Text -> FieldSettings master -> FieldSettings master
placeholder t f = f { fsAttrs = ("placeholder", t) : fsAttrs f }

placeheld :: Text -> FieldSettings master
placeheld label = placeholder label ""

emailFromField :: Text -> Either FormMessage EmailAddress
emailFromField e =
    case TEV.validate (encodeUtf8 e) of
        (Left _) -> Left (MsgInvalidEntry "Not a valid email")
        (Right email') -> Right email'

emailField' :: (Monad m, RenderMessage (HandlerSite m) FormMessage )
            => Field m EmailAddress
emailField' =
    checkMMap
        (return . mungeError . TEV.validate . encodeUtf8)
        (decodeUtf8 . TEV.toByteString)
        emailField
  where
    mungeError (Left a) = Left $ T.pack a
    mungeError (Right a) = Right a
