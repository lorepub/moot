module Helpers.Forms where

import Import.NoFoundation

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

rows :: Text -> FieldSettings master -> FieldSettings master
rows r f = f { fsAttrs = ("rows", r) : fsAttrs f }

emailFromText :: Text -> Either Text Email
emailFromText t =
  case TEV.validate (encodeUtf8 t) of
    (Left err) -> Left $ pack err
    (Right _) ->
      Right $ Email t

emailFromField :: Text -> Either FormMessage Email
emailFromField e =
    case emailFromText e of
        (Left _) -> Left (MsgInvalidEntry "Not a valid email")
        (Right email') -> Right email'

emailField' :: (Monad m, RenderMessage (HandlerSite m) FormMessage )
            => Field m Email
emailField' =
    checkMMap
        (return . emailFromText)
        unEmail
        emailField

renderCard :: Monad m => Text -> FormRender m a
renderCard buttonMsg aform fragment = do
    (res, views') <- aFormToForm aform
    let views'' = views' []
        hasError argView = isJust (fvErrors argView)
    let widget = [whamlet|
<div class="panel panel-store polo-blue">
    <div class="panel-body">
        <div class="row">
            <div class="col-xs-12">
              \#{fragment}
              $forall view'' <- views''
                <div class="form-group">
                  $if (hasError view'')
                    <span.input-error>
                      ^{fvLabel view''}
                      ^{fvInput view''}
                  $else
                    ^{fvLabel view''}
                    ^{fvInput view''}
                  $maybe err <- fvErrors view''
                    <small.form-error.is-visible>#{err}
    <div class="panel-footer">
            <div class="row text-center">
                <div class="col-xs-12">
                    <button class="btn btn-lg btn-primary btn-block" type="submit">#{buttonMsg}</button>
                |]
    return (res, widget)

renderAbstractTypeDropdown :: Entity AbstractType -> (Text, AbstractTypeId)
renderAbstractTypeDropdown (Entity abstractTypeK abstractType) =
  let -- durationLabel =
      --   abstractTypeDuration abstractType
      prettyAbstractLabel =
        renderAbstractType abstractType
        -- [st|#{abstractTypeName abstractType} (#{durationLabel})|]
  in (prettyAbstractLabel, abstractTypeK)
