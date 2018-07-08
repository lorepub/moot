module Model.Render where

import ClassyPrelude.Yesod

import Text.Shakespeare.Text

import Model

renderAbstractType :: AbstractType -> Text
renderAbstractType (AbstractType _ name td) =
  [st|#{name} (#{renderTalkDuration td})|]
