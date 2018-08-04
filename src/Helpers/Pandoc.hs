module Helpers.Pandoc where

import ClassyPrelude.Yesod

import Text.Pandoc
import qualified Text.Pandoc.Highlighting as HL
import qualified Text.Pandoc.Readers.Markdown as MDR
import qualified Text.Pandoc.Writers.HTML as HTMLW

renderHtmlFromMarkdown :: Text -> IO (Either PandocError Html)
renderHtmlFromMarkdown s = do
  pandocE <- runIO $ MDR.readMarkdown defaultBoutiqueReaderOptions s
  case pandocE of
    Left err -> return $ Left $ err
    Right p -> do
      runIO $ HTMLW.writeHtml5 defaultBoutiqueWriterOptions p

defaultBoutiqueReaderOptions :: ReaderOptions
defaultBoutiqueReaderOptions = def
  { readerExtensions = extensionsFromList [Ext_backtick_code_blocks]
  }

defaultBoutiqueWriterOptions :: WriterOptions
defaultBoutiqueWriterOptions = def
    { writerHighlightStyle  = Just HL.tango
    }
