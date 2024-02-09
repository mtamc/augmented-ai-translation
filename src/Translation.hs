{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Translation (loadAndRunTask) where

import Claude                     (Prompt (..), getCompletion)
import Control.Lens               (to, (.~), (^.))
import Data.Aeson                 (FromJSON, ToJSON)
import Data.List.Extra            (takeEnd)
import Data.Map                   qualified as Map
import Data.Maybe                 (fromJust)
import Data.Text                  qualified as T
import Data.Time.Clock            (diffUTCTime, getCurrentTime)
import GHC.IO                     (unsafePerformIO)
import Prelude                    hiding (many)
import Text.Megaparsec            (Parsec, anySingle, many, manyTill, parse)
import Text.Megaparsec.Char       (space, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Show qualified
import Wordpress                  (createPost, editPostContent, searchPosts)


-- EXPORTS

loadAndRunTask ∷ IO ()
loadAndRunTask = do
  task ← loadTask
  cfg ← loadCfg
  runTask cfg task


-- MAIN FUNCTIONS

{-# NOINLINE chunkLineCount #-}
chunkLineCount ∷ Int
chunkLineCount = (^. #linesPerChunk) $ unsafePerformIO loadCfg

promptDataFromTask ∷ Task → IO PromptData
promptDataFromTask (Task { chapter, context, glossary }) = do
  rawLines        ← neLines <$> loadRaw chapter
  previousChLines ← neLines <$> loadTranslation (chapter - 1)
  pure PromptData
    { context                 = unlines context
    , glossary                = glossary
    , translatedPreviousLines = takeEnd (60 - chunkLineCount) previousChLines
    , untranslatedLines       = take chunkLineCount rawLines
    , untranslatedLinesTodo   = drop chunkLineCount rawLines
    }

runTask ∷ Cfg → Task → IO ()
runTask cfg task = do
  let taskType = task ^. #taskType
  echo "--------"
  echo ("Running " ⊕ show taskType)
  start ← getCurrentTime
  case taskType of
    MC1 → echoLns =≪ getTaskCompletion cfg promptForMissingCtx task
    MC2 → echoLns =≪ getTaskCompletion cfg promptForMissingCtxForce task
    MC3 → echoLns =≪ getTaskCompletion cfg promptForMissingTransliterations task
    AskIfContextSufficient → do
      runTask cfg (task & #taskType .~ MC1)
      runTask cfg (task & #taskType .~ MC2)
      runTask cfg (task & #taskType .~ MC3)
    Translate → runTl cfg task
    H → saveHtml task
    P → publish task
  end ← getCurrentTime
  let timeTaken = diffUTCTime end start
  echo ("Ran " ⊕ show taskType ⊕ " in " ⊕ show timeTaken)
  where
  getTaskCompletion ∷ Cfg → (Cfg → PromptData → Prompt) → Task → IO Text
  getTaskCompletion c promptFn t = getCompletion . promptFn c =≪ promptDataFromTask t

saveHtml ∷ Task → IO ()
saveHtml task = do
  tlTxt ← loadTranslation (task ^. #chapter)
  let html = toHtml tlTxt
  writeFile "./playground/html.txt" (toString html)
  echo "written to ./playground/html.txt"

-- | NOTE: This function has a bunch of hardcoded stuff. It's not really meant
-- for anyone other than me to use, at this time.
publish ∷ Task → IO ()
publish task = do
  tlTxt ← loadTranslation (task ^. #chapter)
  prevTlTxt ← loadTranslation ((task ^. #chapter) - 1)
  let ch = task ^. #chapter
  prevChPost ← getChPost "Potions" (ch - 1) <&> fromJust
  prevPrevChPost ← getChPost "Potions" (ch - 2) <&> fromJust
  let html = toHtml tlTxt
      chTitle = task ^. #chapterTitle & fromMaybe "Untitled"
  newPost ← createPost chTitle ["Potions"] $
    newChHtml ch (prevChPost ^. #url) Nothing tlTxt
  editPostContent potionIndexId . addToIndex chTitle $ newPost ^. #url
  let previousChPostNewContent
        = newChHtml (ch-1) (prevPrevChPost ^. #url) (Just $ newPost ^. #url) prevTlTxt
  editPostContent (prevChPost ^. #postId) (const previousChPostNewContent)
  writeFile "./playground/html.txt" (toString html)
  echo "written to ./playground/html.txt"
  echo "uploaded new post, edited index and previous chapter post"
  where
  getChPost series ch = searchPosts series
    <&> find (\p → show ch `T.isInfixOf` (p ^. #title))
  newChHtml ∷ Int → Text → Maybe Text → Text → Text
  newChHtml chapter prevUrl nextUrl raw
    = "<!-- wp:paragraph -->\n"
    ⊕ potionLinks chapter prevUrl nextUrl
    ⊕ "<!-- /wp:paragraph -->\n\n<!-- wp:more -->\n<!--more-->\n<!-- /wp:more -->\n\n<!-- wp:html -->"
    ⊕ toHtml raw
    ⊕ "<!-- /wp:html -->\n\n<!-- wp:paragraph --><p></p><!-- /wp:paragraph -->\n\n<!-- wp:paragraph -->"
    ⊕ potionLinks chapter prevUrl nextUrl
    ⊕ "<!-- /wp:paragraph -->"
  potionLinks ∷ Int → Text → Maybe Text → Text
  potionLinks chapter prevUrl nextUrl
    = "<p><a href=\"https://ainoveltls.wordpress.com/2023/09/20/potions-index/\">Index</a> &#8211; <a href=\"https://ncode.syosetu.com/n6088cy/" ⊕ show chapter ⊕ "/\">Raw</a> &#8211; <a href=\"" ⊕ prevUrl ⊕ "\">Previous</a> &#8211; "
    ⊕ case nextUrl of
         Nothing  → "Next"
         Just url → "<a href=\"" ⊕ url ⊕ "\">Next</a>"
    ⊕ "</p>\n"
  potionIndexId ∷ Int
  potionIndexId = 43
  addToIndex ∷ Text → Text → Text → Text
  addToIndex title url originalIndexHtml
    = originalIndexHtml
    & T.replace "</ul>" ("<li><a href=\"" ⊕ url ⊕ "\">" ⊕ title ⊕ "</a></li></ul>")

toHtml ∷ Text → Text
toHtml raw = getStdOut "node" ["./js/toHtml.js", raw]

runTl ∷ Cfg → Task → IO ()
runTl cfg task = do
  let filename = "./playground/translations/" ⊕ (task ^. #chapter . to show) ⊕ ".txt"
  promptData ← promptDataFromTask task
  let buildTl Nothing   _ = pass
      buildTl (Just pd) num = do
        chunkTxt ← T.replace "<translation>" ""
                 . T.replace "</translation>" ""
                 <$> getCompletion (promptForTranslation cfg pd)
        let chunk = Chunk
              { raw = pd ^. #untranslatedLines . to dblUnlines
              , tl = chunkTxt
              , num = num
              }
            chunkLns = neLines chunkTxt
            nextTranslatedPreviousLines
              = drop (length chunkLns) (pd ^. #translatedPreviousLines) ⊕ chunkLns
            nextUntranslatedLines = take chunkLineCount (pd ^. #untranslatedLinesTodo)
            nextUntranslatedLinesTodo = drop chunkLineCount (pd ^. #untranslatedLinesTodo)
            nextPrompt
              | null nextUntranslatedLines = Nothing
              | otherwise = Just $ pd
                & #translatedPreviousLines .~ nextTranslatedPreviousLines
                & #untranslatedLines       .~ nextUntranslatedLines
                & #untranslatedLinesTodo   .~ nextUntranslatedLinesTodo
        echo $ "Adding chunk to " ⊕ filename
        appendFile (toString filename) . toString $ encodeChunk chunk
        buildTl nextPrompt (num + 1)
  appendFile (toString filename) "\n"
  void $ buildTl (Just promptData) 0
  echo ("Saved translation to " ⊕ filename)


-- TYPES

-- AskIfContextSufficient = MissingContextAll; T = Translation
data TaskType = MC1 | MC2 | MC3 | AskIfContextSufficient | Translate | H | P deriving
  ( Eq
  , FromJSON
  , Generic
  , ToJSON
  )
instance Show TaskType where
  show = \case
    MC1 → "Step 1/3 - Asking if the AI needs more information, giving it the option to say \"OK\" if it has enough.)"
    MC2 → "Step 2/3 - Asking if the AI needs more information, *forcing the AI to say at least one suggestion*. Feel free to ignore those suggestions if they seem overkill!"
    MC3 → "Step 3/3 - Asking for missing canonizations/transliterations in the glossary, *forcing the AI to say at least one suggestion*. Again, feel free to ignore those suggestions if they seem overkill!"
    AskIfContextSufficient → "AskIfContextSufficient (All three tasks querying for missing information)"
    Translate → "Translate (Translate chunk by chunk)"
    H → "H (Generate HTML)"
    P → "P (Publish)"



data Task
  = Task
    { taskType     ∷ TaskType
    , chapter      ∷ Int
    , chapterTitle ∷ Maybe Text
    , context      ∷ [Text]
    , glossary     ∷ Map Text Text
    }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

data Cfg
  = Cfg
    { linesPerChunk            ∷ Int
    , alreadyTranslatedHistory ∷ Text
    , fullJpHistory            ∷ Text
    , ctxAndTask               ∷ Text
    , missingCtx               ∷ Text
    , missingTransliterations  ∷ Text
    , translation              ∷ Text
    , tlPrefill                ∷ Text
    }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

data PromptData
  = PromptData
    { context                 ∷ Text
    , glossary                ∷ Map Text Text
    , translatedPreviousLines ∷ [Text]
    , untranslatedLines       ∷ [Text]
    , untranslatedLinesTodo   ∷ [Text]
    }
  deriving (Eq, Generic, Show)

data Chunk
  = Chunk
    { raw ∷ Text
    , tl  ∷ Text
    , num ∷ Int
    }
  deriving (Eq, Generic, Show)

encodeChunk ∷ Chunk → Text
encodeChunk (Chunk { raw, tl, num })
  = chunkRaw ⊕ show num ⊕ "\n\n"
  ⊕ normalizeParagraphs raw
  ⊕ "\n" ⊕ chunkTl ⊕ show num ⊕ "\n\n"
  ⊕ normalizeParagraphs tl
  ⊕ chunkEnd ⊕ "\n\n"

chunkRaw, chunkTl, chunkEnd ∷ Text
chunkRaw = "||CHUNKRAW||"
chunkTl  = "||CHUNKTL||"
chunkEnd = "||CHUNKEND||"

decodeChunks ∷ Text → [Chunk]
decodeChunks = rights . map (parse chunkP "") . T.splitOn chunkEnd

chunkP ∷ Parsec Void Text Chunk
chunkP = do
  space
  string chunkRaw
  num ← L.decimal
  raw ← manyTill anySingle (string chunkTl)
  -- string chunkTl
  L.decimal
  tl ← many anySingle
  pure $ Chunk (toText raw) (toText tl) num


-- PROMPT MAKERs

promptForMissingCtx ∷ Cfg → PromptData → Prompt
promptForMissingCtx cfg promptData =
  Prompt
    { instruction = missingCtxInstruction cfg promptData
    , prefill = ""
    }

promptForMissingCtxForce ∷ Cfg → PromptData → Prompt
promptForMissingCtxForce cfg promptData =
  Prompt
    { instruction = missingCtxInstruction cfg promptData
    , prefill = "Here are my questions in English:"
    }

missingCtxInstruction ∷ Cfg → PromptData → Text
missingCtxInstruction cfg pd = inquiryInstr cfg pd (cfg ^. #missingCtx)

promptForMissingTransliterations ∷ Cfg → PromptData → Prompt
promptForMissingTransliterations cfg pd =
  Prompt
    { instruction = inquiryInstr cfg pd (cfg ^. #missingTransliterations)
    , prefill = "Here is the list:\n\n-"
    }

promptForTranslation ∷ Cfg → PromptData → Prompt
promptForTranslation cfg pd =
  Prompt
    { instruction = tlInstr cfg pd (cfg ^. #translation)
    , prefill = cfg ^. #tlPrefill
    }

tlInstr ∷ Cfg → PromptData → Text → Text
tlInstr cfg (PromptData { context, glossary, translatedPreviousLines, untranslatedLines }) task
  = (cfg ^. #alreadyTranslatedHistory
  & T.replace "{{passage}}"      (dblUnlines translatedPreviousLines)
  & T.replace "{{continuation}}" (dblUnlines untranslatedLines))
  ⊕ "\n\n"
  ⊕ (cfg ^. #ctxAndTask
  & T.replace "{{context}}" (context ⊕ "\n" ⊕ formatGlossary untranslatedLines glossary)
  & T.replace "{{task}}"    task)

inquiryInstr ∷ Cfg → PromptData → Text → Text
inquiryInstr cfg (PromptData { context, glossary, untranslatedLines, untranslatedLinesTodo }) task
  = let fulljp = untranslatedLines ⊕ untranslatedLinesTodo
  in (cfg ^. #fullJpHistory & T.replace "{{fulljp}}" (dblUnlines fulljp))
  ⊕ "\n\n"
  ⊕ (cfg ^. #ctxAndTask
  & T.replace "{{continuation}}" (dblUnlines untranslatedLines)
  & T.replace "{{context}}"      (context ⊕ formatGlossary fulljp glossary)
  & T.replace "{{task}}"         task)

formatGlossary ∷ [Text] → Map Text Text → Text
formatGlossary (unlines → untranslated) = Map.foldrWithKey f "" where
  f k v acc
    | k `T.isInfixOf` untranslated = acc ⊕ "\n[" ⊕ k ⊕ " = " ⊕ v ⊕ "]"
    | otherwise                    = acc


-- LOADING FILES

loadTask ∷ IO Task
loadTask = decodeFile "./playground/task.json"

loadCfg ∷ IO Cfg
loadCfg = decodeFile "./playground/config.json"

loadRaw ∷ Int → IO Text
loadRaw chapter = decodeUtf8 <$> readFileBS ("./playground/raws/" ⊕ show chapter ⊕ ".txt")

loadTranslation ∷ Int → IO Text
loadTranslation chapter = do
  chunksTxt ← decodeUtf8 <$> readFileBS ("./playground/translations/" ⊕ show chapter ⊕ ".txt")
  let chunks = decodeChunks chunksTxt
      tl = T.intercalate "\n\n" $ map (^. #tl) chunks
  pure tl


-- UTILS

neLines ∷ Text → [Text]
neLines = filter (not . T.null) . lines

dblUnlines ∷ [Text] → Text
dblUnlines = mconcat . map (⊕ "\n\n")

normalizeParagraphs ∷ Text → Text
normalizeParagraphs = dblUnlines . neLines

echoLns ∷ Text → IO ()
echoLns txt = traverse_ echo (lines txt)
