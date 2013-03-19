module Text.HTML.Truncate(truncateHtml,truncateHtml',truncateStringLike) where

import qualified Text.HTML.TagSoup as TS
import qualified Text.StringLike as SL
import Data.Char(isSpace)
import Data.List(dropWhileEnd)

{- 
Roughly, the algorithm works like this:
1. Parse the HTML to a list of tags
2. Walk through those tags. If a tag with actual text content is encountered,
   truncate it. 
   If the text was not long enough to result in actually anything being truncated, keep going and truncate less the in the next tag that is encountered.
   Otherwise, just close all open tags and remove all remaining text in them.
3. Remove trailing empty tags
-}

-- | Truncate HTML, and ensure that tags are closed; Remove trailing empty tags
truncateHtml :: SL.StringLike str => Int -> str -> str
truncateHtml n txt = snd $ truncateHtml' n txt

truncateHtml' :: SL.StringLike str => Int -> str -> (Int,str)
truncateHtml' n txt = fmap (TS.renderTags . removeTrailingEmptyTags) $ go n 0 (TS.parseTags txt)
  where
        removeTrailingEmptyTags = removeTrailingEmptyTags' [] . reverse
        removeTrailingEmptyTags' accm (t@(TS.TagClose _) : ts) = removeTrailingEmptyTags' (t : accm) ts
        removeTrailingEmptyTags' accm (t@(TS.TagOpen _ _) : ts)  = removeTrailingEmptyTags' (delCloseTag accm) ts
        removeTrailingEmptyTags' accm (t@(TS.TagText _) : ts) = reverse (t : ts) ++ accm
        removeTrailingEmptyTags' accm (t : ts) = removeTrailingEmptyTags' (t : accm) ts
        removeTrailingEmptyTags' accm [] = accm
        
        delCloseTag (t@(TS.TagClose _) : ts) = ts
        delCloseTag (t : ts) = t : delCloseTag ts
        delCloseTag [] = []
        
        go :: (SL.StringLike str) 
             => Int -- ^The number of remaining characters to truncate
             -> Int -- ^The number of open tags
             -> [TS.Tag str] -- ^The remaining tags to walk through
             -> (Int,[TS.Tag str])
        go c openTags _ | c <= 0 && openTags <= 0 = (0,[]) -- we're done. Nothing to truncate, nothing to close
        go i _        [] = (i,[])
        
        go 0 openTags (t@(TS.TagOpen _ _) : ts) = go 0 (openTags + 1) ts
        go c openTags (t@(TS.TagOpen _ _) : ts) = fmap (t :) (go c (openTags + 1) ts)
        
        go 0 openTags (t@(TS.TagClose _) : ts) = go 0 (openTags - 1) ts
        go c openTags (t@(TS.TagClose _) : ts) = fmap (t :) (go c (openTags - 1) ts)
        
        go 0 openTags ((TS.TagText str) : ts) = go 0 openTags ts
        go c openTags (t@(TS.TagText str) : ts) = case truncateStringLike c str of
           (c', str') -> fmap ((TS.TagText str') :) (go (max 0 c') openTags ts)
          
        go c openTags (t : ts) = fmap (t :) (go c openTags ts)
        
-- | Truncate to full words. If actual truncation occured, remove the last (usually cut-off) word, then remove trailing whitespace.
-- | Returns the truncated string and the number of characters that remain to be truncated
truncateStringLike :: SL.StringLike str => Int -> str -> (Int, str)
truncateStringLike c t = case truncateStringLike' c t of
  (0, t') -> (0, dropWhileEndSL isSpace $ dropWhileEndSL (not . isSpace) $ t')
  other   -> other
  
truncateStringLike' :: SL.StringLike str => Int -> str -> (Int, str)
truncateStringLike' 0 t = (0, SL.empty)
truncateStringLike' c t = case SL.uncons t of
  Nothing -> (c, t)
  Just (char, rest) -> fmap (SL.cons char) (truncateStringLike (c - 1) rest)
  
-- note: could be optimized for double applications of this.
dropWhileEndSL :: SL.StringLike a => (Char -> Bool) -> a -> a
dropWhileEndSL p = SL.fromString . (dropWhileEnd p) . SL.toString