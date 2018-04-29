-- Create type synonyms for the text editor
type Text = String -- Equivalent to [Char] (character array/list)
type CursorPosition = Int

-- Create a type for the text editor, an editor with text and 2 cursor positions
data Editor = Editor {
    text :: Text,
    cursorLeft :: CursorPosition,
    cursorRight :: CursorPosition
} deriving (Show)

-- NOTE: Sometimes uses `e` to refer to the editor as a whole (i.e. in `e@(Editor t cL cR)`),
-- t for text, and cL, cR for the left and right cursors

-- Create editor with default values
createEditor :: Editor
createEditor = Editor { text = [], cursorLeft = 0, cursorRight = 0 }

-- Return editor with values set to default
initEditor :: Editor -> Editor
initEditor (Editor _ _ _) = Editor { text = [], cursorLeft = 0, cursorRight = 0 }

getText :: Editor -> Text
getText (Editor t _ _) = t

printText :: Editor -> Text
printText (Editor t cL cR)
    -- Guards, if first condition met one cursor, otherwise two cursors
    -- If the cursors are in the same place only show one, otherwise show left and right
    | cL == cR = beforeCursorLeft ++ "|" ++ afterCursorRight
    | otherwise = beforeCursorLeft ++ "[" ++ betweenCursors ++ "]" ++ afterCursorRight
            -- Define the values used in the guards
            -- Take selection of characters from beginning up to the left cursor
    where   beforeCursorLeft = take cL t
            -- Drop characters before the left cursor, then take selection of characters up to the right cursor
            betweenCursors = (take (cR - cL) . drop cL) t
            -- Drop characters before the right cursor, leaving only the characters after it
            afterCursorRight = drop cR t

setText :: Editor -> Text -> Editor
setText (Editor t cL cR) x =
    -- Pre-condition, text not too long
    if length x <= 1024
        then (Editor x (length x) (length x))
    -- Error if pre-condition not met
    else error "Text too long"

insertCharacter :: Editor -> Char -> Editor
insertCharacter (Editor t cL cR) y =
    -- Pre-condition, cursor not at max text length
    if length t < 1024
        then (Editor ((take cL t) ++ y:(drop cR t)) (cL + 1) (cL + 1))
    -- Error if pre-condition not met
    else error "Text too long"

textLength :: Text -> Int
-- Base case
-- Pattern match for empty string or tail
textLength [] = 0
-- Pattern match head `t` and tail `ts`, recursive function
textLength (t:ts) = 1 + textLength ts

getTextLength :: Editor -> Int
-- Higher-order function, return a function to get the length
getTextLength (Editor t _ _) = textLength t

moveCursorLeft :: Editor -> Editor
moveCursorLeft e@(Editor t cL _) =
    -- Pre-conditions, make sure that the cursors will be within the text if moved
    if cL > 0
        then (Editor t (cL - 1) (cL - 1))
    -- Return original if pre-condition not met
    else e

moveCursorRight :: Editor -> Editor
moveCursorRight e@(Editor t _ cR) =
    -- Pre-conditions, make sure that the cursors will be within the text if moved
    if cR < length t
        then (Editor t (cR + 1) (cR + 1))
    -- Return original if pre-condition not met
    else e

leftUntilSpace :: Editor -> Editor
-- Pattern match no text
leftUntilSpace (Editor [] _ _) = (Editor [] 0 0)
-- Guards, if stop condition met return original, otherwise use recursion until condition met
leftUntilSpace e@(Editor t cL _)
    | cL == 0 || ((head . reverse) $ take cL t) == ' ' = e
    | otherwise = leftUntilSpace (moveCursorLeft e)

jumpCursorWordStart :: Editor -> Editor
-- Higher order function, return function that returns an Editor
jumpCursorWordStart e = leftUntilSpace e

rightUntilSpace :: Editor -> Editor
-- Pattern match no text
rightUntilSpace (Editor [] _ _) = (Editor [] 0 0)
-- Guards, if stop condition met return original, otherwise use recursion until condition met
rightUntilSpace e@(Editor t _ cR)
    | cR == length t || (head $ drop cR t) == ' ' = e
    | otherwise = rightUntilSpace (moveCursorRight e)

jumpCursorWordEnd :: Editor -> Editor
-- Higher order function, return function that returns an Editor
jumpCursorWordEnd e = rightUntilSpace e

selectCursorLeft :: Editor -> Editor
selectCursorLeft e@(Editor t cL cR) =
    -- Pre-condition, left cursor must be within text
    if cL > 0
        then (Editor t (cL - 1) cR)
    -- Return original if pre-condition not met
    else e

selectCursorRight :: Editor -> Editor
selectCursorRight e@(Editor t cL cR) =
    -- Pre-condition, right cursor must be within text
    if cR < length t
        then (Editor t cL (cR + 1))
    -- Return original if pre-condition not met
    else e

selectCursorToWordStart :: Editor -> Editor
selectCursorToWordStart (Editor t cL cR) =
    -- Recur until condition met
    if cL == 0 || ((head . reverse) $ take cL t) == ' '
        then (Editor t cL cR)
    -- Recursive function
    else selectCursorToWordStart (Editor t (cL - 1) cR)

selectCursorToWordEnd :: Editor -> Editor
selectCursorToWordEnd (Editor t cL cR) =
    -- Recur until condition met
    if cR == length t || (head $ drop cR t) == ' '
        then (Editor t cL cR)
    -- Recursive function
    else selectCursorToWordEnd (Editor t cL (cR + 1))

selectCursorToStart :: Editor -> Editor
selectCursorToStart (Editor t _ cR) =
    -- Return the original editor but with the left cursor set to 0
    (Editor t 0 cR)

selectCursorToEnd :: Editor -> Editor
selectCursorToEnd (Editor t cL _) =
    -- Return the original editor but with the right cursor set to the length of the text
    (Editor t cL (length t))

jumpCursorToStart :: Editor -> Editor
jumpCursorToStart (Editor t _ _) =
    -- Return the original editor but with the left/right cursors set to 0
    (Editor t 0 0)

jumpCursorToEnd :: Editor -> Editor
jumpCursorToEnd (Editor t _ _) =
    -- Return the original editor but with the left/right cursors set to the length of the text
    -- Use let to easily reuse the value
    let textLength = length t 
    in (Editor t textLength textLength)

copySelection :: Editor -> (Editor, Text)
copySelection e@(Editor t cL cR) =
    -- Get the characters between the cursors
    let textBetweenCursors = (take (cR - cL) . drop cL) t
    -- Return a tuple
    in (e, textBetweenCursors)

pasteText :: Editor -> Text -> Editor
pasteText (Editor t cL cR) y =
    -- Get text before and after cursor, add the pasted text between them
    let beforeCursorLeft = take cL t
        afterCursorRight = drop cR t
    in (Editor (beforeCursorLeft ++ y ++ afterCursorRight) (cL + length y) (cL + length y))

deleteSelection :: Editor -> Editor
deleteSelection (Editor t cL cR) =
    -- Get text before and after cursor, without text between them
    let beforeCursorLeft = take cL t
        afterCursorRight = drop cR t
    in (Editor (beforeCursorLeft ++ afterCursorRight) cL cL)

cutSelection :: Editor -> (Editor, Text)
cutSelection (Editor t cL cR) =
    -- Lambda (anonymous) function taking an editor (pattern matched) and returning an editor
    -- Higher order function, return function that deletes selection and returns Editor
    (\e -> (deleteSelection e, snd (copySelection e))) (Editor t cL cR)

deleteCursorLeft :: Editor -> Editor
deleteCursorLeft (Editor t cL _) =
    -- Get the characters up to 1 before the left cursor and after the right cursor
    (Editor (take (cL - 1) t ++ drop cL t) (cL - 1) (cL - 1))

deleteCursorRight :: Editor -> Editor
deleteCursorRight (Editor t _ cR) =
    -- Get the characters before the left cursor and 1 after the right cursor
    (Editor (take cR t ++ drop (cR + 1) t) cR cR)

-- Returns an IO Editor, impure function
-- Use of monad
setTextFromFile :: Editor -> FilePath -> IO Editor
setTextFromFile (Editor t cL cR) x = do
    text <- readFile x
    return (Editor text (length text) (length text))

-- Returns an IO String, impure function
-- Use of monad
saveTextToFile :: Editor -> FilePath -> IO Editor
saveTextToFile e@(Editor t _ _) x = do
    writeFile x t
    return e

main :: IO ()
main = do
    putStrLn "let e = createEditor"
    let e = createEditor
    putStr "=> "
    print e

    putStrLn "\nlet x = setText e \"hello world\""
    let x = setText e "hello world"
    putStr "=> "
    print x
    
