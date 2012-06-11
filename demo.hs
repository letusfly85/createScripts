{-
 - ghc --make -optl-mwindows demo.hs -o demo
 -}

import Graphics.UI.Gtk
import qualified OraObjectCheck as O

main = do
    initGUI
    window <- windowNew
    set window [ windowTitle         := "demo",
                 windowDefaultWidth  := 250,
                 windowDefaultHeight := 480 ]

    vb1        <- vBoxNew False 0
    containerAdd window vb1

    txtInput <- textViewNew
    textViewSetBorderWindowSize txtInput  TextWindowTop 5
    textViewSetBorderWindowSize txtInput  TextWindowBottom 5
    boxPackStart vb1 txtInput  PackGrow    5

    sep <- hSeparatorNew
    boxPackStart vb1 sep       PackNatural 0

    vb2        <- vBoxNew False 0
    boxPackStart vb1 vb2 PackNatural 5

    scrwin  <- scrolledWindowNew Nothing Nothing

    txtOutput <- textViewNew
    textViewSetEditable         txtOutput False
    widgetSetSizeRequest txtInput  100 (50)
    widgetSetSizeRequest scrwin    100 (350)
    widgetSetSizeRequest txtOutput 100 (-1)

    boxPackStart vb2 scrwin PackGrow 0
    scrolledWindowAddWithViewport scrwin txtOutput

    hb          <- hBoxNew False 0
    boxPackStart vb2 hb PackNatural 5

    getButton   <- buttonNewWithLabel "GET"
    clearButton <- buttonNewWithLabel "CLEAR"

    boxPackStart hb getButton   PackNatural 5
    boxPackStart hb clearButton PackNatural 5

    widgetShowAll window

    onClicked getButton   (
        do  txtInput `copy2output` txtOutput
            downScrolledWindow scrwin 
            )
    onClicked clearButton (clearTextView txtOutput)

    onDestroy window mainQuit
    mainGUI

copy2output :: TextView -> TextView -> IO ()
copy2output txtInput txtOutput = do
    inputBuff  <- textViewGetBuffer txtInput
    startIter  <- textBufferGetStartIter inputBuff
    endIter    <- textBufferGetEndIter   inputBuff
    str        <- textBufferGetText inputBuff startIter endIter True
    showAllOraObjType (O.getAllOraObjType (lines str)) txtOutput

clearTextView :: TextView -> IO ()
clearTextView txtOutput = do
    outputBuff <- textViewGetBuffer txtOutput
    textBufferSetText outputBuff ""

showAllOraObjType :: [Either (String,String) (String,String)] -> TextView -> IO ()
showAllOraObjType [] view = return ()
showAllOraObjType (x:xs) view = do
    outputBuff <- textViewGetBuffer view 
    myTag      <- textTagNew Nothing
    set myTag [ textTagForegroundGdk := (Color 52428 0 26214),
                textTagForeground := "red",
                textTagBackground := "green"]
    case x of
          Right (tableName,tableType) -> do fnc (tableName,tableType) outputBuff view
          Left  (tableName,causion  ) -> do fnc (tableName,causion)   outputBuff view

                                            outputBuff   <- textViewGetBuffer view

                                            tagTable     <- textBufferGetTagTable outputBuff
                                            textTagTableAdd tagTable myTag
                                            startLineNum <- textBufferGetLineCount  outputBuff
                                            startIter    <- textBufferGetIterAtLine outputBuff (startLineNum - 2)
                                            endIter      <- textBufferGetIterAtLine outputBuff (startLineNum + 1)
                                            textBufferApplyTag outputBuff myTag startIter endIter

    showAllOraObjType xs view
    where fnc (tableName,notice) myBuff view = do
                                      textBufferInsertAtCursor myBuff $ notice ++ ":" ++ tableName ++ "\n"

downScrolledWindow :: ScrolledWindow -> IO ()
downScrolledWindow scrwin = do
    aj   <- scrolledWindowGetVAdjustment scrwin
    lo   <- adjustmentGetLower aj
    up   <- adjustmentGetUpper aj
    pz   <- adjustmentGetPageSize aj
    adjustmentSetValue aj (up - pz)
