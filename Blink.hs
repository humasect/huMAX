module Blink where
import Element
import Song

{-
blinkHardNotes :: [Note]
blinkHardNotes =
  [
    (Water,0.063), (Metal,0.44), (Earth,0.63), (Fire,0.82), (Wood,1.21)
  ]

blinkHardOrchestras :: [Orchestra]
blinkHardOrchestras =
  [
    (makeElementValues pre
      "intro1" "intro2" "intro3" "intro4" "intro5"
     ,0)
  ]
  where
  pre a = Left ("Blink/h_" ++ a ++ ".wav")

blinkHard :: Song
blinkHard =
  Song {
    bpm = 128,
    notes = blinkHardNotes,
    orchestras = blinkHardOrchestras
  }

-}

---------------------------------------------------------

blinkEasyNotes :: [Note]
blinkEasyNotes =
  noteBlock "intro" [
    (Water,0.062), (Wood,1.598), (Metal,3.134), (Fire,4.674), (Earth,6.211),
    (Water,7.753), (Wood,9.291), (Earth,10.827)
  ] ++
  noteBlock "begin" [
    (Metal,12.346), (Fire,13.105), (Earth,13.864),
    (Water,15.134), (Water,16.120),
    (Wood,16.942), (Wood,17.708),
    (Metal,18.359), (Fire,19.810), (Fire,20.582),
    (Earth,21.166)
  ] ++
  noteBlock "enter" [
  ]

blinkEasy :: (Int,[Note])
blinkEasy =
  (128, blinkEasyNotes)
