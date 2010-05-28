module HumaDeRumba where
import Element
import Song

humaDeRumbaNotes :: [Note]
humaDeRumbaNotes =
  [
    (Fire, 0),
    (Water, 1.0),
    (Water, 1.5),
    (Earth, 2.0),
    (Water, 2.5),
    (Water, 3.0)
  ]

humaDeRumbaOrchestras :: [Orchestra]
humaDeRumbaOrchestras =
  [
    (ElementValues {
      water = Left "Untitled1.wav",
      metal = Left "Untitled2.wav",
      earth = Left "Untitled3.wav",
      fire = Left "Untitled4.wav",
      wood = Left "saywhat.wav"
    }, 0.0)
  ]

humaDeRumba :: Song
humaDeRumba =
  Song {
    bpm = 128,
    notes = humaDeRumbaNotes,
    orchestras = humaDeRumbaOrchestras
  }