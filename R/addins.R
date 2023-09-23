insertShortAnswer <- function() {
  rstudioapi::insertText(' `r short_answer(r"--(   )--")` ')
}

insertExercise <- function() {
  contents <- paste0('::: {.callout-note collapse="true"}\n',
  glue::glue('`r this_exercise(ID="{new_exercise_name()}")`'),
  "\n\n\n\n:::\n\n",
  collapse="\n")

  rstudioapi::insertText(contents)
}


new_exercise_name <- function() {
  paste(
    sample(animal_words, 1),
    sample(verb_words, 1),
    sample(everyday_nouns, 1),
    sample(LETTERS, 1),
    sample(0:9, 1),
    sep = "-"
  )
}

new_exercise_hash <- function() {
  stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]")
}


animal_words <- c(
  "ant", "aspen", "ash", "ape",
  "bear", "bee", "bird", "boy", "beech",
  "buck", "birch",
  "camel", "cat", "cheetah", "chicken", "calf",
  "cow", "crocodile", "child", "crow", "calf",
  "deer", "dog", "dolphin", "duck", "doe", "daisy",
  "eagle", "elm", "elephant",
  "fish", "fly", "fox", "frog", "finch", "falcon", "fir", "fawn",
  "giraffe", "goat", "goldfish", "girl",
  "hamster",  "horse",
  "kangaroo", "kitten", "kid", "lamb", "lion", "lobster",
  "maple", "monkey", "octopus", "owl", "oak",
  "panda", "pig", "puppy",  "pine", "pollen", "pony",
  "rabbit", "rat", "rhinosaurus", "reptile",
  "seal", "shark", "sheep", "snail", "snake", "spider", "seahorse",
  "squirrel", "spruce", "seaweed",
  "tiger", "turtle", "titmouse",
  "walnut", "wolf", "zebra"
)

verb_words <- c(
  "beat", "become", "begin", "bend","bet", "bid", "bite",
  "blow",  "break", "bring", "build", "burn", "buy",
  "catch", "chew", "choose",
  "come",  "cost", "cut",
  "dig", "dive", "do", "draw", "dream",  "drive",  "drink",
  "eat",
  "fall", "feel", "fight",
  "find", "fly", "forget", "forgive", "freeze",
  "get", "give", "go", "grow",
  "hang",  "have",  "hear", "hide", "hit",  "hold",  "hurt",
  "iron", "jump",
  "keep",  "know",
  "light", "lay",  "lead", "leave",  "lend",
  "let", "lie", "lose", "look",
  "make", "mean",  "meet", "mute",
  "pay","put", "pitch",
  "read","ride", "ring","rise",  "run",
  "say", "sail", "see",  "sell","send",  "show",   "shut",  "sing",
  "sit", "sleep",  "speak", "spend", "stand",   "swim", "sharpen",
  "take", "talk", "teach",
  "tear",  "tell", "think",  "throw", "toss", "trim", "tug", "type",
  "understand", "walk",  "wake",  "wear", "win", "write"
)

everyday_nouns <- c(
  "bed", "blanket", "boat", "book", "bottle", "bowl", "bulb",
  "candy", "canoe", "car", "chair", "clock", "closet", "coat", "cotton",
  "dish", "door", "drawer", "dress",
  "fork", "futon", "fridge", "glasses", "gloves",
  "hamper",
  "jacket", "kayak", "kitchen", "knife", "knob",
  "lamp", "linen", "laundry",
  "magnet", "map", "mug", "mattress",
  "oven", "painting", "pantry",
  "pants", "pen", "pencil", "piano", "plant", "plate", "pot", "pan",
  "radio", "ring", "roof", "room", "rug",
  "saucer", "saw", "scarf", "sheet", "ship", "shirt", "sofa", "screen",
  "shoe", "socks", "sofa", "spoon", "stove",
  "table", "tv", "teacup", "vase", "window")


