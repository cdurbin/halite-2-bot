# Using your starter kit

All starter kits should contain a `run_game.sh` and `run_game.bash`, you can use these scripts to quickly run a game of halite. By default, this script runs the basic __Settler__ bot against itself.

## Bot submission guidelines

Before submitting a bot, make sure you adhere to our guidelines, or the upload or compilation of your bot will fail.

1. You should have a `MyBot.{extension for language}` in the root folder of your zip. For Rust, this should be a `cargo.toml`
2. If you are building on top of starter kit provided by us, make sure to include the hlt folder.

## Uploading your bot

* Website: You can use the [play page](https://halite.io/play-programming-challenge) in the Halite website to submit your bot.
* Halite Client: If you a command line experience, you can use the [Halite Client tool](https://halite.io/learn-programming-challenge/halite-cli-and-tools/halite-client-tools) to upload your bot.

## TODO

  * [x] Stop crashing two ships at the beginning
  * [x] Defend planets under siege
  * [x] Don't dock at planets unless I have more fighters near the planet than the enemy
  * [x] Consider where my ships are going if they have already issued a move when calculating obstacles to avoid
  * [ ] Antagonize other players
  * [ ] Chase down 2 on 1 against enemies
  * [ ] Figure out which metrics I should capture and log them
  * [ ] Potentially spread out docking sites around a planet
  * [ ] Don't send more ships than needed for docking or attacking a planet
  * [ ] Look further out to determine if I should dock (undocked enemies within 7 (speed) * 6 (turns to dock) = 42 from radius of planet)
  * [ ] Prevent loss at beginning of game (don't dock if the enemy is too close)
  * [ ] Bigger planets might be better earlier
  * [ ] In four player maps the outside is better
  * [ ] I'm sending too many to the same planet
  * [ ] Change closeby-docked to include my ships - add together my docked plus their docked

# 11/12

  * [ ] Navigation entities-between should first filter out to only look for things within a box
    - [ ] The direction I am looking up to max-thrust + ship radius away. Field of vision is 90 degrees in both directions + 0.5 ship radius buffer.
  * [ ] Navigation looks at changing speeds (potentially look at how fast I could go before hitting object and compare to choosing a different angle (how far away from my goal am I?))
  * [ ] Pick a default location to move to if I run out of time and submit that move (oh but I could run into things.. maybe not)
  * [ ] Stop running into the edges of the map (if y > max-y or < min-y , x... etc)
  * [ ] Docking - keep track of docking during a turn so I keep at least one fighter nearby instead of letting all dock at once.

# 11/21

  * [ ] Too many protecting
  * [ ] Get dominated by massively grouped together ships
  * [ ] Not going after planets when my ships are just sitting around
  *

NOTE: V31 is the same jar as V24 just to see if that one really is better than 25-30.

map_sizes = [80, 80, 88, 88, 96, 96, 96, 104, 104, 104, 104,
             112, 112, 112, 120, 120, 128, 128]
base_size = random.choice(map_sizes)
# Always generate 3:2 aspect ratio
map_width = 3 * base_size
map_height = 2 * base_size

# 11/30

* [ ] Planets with open dock spots should pull the closest ships.

# 12/6

* [ ] When retreating - retreat the minimum distance to still be safe
* [ ] Go back to V33 and see if there are small changes from V34 and above to use


# 12/12
  - Keep using nemesis
  - Target 3 ships at every docked ship

# 12/13
  - Defending against attack - if can't win crash into their ships
  - Fix best planet choice to pick closest planet (including extra dock slot)
