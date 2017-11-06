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
