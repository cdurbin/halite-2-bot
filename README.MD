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

# 12/16
  - Swarm
    - Track anywhere I find a good attack place
    - Track anywhere I find a safe retreat place
    - Each turn check if there's any reachable attack place
      - Move to the one that is closest to a planet I don't own
      - If no attack place, look for the closest(? maybe another choice) safe retreat place and move there
      - If neither, run through code as normal

# 12/19
  - Newly created ships will appear within 2 units from the surface of the planet (i.e. within 2 + radius units of the center of the planet), in the unoccupied space closest to the center of the map. The ship will start with full health and zero velocity. If there is not adequate space anywhere near the planet, the ship will not spawn.
  - Move as far as I can to attack docked ships (don't just sit still to be picked off if I'm in range)


  Rating
  44.58

  μ
  61.38

  σ
  5.6

# 1/13

  * 8 timeouts in 470 games (1.7%)
  * 112 - 3 timeouts in 118 games (2.5%)
  * 114 - 0 timeouts in 200 games
  * 116 - I think 1 timeout - best mu 55.21 after 102 games
  * 117 - 55.5 mu after ~250 games
  * 128 - 55.64 mu after 228 games
  * 129 - 57.24 mu after 337 games!
  * 149 - 57.76 mu after 89 games
  * 153 - 57.7ish mu
  * 154 - 56.71 after 142 games but climbing
  * 155 - 57.01 and climbing after 225 games
  * 157 - 57.92 mu 283 games
  * 163 - 58.4 mu after 101 games, but 57.58 after 188 games

## Implement
  * [1/2] Figure out cases where I'm still doing dumb things with single ships getting killed - tried but still not great
  * [ ] Don't just attack the closest docked ships, find the closest not super well protected docked ships - do some tweaking to figure out how many fighters and what distance from the docked ship to determine what battles are worth fighting.
  * [ ] Totally revamp the defend ship thing. Assign 2? ships to go after each enemy ship that is getting near my docked ships. (seems hard to get right, might need a different algorithm altogether).
  * [ ] More aggressively go after planets no one is near
  * [ ] Add an imaginary ship near spawn points
  * [x] Stop doing the best corner planet thing with my early ships in four player games
  * [ ] Choose a better initial planet (how many ships can I produce in N turns)
  * [ ] Undock (only early in games - takes 5 turns to undock, so if I'm unprotected and units are < 42ish away).
  * [ ] Random 45 degree angle in either direction when fleeing
  * [ ] "Square up" against enemies (probably will never do, but the idea is put my ships in a line perpendicular to the enemy being targeted).
  * [ ] Stay around the edges in four player games (old corner planet implementation wasn't great)
  * [ ] Rotate the weaker ships to the front to take the damage first?
  * [ ] Figure out the optimal position for an attack is it 3 side by side with 1 behind the middle, etc
  * [ ] Attempt to crash into an enemy when they have more health?
  * [ ] Crash into an enemy when they are attacking your docked ship?
  * [ ] If someone is undocking and I'm more than 28 away - abandon my all out attack
  * [ ] Performance improvement - if more than 300 total ships change navigation iterations to just 7 3 1

## Testing
  * [ ] Find maps where closest planet is not a good choice
  * [ ] Find maps where I have a single ship go in and die and find out why
  * [ ] Where I should attack better

## TODO (Top priorities for tomorrow)
  * [ ] Choose a good planet
  * [ ] Figure out how to choose the outside planets in four players
  * [ ] Undock early game rush, maybe try to rush early less often
  * [ ] Try attack spots again? Track good attacks with the point and the enemy, move perpendicular to it
  * [ ]

# 1/17
  * [ ] If I can reach a ship before another one can spawn send up to 3 ships to get it
      * Once I have a list of all my ships, their docked ship, distance
        - Find the smallest of the distances for my ship and keep it, remove the others
        - For any docked ship that has more than 3 attackers, remove anything beyond the 3rd
  * [ ] Attack unprotected ships before I defend (see if this is better - might not want to do together with the first change)
  * [ ] Choosing to move to a planet that already has ships docked
    - Verify that I can get to the planet and back to the same distance away from 2nd closest planet before a new ship could dock there.
  * [ ] Choosing new planets
    * Time it takes to get one ship back in range of the closest enemy should be the tie breaker between two planets (in 2 player) note that it's not always obvious that being close to the enemy is beneficial see https://halite.io/play/?game_id=8241974&replay_class=1&replay_name=replay-20180117-190405%2B0000--2159712529-312-208-1516215809 turn 100.
  * [ ] More complicated way of choosing which docking ships to target (and with how many units)
    - Will need to weight the value of planets based on distance, number of undocked ships nearby, number of docking spots, position of planet relative to other 2 players not involved (nemesis)
    - Use a tiered approach (high value, medium value, low value). Enemy fighters within 19.5 distance of my docked ships might be high value along with docked ships? Could potentially eliminate a lot of the defend logic? Probably not though because looking out 55 definitely has helped.
  * [ ] Verify I am actually calculating the closest planet correctly (I don't ever seem to send ships until after I've fully populated the planet in front of it even when I have an unlimited number of ships)
  * [ ] Never go after the docked ships for the diagonal player from me in four player

TODO
* [ ] Change attack check to not check to see if we can attack before a single ship - instead can we attack before they have enough defenders.
* [ ] Add number of dock spots into priority calculation - start with each dock spot is worth 2 turns (14 distance)
* [ ] Fix jittery moves due to processing planets out of order when a higher priority planet overrides a closer planet
* [ ] Don't send a ship to a planet if it can spawn enough ships to fill the planet before the ship makes it.
* [ ] Don't send a ship to a planet I already own if there's another planet at a similar distance in the opposite direction.
* [ ] Change away from ordering planets to a value for each planet (I can then calculate distance into the formula.)

V165:  Games Win % Loss %
 reCurs3	30	30	70
