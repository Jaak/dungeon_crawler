# Dungeon Crawler

Download WoW leaderboard data and analyse it. Work in progress and currently
only contains code for analysis.

Using rust to invoke blizzard API to download leaderboard data and R to draw
pretty pictures from it.

For accessing Blizzard API for leaderboard data you need to set up API access
in Blizzard [developer zone](https://develop.battle.net/).
Afer doing so client ID and secret can be specified via `CLIENT_ID` and
`CLIENT_SECRET` environment variables. These variables can be set in `.env` file.

Because we are using credentials flow there's no need for authorization (with password).

## TODO

[] Use proper OAuth2 library. Currently we are making curl requests.
[] Find a better way to represent groups with multiple tank/healer roles.

# License

The code is open under MIT license.
