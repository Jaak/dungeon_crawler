# Dungeon Crawler

Download WoW leaderboard data and analyse it. Work in progress!

Using rust to invoke Blizzard API to download leaderboard data and R to draw
pretty pictures from it.

For accessing leaderboard data you need to set up API access
in Blizzard [developer zone](https://develop.battle.net/).
Afer doing so client ID and secret can be specified via `CLIENT_ID` and
`CLIENT_SECRET` environment variables. These variables can be set in `.env` file.

Because we are using credentials flow there's no need for authorization (with password).

## Usage

To download most recent week leaderboards of eu realms:
```bash
$ dungeon-crawler download --realm eu
```

Supported regions are 'eu', 'us', 'tw' and 'kr'. Display more options with `--help`.

## TODO

- [x] Command line parameters (week, region).
- [x] ~~Support chinese realms.~~ Turns out the API for CN is not available.
- [ ] Time series analysis (using timestamps attached to data).
- [ ] Use proper OAuth2 library. Currently we are making curl requests.
- [ ] Find a better way to represent groups with multiple tank/healer roles.
- [ ] Correlate data with affixes.

## License

The code is open under MIT license.
