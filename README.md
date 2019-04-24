# Dungeon Crawler

Download WoW leaderboard data and analyse it. Work in progress!

Using rust to invoke Blizzard API to download leaderboard data and R to draw
pretty pictures from it.

For accessing leaderboard data you need to set up API access
in Blizzard [developer zone](https://develop.battle.net/).
Afer doing so client ID and secret can be specified via `CLIENT_ID` and
`CLIENT_SECRET` environment variables. These variables can be set in `.env`
file something like this:
```
CLIENT_ID="ffffffffffffffffffffffffffffffff"
CLIENT_SECRET="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
```

Because we are using credentials flow there's no need for authorization (with password).

## Building the web crawler

Leaderboard crawler has been implemented in [rust](https://www.rust-lang.org) programming language.

If you already have rust installed building the code is as easy as `cargo build`. Do not be scared of all the dependencies that are downloaded. I have heavily relied on external libraries for web access, deserialization from JSON and serialization to CSV, parallelization, command line parsing etc. To run the executable `cargo run -- --help` (to see help message) or `cargo run -- download --region eu` (to download this weeks leaderboard data for entire eu region).

If you want to track log message while downloading a specific week of leaderboards:
```bash
$ cargo run -- download --region eu --period 687
```

Easiest way to install rust is to use [rustup](https://www.rust-lang.org/tools/install) and follow the guide on the website. This should work for both linux and osx. I have not tested windows.

## Usage of the web crawler

To download most recent week leaderboards of eu realms:
```bash
$ dungeon-crawler download --region eu
```

This creates a CSV file that on every row contains information about individual run and the group composition. Hopefully, the structure of the file is self-evident

Supported regions are 'eu', 'us', 'tw' and 'kr'. Display more options with `--help`.

## Usage of R scripts

I have split R code into two parts:
- `loadCSV.R` reads a set of CSV files, preprocess them and stored database as R object `db.rds`
- `analysis.R` read the database file (default `db.rds`) and draws bunch of charts and graphs from the data.

During preprocessing we remove duplicate entries, convert timestamps to a more usable format, and check if a run was successful or not. We have split data loading into seperate script in order to make testing various charts faster process.

The `analysis.R` file is very much intended to be modified! In general drawing of each plot has been split into a different function. Scroll all the way down to bottom to see what functionality I have played around with in the past.

## TODO

- [x] Command line parameters (week, region).
- [x] ~~Support chinese realms.~~ Turns out the API for CN is not available.
- [ ] Timebox dungeon time limits.
- [ ] Time series analysis (using timestamps attached to data).
- [ ] Use proper OAuth2 library. Currently we are making curl requests.
- [ ] Find a better way to represent groups with multiple tank/healer roles.
- [ ] Correlate data with affixes.

## License

The code is open under MIT license.
