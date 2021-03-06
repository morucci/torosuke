# torosuke

Torosuke is my Rabbit but also a Candle crawler for Binance.

Torosuke retreives historical data for a given pair and computes a MACD analysis.

There are three running modes:

- Live runner: Constantly fetch last candles and perform the analysis.
- Histo runner: Retreive historical candles for a given pair.
- Histo analyst: run historical analysis on historical candles.

Torosuke stores fetched data and analysis in the `$cwd/store` directory.

## Live runner

```ShellSession
$ cabal run torosuke-live-crawler -- --pair ADAUSDT --interval 1h
```

## Histo runner

```ShellSession
$ cabal run torosuke-histo-crawler -- --pair ADAUSDT --interval 1h --start '2020-01-01 00:00:00 Z' --end '2019-01-01 00:00:00 Z'
```

## Histo analyst

```ShellSession
$ cabal run torosuke-histo-analyst -- --pair ADAUSDT --interval 1h
```

## Store

```ShellSession
[user@c6d4bf1ab000 torosuke]$ find store/
store/
store/ADAUSDT
store/ADAUSDT/1h_analysis.json
store/ADAUSDT/1h_current.json
store/ADAUSDT/1h.json
```

For a given pair three files are created in the store:

- 1h.json is the list of candles retrieved.
- 1h_current.json is the current (live) candle.
- 1h_analysis.json contains the MACD analysis for the last 100 candles.
