# csn-search

Search for album on old.chiasenhac.vn from the command line.

Install:

First, make sure `~/.local/bin` is available in your `$PATH`. Then run the following command:

```
stack install
```

Usage:

```
$ csn-search <term>
$ csn-search <term> mode=album
$ csn-search <term> mode=artist
$ csn-search <term> mode=lyric
```

Example:

```
$ csn-search amon amarth
$ csn-search deceiver of the gods mode=album
$ csn-search we are here to drink your beer mode=lyric
```

Search and copy the album URL to clipboard:

```
csn-search we are here to drink your beer mode=lyric | grep beer-beer.korpiklaani | pbcopy
```

Then you can use [chiasenhac-fetcher](https://github.com/huytd/chiasenhac-fetcher) and [aria2c](https://aria2.github.io/) to download the whole album:

```
chiasenhac-fetcher http://old.chiasenhac.vn/nghe-album/beer-beer~korpiklaani~tsvqccqwqevve9.html | aria2c -i -
```
