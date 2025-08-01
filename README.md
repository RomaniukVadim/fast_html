<!--
SPDX-FileCopyrightText: 2017-2019 myhtmlex authors <https://github.com/Overbryd/myhtmlex>
SPDX-FileCopyrightText: 2019-2022 Pleroma Authors <https://pleroma.social>
SPDX-License-Identifier: LGPL-2.1-only
-->

# FastHTML

A C Node wrapping [lexbor](https://github.com/lexbor/lexbor).
Primarily used with [FastSanitize](https://git.pleroma.social/pleroma/fast_sanitize).

* Available as a hex package: `{:fast_html, "~> 2.0"}`
* [Documentation](https://hexdocs.pm/fast_html/fast_html.html)

## Compiling
- GNU Make
- C Compiler
- Erlang 22.0+ with development headers
- (optional) [lexbor](https://github.com/lexbor/lexbor) 2.2.0+

If you want to use a system installation of lexbor, you can set `WITH_SYSTEM_LEXBOR=1` during compilation time. By default it will used the vendored version present at `c_src/lexbor`.

## Benchmarks

The following table provides median times it takes to decode a string to a tree for html parsers that can be used from Elixir. Benchmarks were conducted on a machine with an `AMD Ryzen 9 3950X (32) @ 3.500GHz` CPU and 32GB of RAM. The `mix fast_html.bench` task can be used for running the benchmark by yourself.

| File/Parser          | fast_html (Port) | mochiweb_html (erlang) | html5ever (Rust NIF) | Myhtmlex (NIF)¹ |
|----------------------|--------------------|------------------------|----------------------|----------------|
| document-large.html (6.9M)  | 125.12 ms          | 1778.34 ms             | 395.21 ms            | 327.17 ms      |
| document-small.html  (25K)| 0.50 ms            | 2.76 ms                | 1.72 ms              | 1.19 ms        |
| fragment-large.html  (33K)| 0.93 ms            | 4.78 ms               | 2.34 ms              | 2.15 ms        |
| fragment-small.html²  (757B)| 44.60 μs | 42.13 μs | 43.58 μs | 289.71 μs |

Full benchmark output can be seen in [this snippet](https://git.pleroma.social/pleroma/elixir-libraries/fast_html/snippets/3128)

1. Myhtmlex has a C-Node mode, but it wasn't benchmarked here because it segfaults on `document-large.html`
2. The slowdown on `fragment-small.html` is due to Port overhead. Unlike html5ever and Myhtmlex in NIF mode, `fast_html` has the parser process isolated and communicates with it over stdio, so even if a fatal crash in the parser happens, it won't bring down the entire VM.

## Contribution / Bug Reports

* Please make sure you do `git submodule update` after a checkout/pull
* The project aims to be fully tested
