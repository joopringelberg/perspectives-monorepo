# Perspectives Language Reference Manual

The editable AsciiDoc reference manual, imported from the `reference` directory
of `perspectives-documentation` without its Git history. Generated HTML and PDF
files are not versioned. The rest of that documentation repository remains
separate.

## Setup

Install Node.js and pnpm as required by the monorepo, plus Ruby 3.0 or newer and
Bundler (`gem install bundler` if needed). From the monorepo root:

```sh
pnpm install
pnpm --filter perspectives-reference setup
```

The setup command runs `bundle install` using this package's `Gemfile.lock`.
To keep gems local to the package, first run
`bundle config set --local path vendor/bundle` from this directory.

## Build and preview

From the monorepo root:

```sh
pnpm --filter perspectives-reference... build
pnpm --filter perspectives-reference test
pnpm --filter perspectives-reference build:pdf
```

The trailing `...` builds the workspace dependency `perspectives-highlightjs`
before the manual. The root `pnpm run build` also includes both packages in
dependency order. Once the highlighter has been built, edits to the manual can
be rebuilt with `pnpm --filter perspectives-reference build`, or the VS Code
task **build perspectives-reference**.

Open `output/overview.html` in a browser to read the multipage manual. The PDF
is written to `output-pdf/overview.pdf`. All AsciiDoc includes, images and the
Colony stylesheet are contained in this package. The HTML build bundles
highlight.js and the workspace ARC language definition locally; it does not
load syntax-highlighting scripts from a CDN. Build warnings fail the command.

Edit [overview.adoc](overview.adoc) to change the chapter structure, and the
included `.adoc` files for the text. Assets live in [images](images),
[adoc-colony.css](adoc-colony.css), and [extrastyles.css](extrastyles.css).

## Publish

After building the highlighter, the original manual SSH deployment is available
from the monorepo root:

```sh
pnpm --filter perspectives-reference publish:reference
```

This rebuilds the HTML and copies it to
`joop@inplace.works:/var/www/mycontexts.com/reference/`, preserving the existing
public URL. It requires SSH access to that host and is never run by the normal
build. No deployment is performed during setup or tests.
