## Parrot

![Parrot](https://raw.githubusercontent.com/jawline/Parrot2/main/parrot.png)

Parrot is a static website generator written in Ocaml. The website generator
takes a series of markdown articles and a website template and generates
a static website with the articles rendered to HTML, pages for navigation,
and a homepage. This tool can be used to develop a personal website or blog
without SaaS dependencies, JavaScript, or bloat.

### Generating a website

We use dune, the Ocaml build tool, to compile Parrot. To compile a
website from the source directory execute `$ dune exec cli/parrot.exe -- INPUT_DIRECTORY`.
Files will be placed in INPUT_DIRECTORY/_build.

For examples on the directory structure see
[https://github.com/jawline/website/](https://github.com/jawline/website/),
the source for my personal blog.
