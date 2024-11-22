# Slicing Coverage

Track and report code coverage for your package. Compared to traditional approaches,
Slicing Coverage aims to enhance the accuracy of coverage scores by calculating them
based on the program slice resulting from the test’s assertion criteria. This way, we
are able to account for code that's covered but no checked, thus providing a score
that can reflect the tests quality more accurately. This package integrates with most
of covr's functionality and should therefore be easily integrated into existing projects.

To use this package, you also need the program slicer [flowr](https://github.com/flowr-analysis/flowr).

## Development
To get ready for development, you can follow the steps below:
1. Clone the repository:
```bash
git clone https://github.com/flowr-analysis/slicing-coverage-r-package.git
```
2. Enable the project-specific git-hooks:
```bash
git config --local core.hooksPath .githooks/
```
