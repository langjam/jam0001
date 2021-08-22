#include "prelude.h"

namespace lafun {

std::string latexPrelude = R"latex(\documentclass{article}

% Useful packages
\usepackage{amsmath}
\usepackage{listings}
\usepackage[colorlinks=true, allcolors=blue]{hyperref}

\lstset{ % General setup for the package
    basicstyle=\small\ttfamily,
    tabsize=4,
    columns=fixed,
    showstringspaces=false,
    showtabs=false,
    keepspaces,
}

\begin{document}
)latex";

std::string latexPostlude = R"latex(
\end{document}
)latex";

}
