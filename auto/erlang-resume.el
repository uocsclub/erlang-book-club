(TeX-add-style-hook
 "erlang-resume"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "margin=1in")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref"
    "minted"
    "geometry")
   (LaTeX-add-labels
    "sec:org4f40aca"
    "sec:org53d2f4e"
    "sec:org957dec1"
    "sec:org11b5bb6"
    "sec:org31c4cb2"
    "sec:org6d2bb00"
    "sec:org4461f10"
    "sec:orgfd92461"
    "sec:org39f4d66"
    "sec:orgc08f606"
    "sec:org11e2dd2"
    "sec:orgca26a1f"
    "sec:org3741366"
    "sec:org86bda5a"
    "sec:org9ec49b1"
    "sec:org5a8b25f"
    "sec:orgac0c484"
    "sec:org496dbca"
    "sec:org01bd7be"
    "sec:org7b88ffa"
    "sec:org24582d7"
    "sec:orga18b5b1"
    "sec:org8974407"
    "sec:org1c0f43c"
    "sec:orgb119fe4"
    "sec:org08760ec"
    "sec:org9ec5260"
    "sec:org7666a87"
    "sec:org3b3520b"
    "sec:org041b7dc"
    "sec:org88ae44b"
    "sec:orgedb13bc"
    "sec:org3185fc6"
    "sec:orgeae6b40"
    "sec:orgf27d561"
    "sec:org58f288d"
    "sec:org0639d3e"
    "sec:org4ecfad9"
    "sec:org5fd7ac9"
    "sec:org2b7166c"
    "sec:org7b58984"
    "sec:orgd04857c"
    "sec:org1d4c954"
    "sec:org5c154b6"
    "sec:orgd8fea7f"
    "sec:orgfe86224"
    "sec:orgbebc611"
    "sec:org4378a12"
    "sec:org1654649"
    "sec:orgd3ed325"
    "sec:org42bc1e3"
    "sec:org3a9c9b6"
    "sec:org81d276f"
    "sec:orgb43bb21"
    "sec:org957dc6e"
    "sec:orgd348a80"
    "sec:org4f3ede9"
    "sec:orgf194eda"
    "sec:org8c398e8"
    "sec:org5b2014a"
    "sec:org79def82"
    "sec:org63846b6"
    "sec:org0494757"
    "sec:org1eb26c8"
    "sec:orgb146462"
    "sec:org9b52dfd"
    "sec:orga109894"
    "sec:org198cd0d"
    "sec:org7391a56"
    "sec:org5d2b9f5"
    "sec:orgf1ff493"
    "sec:orgfdf59f4"
    "sec:orgef7a864"
    "sec:org7c19af7"
    "sec:org5afe7c2"
    "sec:org5a39d33"
    "sec:org32ec018"
    "sec:org690ddd4"
    "sec:orge8e2da0"
    "sec:orgb31ee68"
    "sec:orgcbb716d"
    "sec:orgaf50e7c"
    "sec:org18b84bf"
    "sec:org1cb9a13"
    "sec:org791181a"
    "sec:org9b9fd49"
    "sec:org00ef6ba"
    "sec:orge9d2a23"
    "sec:org4a4819c"
    "sec:org4a1877e"
    "sec:org532ceba"
    "sec:orgc1b7725"
    "sec:org9723912"))
 :latex)

