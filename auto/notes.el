(TeX-add-style-hook
 "notes"
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
    "sec:orgb830c8b"
    "sec:orgf9748cd"
    "sec:orgfc5859a"
    "sec:org92ab269"
    "sec:org41b02ed"
    "sec:org3d92af9"
    "sec:org1ec7a19"
    "sec:orgc94b081"
    "sec:org9de67a7"
    "sec:orgd3ead30"
    "sec:org4479265"
    "sec:org37b1692"
    "sec:orgc705b27"
    "sec:orgf567a58"
    "sec:orgea64dda"
    "sec:org8fb8b87"
    "sec:org7e5ff93"
    "sec:orgd681598"
    "sec:org6da2591"
    "sec:orgffd7ac2"
    "sec:org4ffc160"
    "sec:orgd991320"
    "sec:org4b74857"
    "sec:org07b07fc"
    "sec:org9e1ba4b"
    "sec:orgac53be5"
    "sec:org0c29796"
    "sec:orgabea502"
    "sec:org560c019"
    "sec:org5415f4e"
    "sec:org5d11258"
    "sec:org66f37d0"
    "sec:org5d4e333"
    "sec:orgb7a8a96"
    "sec:org0365f27"
    "sec:orgdf8f1c3"
    "sec:org2f1d2e0"
    "sec:orgaa75cc5"
    "sec:orgdb2aa06"
    "sec:org179cde0"
    "sec:org5fc0c10"
    "sec:orgbd59053"
    "sec:orgd6eecb3"
    "sec:org30ce142"
    "sec:orga564a95"
    "sec:orgb954ed2"
    "sec:org8577237"
    "sec:orgf82f951"
    "sec:org121d4b6"
    "sec:org9dc50d2"
    "sec:orgfa8241f"
    "sec:org7729b0b"
    "sec:org989ce96"
    "sec:org1ed0329"
    "sec:org564fa9c"
    "sec:org0fe808a"
    "sec:org7ce59c0"
    "sec:orgea43b71"
    "sec:org3aafd7a"
    "sec:org598e103"
    "sec:org91b3106"
    "sec:orge764471"
    "sec:org254845c"
    "sec:org7f89291"
    "sec:orgeb20268"
    "sec:orgadebf39"
    "sec:org8fcab24"
    "sec:org77372cf"
    "sec:orgeb11fe6"
    "sec:orga1aef41"
    "sec:org6b5055b"
    "sec:org6e75c05"
    "sec:org4f3b609"
    "sec:org7e9f8e3"
    "sec:orgc9de632"
    "sec:org21c7ff6"
    "sec:org733fd0f"
    "sec:orgcc6a0bf"
    "sec:orgc40b24b"
    "sec:org0ce7f0a"
    "sec:org8b24070"
    "sec:org0ad773c"
    "sec:orge13501a"
    "sec:orgf555c12"
    "sec:org3faeaa0"
    "sec:orgcd2c4df"
    "sec:orgf456bec"
    "sec:org95339ce"
    "sec:org5f8b7f0"
    "sec:org486d8f4"
    "sec:org9410d0d"
    "sec:orgfa6234e"
    "sec:orgf716bed"))
 :latex)
