# Process Language Parameter

Internal helper to normalize the `lang` parameter to a two-letter code
('en' or 'fr'). Accepts common aliases and falls back to the package
option `PlotFTIR.lang`.

Assistant interne pour normaliser le paramètre `lang` en un code à deux
lettres ('en' ou 'fr'). Accepte les alias courants et utilise par défaut
l'option de package `PlotFTIR.lang`.

## Usage

``` r
.process_language(lang, call = rlang::caller_env())
```

## Arguments

- lang:

  Character string with language preference, or `NA`.

  Chaîne de caractères avec la préférence de langue, ou `NA`.

- call:

  The calling environment for error messages.

  L'environnement d'appel pour les messages d'erreur.

## Value

A two-character string: 'en' or 'fr'.

Une chaîne de deux caractères : 'en' ou 'fr'.
