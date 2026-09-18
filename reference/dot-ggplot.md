# Add GGPlot Layer below others

Inserts a layer *underneath* the existing layers on a ggplot object
simply by calling `plot` - `layer` instead of the usual `plot` +
`layer`.

Insère un calque *sous* les calques existants sur un objet ggplot
simplement en appelant `plot` - `layer` au lieu de l'habituel `plot` +
`layer`.

## Usage

``` r
# S3 method for class 'ggplot'
plot - NULL
```

## Arguments

- plot:

  The plot to which the layer should be added.

  Le tracé auquel le calque doit être ajouté.

- layer:

  The layer to add to the plot.

  La couche à ajouter au tracé.

## Value

The ggplot2 `plot` object, with layer `layer` added underneath.

L'objet ggplot2 `plot`, avec le calque `layer` ajouté en dessous.

## References

modified from https://stackoverflow.com/a/64011534
