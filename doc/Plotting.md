# Plotting

There is currently support for plots of variable sections reduced to two dimensions, resulting in a heatmap style plot similar to what Ncview or Panoply do.

To open a plot from the GUI, select a floating point variable of at least two dimensions, and press the `Plot` button. Programmatic access is not yet implemented.

The plot uses that statistics [cache](Cache.md) for scale normalization. If the cache is not available upon opening the plot, it will begin with per-frame normalization and issue the cache to be actualized. This is indicated by all the normalization check boxes being selected and disabled. Once the cache is ready, the check boxes become enabled.

The plot shows magnitudes on a psychooptically balanced scala from black via purple and green to yellow and white. The plotting is done through JFreeChart, and a context menu allows to export the current plot as an image file, and to zoom in and out.

## Normalization

For variables having more than two dimensions, single indices in the dimensions which do not form the X- and Y-axes are used to section the matrix down to two dimension. The sectioning dimensions are listed below the plot. For any of these dimensions, a check box on the left hand side activates or deactivates normalization across this dimension. For example, selecting the check box for dimension `"time"` means that normalization is performed _per time slot_. Selecting the check box for `"plev"` means that normalization is performed _per pressure level_. If all boxes are checked, each frame is normalized on its own. If none of the boxes is checked, data is only normalized across the total dataset. A future version will allow to explicitly state minimum and maximum bounds.

## Sectioning

The slider and number spinner next to each dimension determine the index which is used to section the matrix in that dimension.
