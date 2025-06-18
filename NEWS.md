# choroplethr 5.0.0

-   Columns no longer need to be named "value" and "region"! Instead, specify *value.name* and *geoid.name* as the variable to be plotted and the region names, respectively.
-   There are now multiple ways to identify each region in your map, as specified by *geoid.type*. For example, abbreviations, proper names, and FIPS code are now accepted for U.S. state level data. If geoid.type is not given, the package will try to automatically guess the type of geographic identifier being used.
-   Regions in your map can now be labeled using the *ggrepel* package, which intelligently places labels and prevents them from intersecting.
-   Many more customization options are now available for your map, including custom colors, styles, and map projections.
-   Added a `NEWS.md` file to track changes to the package.
