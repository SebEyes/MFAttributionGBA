# MFAttributionGBA

An R package containing a Shiny app for adding taxonomy to morphospecies (MF) codes in a working database. The app uses a taxonomy reference table and an MF synonym table, then lets you download the enriched working database as an Excel workbook.

## Input files

The app requires **three input files**. Each can be a CSV (`.csv`), Excel workbook (`.xlsx`), or legacy Excel workbook (`.xls`). CSV files may use commas or semicolons as separators. Include a header row in each file.

### 1. Working database

Your input data to be enriched with taxonomy. It must have a column containing MF codes. The app does not require a particular column name; select the MF column in the app after uploading the file. MF codes can be numbers or text; they are converted to trimmed text for matching.

### 2. MF taxonomy reference (`MF_completeInfo`)

A table mapping MF codes to taxonomy. It must include these exact column names:

| Required column | Use |
| --- | --- |
| `MF` | Reference morphospecies code |
| `order` | Taxonomic order |
| `class` | Taxonomic class |
| `phylum` | Taxonomic phylum |
| ` . . . ` | Taxonomic information following DWC Archive standards |

For reliable results, ensure MF codes and taxonomy values are populated consistently.

### 3. MF synonym table (`MF_synonym`)

A table with **at least two columns**. Column names are not important; column position is:

1. **First column:** synonym/old MF code found in the working database.
2. **Second column:** corresponding current/reference MF code, which must match an `MF` value in the taxonomy reference.

## Matching notes

- MF codes are converted to text and leading/trailing whitespace is removed before matching.
- Matching is exact and case-sensitive; for example, `mf01` and `MF01` are different codes.
- The app resolves a working-database MF code through the synonym table and then looks up its taxonomy in `MF_completeInfo`.
- Codes that do not resolve to the reference table remain unmatched and are shown in the app's unmatched-MF view; check the codes in all three files.
- The app's default code for “not identified” is `NPI` (configurable in the app). NPI entries receive special handling rather than a normal reference lookup.

## Install and launch

Install the package from GitHub and launch the app from R:

```r
install.packages("remotes")
remotes::install_github("SebEyes/MFAttributionGBA")
MFAttributionGBA::run_mf_app()
```

## Output

The app provides the input working database with the taxonomy results for download as an `.xlsx` workbook.
