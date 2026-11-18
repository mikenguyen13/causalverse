# E12: draft entries for curated lists

Drafts for the owner to submit on or after the E12 start date (2026-11-23). Nothing
here has been submitted. Each entry follows the list's own format (checked
2026-11-17); re-read the list's README and CONTRIBUTING file before opening the pull
request, since sections and rules change.

## Link tagging

Links to the documentation site carry tags so the dashboard's `experiment_scorecard`
can attribute visits (see "Tagging links for experiments" in
`research-methods-mcp/docs/DASHBOARD.md`): `utm_source` is the site the link is on
(`github`), `utm_medium=directory`, and `utm_campaign=e12-<list>`.

| List | Tagged site link |
| --- | --- |
| awesome-R | `https://mikenguyen13.github.io/causalverse/?utm_source=github&utm_medium=directory&utm_campaign=e12-awesome-r` |
| awesome-causal-inference | `https://mikenguyen13.github.io/causalverse/?utm_source=github&utm_medium=directory&utm_campaign=e12-awesome-causal-inference` |
| awesome-causality-algorithms | `https://mikenguyen13.github.io/causalverse/?utm_source=github&utm_medium=directory&utm_campaign=e12-awesome-causality-algorithms` |
| awesome-economics | `https://mikenguyen13.github.io/causalverse/?utm_source=github&utm_medium=directory&utm_campaign=e12-awesome-economics` |

Some maintainers remove tracking parameters. If a maintainer asks, use the plain URL
`https://mikenguyen13.github.io/causalverse/`; visits then arrive as referrals from
`github.com`, which the scorecard still sees, but not per list. Links to the GitHub
repository itself cannot be tagged; GitHub's traffic page (Insights → Traffic →
Referring sites) shows them.

## 1. awesome-R (qinwf/awesome-R)

- Repository: https://github.com/qinwf/awesome-R
- Section: there is no causal inference or econometrics section. The closest fit is
  **Machine Learning**, which already lists `CausalImpact` ("Causal inference using
  Bayesian structural time-series models"). Alternative: propose a new
  "Causal Inference" section in the pull request, seeded with this entry and the
  CausalImpact, MatchIt, and grf entries.
- Format: `*   [name](url) - description`.

```markdown
*   [causalverse](https://mikenguyen13.github.io/causalverse/?utm_source=github&utm_medium=directory&utm_campaign=e12-awesome-r) - Toolkit for the causal inference workflow: DiD and staggered adoption, synthetic DiD, synthetic control, RD, IV, matching, heterogeneous effects, mediation, and sensitivity analysis, with publication-ready plots and tables.
```

Pull request title: `Add causalverse (causal inference toolkit)`.

## 2. awesome-causal-inference (matteocourthoud/awesome-causal-inference)

- Repository: https://github.com/matteocourthoud/awesome-causal-inference
- File and section: `src/libraries.md`, subsection **R** (entries are alphabetical
  within the subsection; check the order before inserting).
- Format: a top-level bullet with the repository link and a GitHub stars badge,
  then a nested bullet with a description and a docs link. Copy the stars-badge
  markup from a neighbouring entry (it is a shields.io badge for the repository).

```markdown
*   [causalverse](https://github.com/mikenguyen13/causalverse) ![stars](https://img.shields.io/github/stars/mikenguyen13/causalverse?style=social)
    *   Causal inference workflow: DiD, synthetic DiD, synthetic control, RD, IV, event studies, matching, HTE, mediation, sensitivity \[[Docs](https://mikenguyen13.github.io/causalverse/?utm_source=github&utm_medium=directory&utm_campaign=e12-awesome-causal-inference)\]
```

Pull request title: `Add causalverse to R libraries`.

## 3. awesome-causality-algorithms (rguo12/awesome-causality-algorithms)

- Repository: https://github.com/rguo12/awesome-causality-algorithms
- Section: **Toolboxes → Comprehensive** (packages that cover many estimators).
  The Treatment Effect Estimation toolbox table currently lists Python packages only.
- Format: a markdown table row `| Name | Description | Venue | [Language](url) |`.
  The list requires open-source code (causalverse qualifies). Use `Zenodo` and the
  DOI in the venue column if the table asks for a publication.

```markdown
| causalverse | R toolkit for the causal inference workflow: DiD (incl. staggered adoption), synthetic DiD, synthetic control, RD, IV, matching, HTE, mediation, sensitivity analysis, with vignettes for each design | [Zenodo](https://doi.org/10.5281/zenodo.8254063) | [R](https://github.com/mikenguyen13/causalverse) |
```

This entry links to the repository and the DOI, not the documentation site, so it
carries no UTM tags.

## 4. awesome-economics (antontarasenko/awesome-economics)

- Repository: https://github.com/antontarasenko/awesome-economics
- Section: **Economics on GitHub → Projects** (or **Research → Software**, if the
  maintainer prefers). The README says new links may also go to "Links Sent by
  Readers"; follow its contributing guide.
- Format: `- [name](url) - description`.

```markdown
- [causalverse](https://mikenguyen13.github.io/causalverse/?utm_source=github&utm_medium=directory&utm_campaign=e12-awesome-economics) - R package for applied econometrics and policy evaluation: DiD and staggered adoption, synthetic DiD, synthetic control, regression discontinuity, instrumental variables, finance event studies, and panel data diagnostics.
```

## Not eligible yet

- CRAN Task Views (CausalInference, Econometrics): list CRAN packages only.
  causalverse installs from GitHub; submit to CRAN first, then file an issue in
  `cran-task-views/CausalInference` and `cran-task-views/Econometrics`.
- rOpenSci and R-universe are registries, not curated lists; an R-universe entry
  (`mikenguyen13.r-universe.dev`) would also host a second copy of the docs, which
  matters for E2 (one canonical host).
