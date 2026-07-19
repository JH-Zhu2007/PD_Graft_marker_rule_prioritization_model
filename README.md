# DA neuron / graft-related transcriptomic cell-state prioritisation framework

This repository package contains scripts, source manifests, provenance tables and manuscript-supporting text for a source-traceable computational transcriptomic prioritisation framework.

## Scope

The project prioritises candidate dopaminergic neuron and graft-related transcriptomic cell states using multi-layer computational evidence.

Allowed interpretation:

- source-traceable computational transcriptomic prioritisation framework
- candidate transcriptomic cell states
- candidate transcriptomic marker signatures
- marker-rule-derived prioritization model audit
- graph-based transcriptomic pseudotime/module support
- proxy/contextual evidence support

Not claimed:

- clinical-use model
- validated diagnostic, prognostic or therapeutic biomarker
- graft efficacy or clinical safety prediction
- anatomical-projection claim
- barcode-lineage claim
- genetic causality or disease mechanism proof

## Repository structure

- `scripts/`: discovered R scripts copied from the local workflow, including the currently sourced 12K V2 script when available.
- `tables/`: selected panel/caption/source tables.
- `docs/provenance/`: locked module provenance and reproducibility checklist.
- `docs/claim_boundary/`: allowed/prohibited claim wording statement.
- `docs/manuscript_text/`: Results, Discussion, Methods and code availability text outputs.
- `metadata/`: script index and data/source manifest.

## Reproducibility principle

Final manuscript-preparation modules were designed to read locked upstream outputs only and to avoid same-module old-output reuse. Raw public data are not redistributed here and should be retrieved from original public repositories.

## Script archive note

The workflow script index records scripts discovered locally at the time this package was generated. If earlier scripts were run from temporary download folders and were not saved under the project directory, they should be manually added before public GitHub release.

## Module status

The package is generated from locked 12J Methods/reproducibility outputs and is ready for 12L journal/cover-letter planning.
