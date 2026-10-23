/// Messages between the page and the training worker.
/// Anonymous records compile to plain JS objects. Structured clone drops class prototypes,
/// so F# records and unions would arrive broken.
module SnakeEvo.Protocol

open SnakeEvo.Evolution

/// Desired worker state, sent whole. Each order is idempotent, so no command can arrive out of context.
/// A new Run starts a fresh population from Seed.
type Order = {| Run: int; Seed: uint32; Running: bool |}

/// One finished generation. Run lets the page drop reports from a population it already replaced.
type Report =
    {| Run: int
       Generation: int
       Brain: float[][][]
       Seed: uint32
       Score: int |}

let report run (c: Champion) : Report =
    {| Run = run
       Generation = c.Generation
       Brain = c.Brain
       Seed = c.Seed
       Score = c.Score |}

let champion (r: Report) : Champion =
    { Generation = r.Generation
      Brain = r.Brain
      Seed = r.Seed
      Score = r.Score }
