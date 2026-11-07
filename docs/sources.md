# Format references

The implementation uses publicly documented formats. The following material informed the design and validation. Third-party source files are not included in the synthetic demo.

- [NONMEM output files](https://nmhelp.tingjieguo.com/nonmemo), [raw output](https://nmhelp.tingjieguo.com/rawout), [additional output](https://nmhelp.tingjieguo.com/addtlout), and [$TABLE](https://nmhelp.tingjieguo.com/%24table), public NONMEM 7.5.1 Help.
- [PsN execute guide](https://github.com/UUPharmacometrics/PsN/blob/b897860f97d7f220d2c6f6f3e3e553698de37577/doc/execute_userguide.tex).
- [PsN bootstrap guide](https://github.com/UUPharmacometrics/PsN/blob/b897860f97d7f220d2c6f6f3e3e553698de37577/doc/bootstrap_userguide.tex).
- [PsN VPC/NPC guide](https://github.com/UUPharmacometrics/PsN/blob/b897860f97d7f220d2c6f6f3e3e553698de37577/doc/vpc_npc_userguide.tex).
- [xpose run001 example](https://github.com/UUPharmacometrics/xpose/tree/a2e988966cc898ce86798df6732cd71414702732/inst/extdata), used as an external numerical check. The upstream xpose package identifies its license as LGPL-3.
- [Bergstrand et al., Prediction-Corrected Visual Predictive Checks for Diagnosing Nonlinear Mixed-Effects Models](https://pmc.ncbi.nlm.nih.gov/articles/PMC3085712/), AAPS Journal, 2011.
- [Nguyen et al., Model Evaluation of Continuous Data Pharmacometric Models: Metrics and Graphics](https://ascpt.onlinelibrary.wiley.com/doi/10.1002/psp4.12161), CPT PSP, 2017.
- [Shiny plot interactions](https://shiny.posit.co/r/reference/shiny/latest/plotoutput.html), [xpose4 VPC intervals](https://uupharmacometrics.github.io/xpose4/reference/xpose.VPC.html).
- [GitHub checkout action](https://github.com/actions/checkout) and [R community Actions](https://github.com/r-lib/actions), used by the supplied CI configuration.

References were inspected on 2026-11-07. The synthetic fixture generator independently creates its own values and does not reproduce patient records from these references.
