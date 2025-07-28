namespace BioFSharp.Visualization.Plotly

open BioFSharp.Visualization

open System
open Plotly.NET
open BioFSharp
open BioFSharp.Visualization.Logo.ColorThemes

module StyleParam =
        
    type SequenceLogoNucleotideColorScheme =
        | Custom of LogoTheme<Nucleotides.Nucleotide>
        | Classic
        | BasePairing

    type SequenceLogoAminoAcidColorScheme =
        | Custom of LogoTheme<AminoAcids.AminoAcid>
        | Hydrophobicity
        | Chemistry
        | Charge