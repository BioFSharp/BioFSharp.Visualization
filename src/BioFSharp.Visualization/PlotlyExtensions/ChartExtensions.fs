namespace BioFSharp.Visualization.Plotly

open BioFSharp.Visualization
open Plotly.NET
open BioFSharp

module ChartExtensions = 
    
    open Utils

    type Chart with
    
        static member SequenceLogo(
            sequences: #seq<#seq<Nucleotides.Nucleotide*float>>,
            ?NucleotideColorScheme: StyleParam.SequenceLogoNucleotideColorScheme,
            ?OutlineColor: Color,
            ?Outline: Line
        ) =
            
            let theme = 
                defaultArg NucleotideColorScheme StyleParam.SequenceLogoNucleotideColorScheme.Classic
                |> getNucleotideColorMap

            let legendShapes = LogoUtils.createLogoLegendShapes(theme = theme, ?OutlineColor = OutlineColor, ?Outline = Outline)

            let sequences = sequences |> Array.ofSeq |> Array.map Array.ofSeq

            let sequenceIndices = getSequenceIndices sequences

            Chart.Point([])
            |> Chart.withShapes (
                sequenceIndices
                |> Array.map (fun index ->
                    LogoUtils.createMotifShapes(
                        sequenceIndex = index,
                        colorLookup = (lookupNucleotideColor theme),
                        motifPosition = (getNucleotideMotifPosition index sequences),
                        ?OutlineColor = OutlineColor, 
                        ?Outline = Outline
                    )
                )
                |> Array.concat
            )
            |> Chart.withShapes(legendShapes, Append=true)

        static member SequenceLogo(
            sequences: #seq<#seq<AminoAcids.AminoAcid*float>>,
            ?AminoAcidColorScheme: StyleParam.SequenceLogoAminoAcidColorScheme,
            ?OutlineColor: Color,
            ?Outline: Line
        ) =
            
            let theme = 
                defaultArg AminoAcidColorScheme StyleParam.SequenceLogoAminoAcidColorScheme.Hydrophobicity
                |> getAminoAcidColorMap

            let legendShapes = LogoUtils.createLogoLegendShapes(theme = theme, ?OutlineColor = OutlineColor, ?Outline = Outline)

            let sequences = sequences |> Array.ofSeq |> Array.map Array.ofSeq

            let sequenceIndices = getSequenceIndices sequences

            Chart.Point([])
            |> Chart.withShapes (
                sequenceIndices
                |> Array.map (fun index ->
                    LogoUtils.createMotifShapes(
                        sequenceIndex = index,
                        colorLookup = (lookupAminoAcidColor theme),
                        motifPosition = (getAminoAcidMotifPosition index sequences),
                        ?OutlineColor = OutlineColor, 
                        ?Outline = Outline
                    )
                )
                |> Array.concat
            )
            |> Chart.withShapes(legendShapes, Append=true)


