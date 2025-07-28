namespace BioFSharp.Visualization.Plotly

open Plotly.NET
open Plotly.NET.LayoutObjects
open BioFSharp
open BioFSharp.Visualization
open BioFSharp.Visualization.Logo
open BioFSharp.Visualization.SVG

module Utils =

    let internal getNucleotideColorMap (m: StyleParam.SequenceLogoNucleotideColorScheme) =
        match m with
        | StyleParam.SequenceLogoNucleotideColorScheme.Custom table -> table
        | StyleParam.SequenceLogoNucleotideColorScheme.Classic      -> ColorThemes.Nucleotides.classic
        | StyleParam.SequenceLogoNucleotideColorScheme.BasePairing  -> ColorThemes.Nucleotides.basePairing

    let internal getAminoAcidColorMap (m: StyleParam.SequenceLogoAminoAcidColorScheme) =
        match m with
        | StyleParam.SequenceLogoAminoAcidColorScheme.Custom table      -> table
        | StyleParam.SequenceLogoAminoAcidColorScheme.Hydrophobicity    -> ColorThemes.AminoAcids.hydrophobicity
        | StyleParam.SequenceLogoAminoAcidColorScheme.Chemistry         -> ColorThemes.AminoAcids.chemistry
        | StyleParam.SequenceLogoAminoAcidColorScheme.Charge            -> ColorThemes.AminoAcids.charge

    let internal lookupNucleotideColor (theme: ColorThemes.LogoTheme<Nucleotides.Nucleotide>) (nuc: Nucleotides.Nucleotide) =
        theme.ColorMap
        |> Array.tryFind (fun (n, _) -> n = nuc)
        |> Option.map snd
        |> Option.defaultValue (Color.fromKeyword Gray)

    let internal lookupAminoAcidColor (theme: ColorThemes.LogoTheme<AminoAcids.AminoAcid>) (aa: AminoAcids.AminoAcid) =
        theme.ColorMap
        |> Array.tryFind (fun (a, _) -> a = aa)
        |> Option.map snd
        |> Option.defaultValue (Color.fromKeyword Gray)

    let internal getSequenceIndices (sequences: #seq<#seq<#IBioItem*float>>) =
        let maxLength =
            sequences
            |> Seq.map Seq.length
            |> Seq.max
        [|1 .. maxLength|]

    let internal getMotifPosition (index: int) (sequences: #seq<#seq<#IBioItem*float>>) =
        sequences
        |> Seq.choose (Seq.tryItem (index - 1))
        |> Seq.sortBy snd
        |> Seq.map (fun (item, value) -> item, value)
        |> Array.ofSeq

    let internal getNucleotideMotifPosition (index: int) (sequences: #seq<#seq<Nucleotides.Nucleotide*float>>) =
        sequences |> getMotifPosition index

    let internal getAminoAcidMotifPosition (index: int) (sequences: #seq<#seq<AminoAcids.AminoAcid*float>>) =
        sequences |> getMotifPosition index

    type LogoUtils =
        
        static member createLogoLegendShapes (
            theme:ColorThemes.LogoTheme<_>,
            ?OutlineColor: Color,
            ?Outline: Line
        ) =
            
            let outline =
                Outline
                |> Option.defaultValue (Plotly.NET.Line.init ())
                |> Plotly.NET.Line.style (
                    ?Color = OutlineColor,
                    Width = 0.
                )
        
            theme.ColorKeys
            |> Array.map (fun key ->
                Shape.init(
                    FillColor = snd key,
                    Name = fst key,
                    ShowLegend = true,
                    LegendGroup = theme.Name,
                    LegendGroupTitle = Title.init($"Logo Theme: {theme.Name}"),
                    ShapeType = StyleParam.ShapeType.Circle,
                    Line = outline,
                    X0 = 1.,
                    Y0 = 0.,
                    X1 = 1.,
                    Y1 = 0.
                )
            )

        static member createMotifShapes (
            sequenceIndex: int,
            colorLookup: #IBioItem -> Color,
            motifPosition: #seq<#IBioItem*float>,
            ?OutlineColor: Color,
            ?Outline: Line        
        ) =
        
            let outline =
                Outline
                |> Option.defaultValue (Plotly.NET.Line.init ())
                |> Plotly.NET.Line.style (
                    ?Color = OutlineColor
                )
        
            let x = float sequenceIndex
            let mutable currentPosBottom = 0.0
            let mutable currentNegTop = 0.0

            motifPosition 
            |> Seq.map (fun (letter, y) ->
                let path = 
                    Map.tryFind (BioItem.name letter) letterPaths
                    |> fun x -> 
                        if x.IsNone then printfn $"wtf {letter}" 
                        x
                    |> Option.defaultValue ""
                    |> SVGPath
                    |> SVGPath.normalize
                if y < 0 then
                    // letter should stack below x axis
                    let top = currentNegTop
                    let bottom = currentNegTop + y
                    currentNegTop <- currentNegTop + y
                    let svg = 
                        path
                        |> SVGPath.reposition (x - 0.5) (x + 0.5) bottom top
                    Shape.init(
                        Path = SVGPath.toString svg, 
                        FillColor = colorLookup letter, 
                        Line = outline
                    )
                else
                    // letter should stack above x axis
                    let bottom = currentPosBottom
                    currentPosBottom <- bottom + y
                    let top = bottom + y
                    let svg = 
                        path
                        |> SVGPath.reposition (x - 0.5) (x + 0.5) bottom top
                    Shape.init(Path = SVGPath.toString svg, FillColor = colorLookup letter, Line = outline)
            )
            |> Seq.toArray