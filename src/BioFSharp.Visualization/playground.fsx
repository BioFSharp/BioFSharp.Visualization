#r "nuget: Plotly.NET, 6.0.0-preview.1"
#r "nuget: BioFSharp, 2.0.0"

#I "../../src/BioFSharp.Visualization/bin/Debug/netstandard2.0"
#r "BioFSharp.Visualization.dll"

open Plotly.NET
open Plotly.NET.LayoutObjects
open BioFSharp
open DynamicObj
open BioFSharp.AminoAcids
open BioFSharp.Nucleotides
open BioFSharp.Visualization
open BioFSharp.Visualization.Plotly
open BioFSharp.Visualization.Plotly.ChartExtensions

let aminos = 
    [|
        [| (Ala, 0.3); (Glu, 0.4); (His, 0.2); (Leu, 0.1); (Pro, 0.2); (Ser, 0.1); (Trp, 0.1) |]
        [| (Cys, 0.1); (Phe, 0.2); (Ile, 0.1); (Met, 0.2); (Gln, 0.1); (Thr, 0.2); (Tyr, 0.2) |]
        [| (Asp, 0.4); (Gly, 0.3); (Lys, 0.4); (Asn, 0.2); (Arg, 0.4); (Val, 0.2) |]
    |]

let nucleos = 
    [|
        [| (U, 0.3); (G, 0.4); (G, 0.2); (T, 0.1) |]
        [| (A, 0.1); (C, 0.2); (G, 0.1); (T, 0.2) |]
        [| (T, 0.4); (U, 0.3); (G, 0.4); (T, 0.2) |]
    |]

[
    Chart.SequenceLogo(
        aminos, 
        AminoAcidColorScheme = StyleParam.SequenceLogoAminoAcidColorScheme.Chemistry,
        Outline = Line.init(Width = 0)
    )
    Chart.SequenceLogo(nucleos)
    |> GenericChart.mapLayout (fun l ->
        l?shapes
        |> unbox<seq<Shape>>
        |> Seq.iter (fun s ->
            s?xref <- "x2"
            s?yref <- "y2"
        )
        l
        
    )
]
|> Chart.SingleStack(SubPlotTitles = ["Amino Acids"; "Nucleotides"])
|> Chart.show

AminoAcids.standardSet |> Seq.map (fun a -> BioItem.symbol a, BioItem.name a, a) |> Seq.iter (printfn "%A")