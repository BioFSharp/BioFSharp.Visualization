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


    open Plotly.NET.LayoutObjects
    open FSharp.Stats.ML.Unsupervised.HierarchicalClustering

    type Chart with

        /// <summary>
        /// Build a dendrogram from an FSharp.Stats Cluster&lt;'T&gt;.
        /// toLabel converts your leaf tag to a label string (e.g. id -> name).
        /// Use MonotoneHeights=true if your node 'dist' is a merge height; set false if it's a branch length.
        /// </summary>
        static member Dendrogram
            (
                tree: Cluster<'T>,
                toLabel: 'T -> string,
                ?MonotoneHeights: bool,
                ?LeafTextAngle: float
            ) =
            
            // options with sensible defaults to match repo style
            let monotone     = defaultArg MonotoneHeights true
            let leafAngle    = defaultArg LeafTextAngle -90.0

            // 1) Left-to-right leaf order
            let rec leaves = function
                | Leaf(id,_,tag)      -> [id, tag]
                | Node(_,_,_,l,r) -> leaves l @ leaves r
            let orderedLeaves = leaves tree

            // 2) Assign x positions to leaves (0,1,2,...) and map by leaf id
            let idToX =
                orderedLeaves
                |> List.mapi (fun i (id,_) -> id, float i)
                |> Map.ofList

            // 3) Layout: compute segment list; optionally enforce monotone heights
            let rec layout = function
                | Leaf(id,_,_) ->
                    let x = idToX.[id]
                    x, 0.0, []
                | Node(_, dist, _, l, r) ->
                    let xL, yL, segL = layout l
                    let xR, yR, segR = layout r

                    // If 'dist' is merge height, keep monotone; if it's a branch length, add.
                    let yN =
                        if monotone then max dist (max yL yR)
                        else max yL yR + dist

                    let segs =
                        [ ((xL, yL), (xL, yN))
                          ((xR, yR), (xR, yN))
                          ((xL, yN), (xR, yN)) ]

                    let xC = (xL + xR) / 2.0
                    xC, yN, segL @ segR @ segs

            let _,_,segments = layout tree

            // 4) One line trace; use NaNs to break segments
            let xs =
                segments
                |> List.collect (fun ((x1,_),(x2,_)) -> [x1; x2; System.Double.NaN])
            let ys =
                segments
                |> List.collect (fun ((_,y1),(_,y2)) -> [y1; y2; System.Double.NaN])

            let branches =
                Chart.Scatter(
                    x = xs,
                    y = ys,
                    mode = StyleParam.Mode.Lines,
                    Name = "branches"
                )
                |> Chart.withLine (Line.init())

            // 5) Leaf labels at y=0, rotated
            let leafXs    = orderedLeaves |> List.map (fun (id,_) -> idToX.[id])
            let leafTexts = orderedLeaves |> List.map (fun (_,tag) -> toLabel tag)

            let annos =
                [ for x, txt in List.zip leafXs leafTexts ->
                    Annotation.init(
                        X = x, Y = 0.0, Text = txt,
                        YAnchor = StyleParam.YAnchorPosition.Top,
                        TextAngle = leafAngle,
                        ShowArrow = false
                    ) ]

            // Pad the y-range a bit
            let maxY =
                segments
                |> List.collect (fun ((_,y1),(_,y2)) -> [y1; y2])
                |> List.fold (fun m v -> if v > m then v else m) 0.0

            [ branches ]
            |> Chart.combine
            |> Chart.withAnnotations annos
            |> Chart.withYAxisStyle(MinMax = (0.0, maxY * 1.05))
            |> Chart.withXAxis(
                LinearAxis.init(
                    ShowTickLabels = false,
                    Ticks = StyleParam.TickOptions.Empty,
                    ShowGrid = false,
                    ZeroLine = false
                )
            )
