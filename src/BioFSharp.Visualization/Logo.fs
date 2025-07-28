namespace BioFSharp.Visualization

open BioFSharp
open BioFSharp.Nucleotides
open BioFSharp.AminoAcids
open Plotly.NET
open Plotly.NET.LayoutObjects

module Logo =

    // https://weblogo.threeplusone.com/manual.html
    module ColorThemes =
        
        type LogoTheme<'T> = {
            Name: string
            ColorKeys: (string*Color) array
            ColorMap: ('T*Color) array
        } with
            static member create name colorKeys colorMap =
                { Name = name; ColorKeys = colorKeys; ColorMap = colorMap }
            

        module Nucleotides =
            
            let classic = 
                LogoTheme<Nucleotides.Nucleotide>.create 
                    "Classic"
                    [|
                        "A", Color.fromKeyword Green
                        "C", Color.fromKeyword Blue
                        "G", Color.fromKeyword Orange
                        "T+U", Color.fromKeyword Red
                    |]
                    [|
                        Nucleotides.A, Color.fromKeyword Green
                        Nucleotides.C, Color.fromKeyword Blue
                        Nucleotides.G, Color.fromKeyword Orange
                        Nucleotides.T, Color.fromKeyword Red
                        Nucleotides.U, Color.fromKeyword Red
                    |]

            let basePairing = 
                LogoTheme<Nucleotides.Nucleotide>.create 
                    "Base Pairing"
                    [|
                        "2 Watson-Crick hydrogen bonds", Color.fromKeyword DarkOrange
                        "3 Watson-Crick hydrogen bonds", Color.fromKeyword Blue
                    |]
                    [|
                        (Nucleotides.T, Color.fromKeyword DarkOrange)
                        (Nucleotides.A, Color.fromKeyword DarkOrange)
                        (Nucleotides.U, Color.fromKeyword DarkOrange)
                        (Nucleotides.C, Color.fromKeyword Blue)
                        (Nucleotides.G, Color.fromKeyword Blue)
                    |]

        module AminoAcids =
            
            let hydrophobicity = 
                 LogoTheme<AminoAcids.AminoAcid>.create 
                    "Hydrophobicity"
                    [|
                        "Hydrophilic", Color.fromKeyword Blue
                        "Hydrophobic", Color.fromKeyword Green
                        "Neutral", Color.fromKeyword Black
                    |]
                    [|
                        // Hydrophilic
                        Arg, Color.fromKeyword Blue
                        Lys, Color.fromKeyword Blue
                        Asp, Color.fromKeyword Blue
                        Glu, Color.fromKeyword Blue
                        Asn, Color.fromKeyword Blue
                        Gln, Color.fromKeyword Blue
                        // Hydrophobic
                        Tyr, Color.fromKeyword Green
                        Val, Color.fromKeyword Green
                        Met, Color.fromKeyword Green
                        Cys, Color.fromKeyword Green
                        Leu, Color.fromKeyword Green
                        Phe, Color.fromKeyword Green
                        Ile, Color.fromKeyword Green
                        Trp, Color.fromKeyword Green
                        // Neutral
                        Ser, Color.fromKeyword Black
                        Gly, Color.fromKeyword Black
                        His, Color.fromKeyword Black
                        Thr, Color.fromKeyword Black
                        Ala, Color.fromKeyword Black
                        Pro, Color.fromKeyword Black
                    |]

            let chemistry =
                LogoTheme<AminoAcids.AminoAcid>.create 
                    "Chemistry"
                    [|
                        "Polar", Color.fromKeyword Green
                        "Neutral", Color.fromKeyword Purple
                        "Basic", Color.fromKeyword Blue
                        "Acidic", Color.fromKeyword Red
                        "Hydrophobic", Color.fromKeyword Black
                    |]
                    [|
                        // polar
                        Gly, Color.fromKeyword Green
                        Ser, Color.fromKeyword Green
                        Thr, Color.fromKeyword Green
                        Tyr, Color.fromKeyword Green
                        Cys, Color.fromKeyword Green
                        // neutral
                        Gln, Color.fromKeyword Purple
                        Asn, Color.fromKeyword Purple
                        // basic
                        Lys, Color.fromKeyword Blue
                        Arg, Color.fromKeyword Blue
                        His, Color.fromKeyword Blue
                        // acidic
                        Asp, Color.fromKeyword Red
                        Glu, Color.fromKeyword Red
                        // hydrophobic
                        Ala, Color.fromKeyword Black
                        Val, Color.fromKeyword Black
                        Leu, Color.fromKeyword Black
                        Ile, Color.fromKeyword Black
                        Pro, Color.fromKeyword Black
                        Trp, Color.fromKeyword Black
                        Phe, Color.fromKeyword Black
                        Met, Color.fromKeyword Black
                    |]

            let charge = 
                LogoTheme<AminoAcids.AminoAcid>.create 
                    "Charge"
                    [|
                        "Positive", Color.fromKeyword Blue
                        "Negative", Color.fromKeyword Red
                    |]
                    [|
                        // positive
                        Lys, Color.fromKeyword Blue
                        Arg, Color.fromKeyword Blue
                        His, Color.fromKeyword Blue

                        // negative
                        Asp, Color.fromKeyword Red
                        Glu, Color.fromKeyword Red
                    |]
    
    open SVG
    
    let letterPaths = 
        Map [
            (BioItem.name Nucleotide.A), "M 10.72 14 L 8.92 14 L 7.84 10.56 L 2.9 10.56 L 1.84 14 L 0 14 L 4.62 0 L 6.08 0 L 10.72 14 Z M 4.98 3.9 L 3.38 9.02 L 7.34 9.02 L 5.72 3.9 Q 5.48 3.12 5.34 2.54 Q 5.26 3.02 4.98 3.9 Z"
            (BioItem.name Nucleotide.T), "M 5.74 14 L 4.02 14 L 4.02 1.66 L 0 1.66 L 0 0 L 9.76 0 L 9.76 1.66 L 5.74 1.66 L 5.74 14 Z"
            (BioItem.name Nucleotide.G), "M 8.925 2.446 L 7.405 3.326 Q 6.965 2.526 6.425 2.066 Q 5.885 1.606 5.045 1.606 Q 3.905 1.606 3.145 2.506 Q 2.385 3.406 2.095 4.616 Q 1.805 5.826 1.805 7.246 Q 1.805 9.626 2.625 11.256 Q 3.445 12.886 5.045 12.886 Q 6.225 12.886 6.905 12.016 Q 7.585 11.146 7.585 9.706 L 7.585 9.046 L 5.065 9.046 L 5.065 7.466 L 9.385 7.466 L 9.385 9.146 Q 9.385 11.666 8.165 13.076 Q 6.945 14.486 5.045 14.486 Q 3.345 14.486 2.175 13.416 Q 1.005 12.346 0.505 10.756 Q 0.005 9.166 0.005 7.246 Q 0.005 4.226 1.305 2.116 Q 2.605 0.006 5.045 0.006 Q 6.565 0.006 7.445 0.666 Q 8.325 1.326 8.925 2.446 Z"
            (BioItem.name Nucleotide.C), "M 4.54 1.242 L 3.78 1.652 Q 3.56 1.242 3.3 1.022 Q 3.04 0.802 2.63 0.802 Q 2.17 0.802 1.82 1.057 Q 1.47 1.312 1.275 1.732 Q 1.08 2.152 0.985 2.627 Q 0.89 3.102 0.89 3.622 Q 0.89 4.372 1.065 4.987 Q 1.24 5.602 1.645 6.022 Q 2.05 6.442 2.63 6.442 Q 3.43 6.442 3.85 5.552 L 4.62 5.842 Q 4.36 6.462 3.865 6.852 Q 3.37 7.242 2.63 7.242 Q 2.08 7.242 1.625 7.037 Q 1.17 6.832 0.875 6.487 Q 0.58 6.142 0.38 5.672 Q 0.18 5.202 0.09 4.692 Q 0 4.182 0 3.622 Q 0 2.662 0.26 1.867 Q 0.52 1.072 1.13 0.537 Q 1.74 0.002 2.63 0.002 Q 3.96 0.002 4.54 1.242 Z"
            (BioItem.name Nucleotide.U), "M 0 2 L 0 8 C 0 10 4 10 4 8 L 4 2 L 3 2 L 3 8 C 3 9 1 9 1 8 L 1 2 Z"

            (BioItem.name AminoAcid.Ala), "M 10.72 14 L 8.92 14 L 7.84 10.56 L 2.9 10.56 L 1.84 14 L 0 14 L 4.62 0 L 6.08 0 L 10.72 14 Z M 4.98 3.9 L 3.38 9.02 L 7.34 9.02 L 5.72 3.9 Q 5.48 3.12 5.34 2.54 Q 5.26 3.02 4.98 3.9 Z"
            (BioItem.name AminoAcid.Ile), "M 0.95 0.0 Q 1.05 0.0565 0.95 0.113 L 0.606 0.113 L 0.606 0.887 L .95 0.887 Q 1.05 0.9435 .95 1.0 L 0.05 1.0 Q -0.05 0.9435 .05 0.887 L 0.394 0.887 L 0.394 0.113 L 0.05 0.113 Q -0.05 0.0565 0.05 0.0 L 0.95 0.0 Z"
            (BioItem.name AminoAcid.Leu), "M 8.38 14 L 0 14 L 0 0 L 1.72 0 L 1.72 12.34 L 8.38 12.34 L 8.38 14 Z"
            (BioItem.name AminoAcid.Met), "M 1.58 14 L 0 14 L 0 0 L 1.86 0 L 3.96 8.38 Q 4.18 9.26 4.26 9.84 Q 4.32 9.26 4.56 8.38 L 6.68 0 L 8.48 0 L 8.48 14 L 6.9 14 L 6.9 5.22 Q 6.9 4.38 6.94 3.94 Q 6.9 4.16 6.64 5.22 L 4.38 14 L 4.1 14 L 1.84 5.22 Q 1.6 4.16 1.54 3.94 Q 1.58 4.82 1.58 5.22 L 1.58 14 Z"
            (BioItem.name AminoAcid.Val), "M 6.06 14 L 4.62 14 L 0 0 L 1.86 0 L 5.08 10.38 Q 5.2 10.76 5.34 11.5 Q 5.48 10.76 5.6 10.38 L 8.84 0 L 10.68 0 L 6.06 14 Z"
            (BioItem.name AminoAcid.Phe), "M 1.72 14 L 0 14 L 0 0 L 8.78 0 L 8.78 1.64 L 1.72 1.64 L 1.72 5.94 L 6.66 5.94 L 6.66 7.58 L 1.72 7.58 L 1.72 14 Z"
            (BioItem.name AminoAcid.Trp), "M 3.6 14 L 2.2 14 L 0 0 L 1.64 0 L 2.94 9.24 Q 2.96 9.4 2.99 9.71 Q 3.02 10.02 3.04 10.14 Q 3.1 9.68 3.2 9.24 L 4.8 0 L 6 0 L 7.6 9.14 Q 7.66 9.46 7.74 9.96 Q 7.76 9.56 7.84 9.16 L 9.1 0 L 10.76 0 L 8.56 14 L 7.16 14 L 5.52 4.82 Q 5.5 4.68 5.45 4.37 Q 5.4 4.06 5.38 3.88 Q 5.3 4.52 5.24 4.78 L 3.6 14 Z"
            (BioItem.name AminoAcid.Tyr), "M 4.9 6.78 L 7.92 0 L 9.76 0 L 5.76 8.48 L 5.76 14 L 4.04 14 L 4.04 8.48 L 0 0 L 1.92 0 L 4.9 6.78 Z"
            (BioItem.name AminoAcid.Asn), "M 1.68 14 L 0 14 L 0 0 L 1.7 0 L 6.48 9.82 Q 6.8 10.44 6.84 10.56 Q 6.82 10.18 6.82 9.8 L 6.82 0 L 8.48 0 L 8.48 14 L 6.82 14 L 2.02 4.3 Q 1.88 4.06 1.64 3.54 Q 1.68 3.84 1.68 4.3 L 1.68 14 Z"
            (BioItem.name AminoAcid.Cys), "M 4.54 1.242 L 3.78 1.652 Q 3.56 1.242 3.3 1.022 Q 3.04 0.802 2.63 0.802 Q 2.17 0.802 1.82 1.057 Q 1.47 1.312 1.275 1.732 Q 1.08 2.152 0.985 2.627 Q 0.89 3.102 0.89 3.622 Q 0.89 4.372 1.065 4.987 Q 1.24 5.602 1.645 6.022 Q 2.05 6.442 2.63 6.442 Q 3.43 6.442 3.85 5.552 L 4.62 5.842 Q 4.36 6.462 3.865 6.852 Q 3.37 7.242 2.63 7.242 Q 2.08 7.242 1.625 7.037 Q 1.17 6.832 0.875 6.487 Q 0.58 6.142 0.38 5.672 Q 0.18 5.202 0.09 4.692 Q 0 4.182 0 3.622 Q 0 2.662 0.26 1.867 Q 0.52 1.072 1.13 0.537 Q 1.74 0.002 2.63 0.002 Q 3.96 0.002 4.54 1.242 Z"
            (BioItem.name AminoAcid.Gln), "M 7.58 13.381 L 8.68 15.041 L 7.46 15.901 L 6.32 14.201 Q 5.6 14.481 4.76 14.481 Q 3.5 14.481 2.55 13.861 Q 1.6 13.241 1.06 12.181 Q 0.52 11.121 0.26 9.881 Q 0 8.641 0 7.241 Q 0 5.841 0.26 4.601 Q 0.52 3.361 1.06 2.301 Q 1.6 1.241 2.55 0.621 Q 3.5 0.001 4.76 0.001 Q 6.02 0.001 6.97 0.621 Q 7.92 1.241 8.47 2.301 Q 9.02 3.361 9.28 4.601 Q 9.54 5.841 9.54 7.241 Q 9.54 11.481 7.58 13.381 Z M 5.38 12.801 L 4.42 11.381 L 5.68 10.521 L 6.58 11.881 Q 7.74 10.361 7.74 7.241 Q 7.74 4.821 7.01 3.211 Q 6.28 1.601 4.76 1.601 Q 3.72 1.601 3.03 2.471 Q 2.34 3.341 2.06 4.561 Q 1.78 5.781 1.78 7.241 Q 1.78 9.661 2.51 11.271 Q 3.24 12.881 4.76 12.881 Q 5.12 12.881 5.38 12.801 Z"
            (BioItem.name AminoAcid.Ser), "M 0 5.692 L 0.8 5.402 Q 1 5.882 1.39 6.167 Q 1.78 6.452 2.27 6.452 Q 2.82 6.452 3.14 6.147 Q 3.46 5.842 3.46 5.282 Q 3.46 4.372 2.05 3.762 Q 1.65 3.592 1.38 3.437 Q 1.11 3.282 0.815 3.037 Q 0.52 2.792 0.365 2.457 Q 0.21 2.122 0.21 1.702 Q 0.21 0.982 0.74 0.492 Q 1.27 0.002 2.15 0.002 Q 2.86 0.002 3.365 0.347 Q 3.87 0.692 4.02 1.242 L 3.23 1.502 Q 3.1 1.192 2.81 0.992 Q 2.52 0.792 2.12 0.792 Q 1.65 0.792 1.37 1.037 Q 1.09 1.282 1.09 1.702 Q 1.09 1.942 1.195 2.132 Q 1.3 2.322 1.53 2.482 Q 1.76 2.642 1.945 2.742 Q 2.13 2.842 2.48 2.992 Q 2.86 3.162 3.135 3.332 Q 3.41 3.502 3.715 3.772 Q 4.02 4.042 4.18 4.417 Q 4.34 4.792 4.34 5.252 Q 4.34 6.182 3.75 6.712 Q 3.16 7.242 2.23 7.242 Q 1.43 7.242 0.825 6.802 Q 0.22 6.362 0 5.692 Z"
            (BioItem.name AminoAcid.Thr), "M 5.74 14 L 4.02 14 L 4.02 1.66 L 0 1.66 L 0 0 L 9.76 0 L 9.76 1.66 L 5.74 1.66 L 5.74 14 Z"
            (BioItem.name AminoAcid.Asp), "M 3.22 14.001 L 0 14.001 L 0 0.001 L 3.3 0.001 Q 4.82 0.001 5.97 0.601 Q 7.12 1.201 7.77 2.221 Q 8.42 3.241 8.73 4.441 Q 9.04 5.641 9.04 7.001 Q 9.04 8.301 8.75 9.471 Q 8.46 10.641 7.83 11.691 Q 7.2 12.741 6.01 13.371 Q 4.82 14.001 3.22 14.001 Z M 1.74 1.581 L 1.74 12.421 L 3.28 12.421 Q 4.38 12.421 5.18 11.951 Q 5.98 11.481 6.41 10.661 Q 6.84 9.841 7.04 8.941 Q 7.24 8.041 7.24 7.001 Q 7.24 4.721 6.31 3.151 Q 5.38 1.581 3.5 1.581 L 1.74 1.581 Z"
            (BioItem.name AminoAcid.Glu), "M 8.62 14 L 0 14 L 0 0 L 8.2 0 L 8.2 1.64 L 1.72 1.64 L 1.72 5.94 L 6.14 5.94 L 6.14 7.58 L 1.72 7.58 L 1.72 12.36 L 8.62 12.36 L 8.62 14 Z"
            (BioItem.name AminoAcid.Arg), "M 5.64 7.88 L 8.64 14.02 L 6.74 14.02 L 3.82 8.02 L 1.72 8.02 L 1.72 14.02 L 0 14.02 L 0 0.02 L 4.3 0.02 Q 6.64 0.02 7.67 1.15 Q 8.7 2.28 8.7 4 Q 8.7 5.36 7.95 6.45 Q 7.2 7.54 5.64 7.88 Z M 4.34 1.6 L 1.72 1.6 L 1.72 6.44 L 4.34 6.44 Q 5.74 6.44 6.37 5.75 Q 7 5.06 7 4.04 Q 7 3.08 6.38 2.34 Q 5.76 1.6 4.34 1.6 Z"
            (BioItem.name AminoAcid.His), "M 1.72 14 L 0 14 L 0 0 L 1.72 0 L 1.72 5.92 L 6.76 5.92 L 6.76 0 L 8.48 0 L 8.48 14 L 6.76 14 L 6.76 7.56 L 1.72 7.56 L 1.72 14 Z"
            (BioItem.name AminoAcid.Lys), "M 9.32 14 L 7.34 14 L 3.32 6.38 L 1.72 8.4 L 1.72 14 L 0 14 L 0 0 L 1.72 0 L 1.72 6.02 L 6.4 0 L 8.38 0 L 4.5 4.9 L 9.32 14 Z"
            (BioItem.name AminoAcid.Gly), "M 8.925 2.446 L 7.405 3.326 Q 6.965 2.526 6.425 2.066 Q 5.885 1.606 5.045 1.606 Q 3.905 1.606 3.145 2.506 Q 2.385 3.406 2.095 4.616 Q 1.805 5.826 1.805 7.246 Q 1.805 9.626 2.625 11.256 Q 3.445 12.886 5.045 12.886 Q 6.225 12.886 6.905 12.016 Q 7.585 11.146 7.585 9.706 L 7.585 9.046 L 5.065 9.046 L 5.065 7.466 L 9.385 7.466 L 9.385 9.146 Q 9.385 11.666 8.165 13.076 Q 6.945 14.486 5.045 14.486 Q 3.345 14.486 2.175 13.416 Q 1.005 12.346 0.505 10.756 Q 0.005 9.166 0.005 7.246 Q 0.005 4.226 1.305 2.116 Q 2.605 0.006 5.045 0.006 Q 6.565 0.006 7.445 0.666 Q 8.325 1.326 8.925 2.446 Z"
            (BioItem.name AminoAcid.Pro), "M 1.72 14.001 L 0 14.001 L 0 0.001 L 4.32 0.001 Q 5.54 0.001 6.45 0.361 Q 7.36 0.721 7.86 1.331 Q 8.36 1.941 8.6 2.661 Q 8.84 3.381 8.84 4.201 Q 8.84 5.861 7.71 7.121 Q 6.58 8.381 4.32 8.381 L 1.72 8.381 L 1.72 14.001 Z M 1.72 1.581 L 1.72 6.781 L 4.44 6.781 Q 5.82 6.781 6.48 6.021 Q 7.14 5.261 7.14 4.201 Q 7.14 3.161 6.51 2.371 Q 5.88 1.581 4.44 1.581 L 1.72 1.581 Z"
        ]
