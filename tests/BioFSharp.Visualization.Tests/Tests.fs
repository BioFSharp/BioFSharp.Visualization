namespace BioFSharp.Visualization.Tests

open Xunit

open BioFSharp.Visualization

type VisualizationTests() =
    [<Fact>]
    let ``yes`` () =
        Assert.True(true, "nah")