namespace BioFSharp.Visualization

module SVG =

    type InvertDirection =
        | X
        | Y
        | XY

    type Coordinate2D(x: float, y: float) =
        member this.X = x
        member this.Y = y

        static member normalizeBy (minx: float) (maxx: float) (miny: float) (maxy: float) (c: Coordinate2D) =
            Coordinate2D((c.X-minx) / (maxx - minx), (c.Y-miny) / (maxy - miny))
    
        static member invert (direction: InvertDirection) (c: Coordinate2D) =
            match direction with
            | X -> Coordinate2D(-c.X, c.Y)
            | Y -> Coordinate2D(c.X, -c.Y)
            | XY -> Coordinate2D(-c.X, -c.Y)

    type SVGPathSegment =
        | M of XY: Coordinate2D
        | L of XY: Coordinate2D
        | Q of XY1: Coordinate2D * XY: Coordinate2D
        | C of XY1: Coordinate2D * XY2: Coordinate2D * XY: Coordinate2D
        | Z

        static member toString (s: SVGPathSegment) =
            match s with
            | M(c)          -> sprintf "M %f %f" c.X c.Y
            | L(c)          -> sprintf "L %f %f" c.X c.Y
            | Q(c1, c)      -> sprintf "Q %f %f %f %f" c1.X c1.Y c.X c.Y
            | C(c1, c2, c)  -> sprintf "C %f %f %f %f %f %f" c1.X c1.Y c2.X c2.Y c.X c.Y
            | Z -> "Z"          

        static member getMaxCoords (s: SVGPathSegment) =
            match s with
            | M(c)          -> (c.X, c.Y)
            | L(c)          -> (c.X, c.Y)
            | Q(c1, c)      -> (max c1.X c.X , max c1.Y c.Y)
            | C(c1, c2, c)  -> (max c1.X (max c2.X c.X) , max c1.Y (max c2.Y c.Y))
            | Z -> (0., 0.)

        static member getMinCoords (s: SVGPathSegment) =
            match s with
            | M(c)          -> (c.X, c.Y)
            | L(c)          -> (c.X, c.Y)
            | Q(c1, c)      -> (min c1.X c.X , min c1.Y c.Y)
            | C(c1, c2, c)  -> (min c1.X (min c2.X c.X) , min c1.Y (min c2.Y c.Y))
            | Z -> (0., 0.)

        static member normalizeBy (minx: float) (maxx: float) (miny: float) (maxy: float) (s: SVGPathSegment) =
            match s with
            | M(c) ->  M(c |> Coordinate2D.normalizeBy minx maxx miny maxy)

            | L(c) ->  L(c |> Coordinate2D.normalizeBy minx maxx miny maxy)
            | Q(c1, c) ->
                Q(
                    c1 |> Coordinate2D.normalizeBy minx maxx miny maxy,
                    c |> Coordinate2D.normalizeBy minx maxx miny maxy
                )
            | C(c1, c2, c) ->
                C(
                    c1 |> Coordinate2D.normalizeBy minx maxx miny maxy,
                    c2 |> Coordinate2D.normalizeBy minx maxx miny maxy,
                    c |> Coordinate2D.normalizeBy minx maxx miny maxy
                )
            | Z -> Z

        static member invert (direction: InvertDirection) (s: SVGPathSegment) =
            match s with
            | M(c) -> M(c |> Coordinate2D.invert direction)
            | L(c) -> L(c |> Coordinate2D.invert direction)
            | Q(c1, c) ->
                Q(
                    c1 |> Coordinate2D.invert direction,
                    c |> Coordinate2D.invert direction
                )
            | C(c1, c2, c) ->
                C(
                    c1 |> Coordinate2D.invert direction,
                    c2 |> Coordinate2D.invert direction,
                    c |> Coordinate2D.invert direction
                )
            | Z -> Z


        static member reposition (anchorLeft:float) (anchorRight:float) (anchorTop:float) (anchorBottom:float) (left:float) (right:float) (top:float) (bottom:float) (s: SVGPathSegment) =
            let x_ratio = (right - left) / (anchorRight - anchorLeft)
            let y_ratio = (top - bottom) / (anchorTop - anchorBottom)
            match s with
            | M(c) ->
                let newX = (c.X - anchorLeft) * x_ratio + left
                let newY = (c.Y - anchorBottom) * y_ratio + bottom
                M(Coordinate2D(newX, newY))
            | L(c) ->
                let newX = (c.X - anchorLeft) * x_ratio + left
                let newY = (c.Y - anchorBottom) * y_ratio + bottom
                L(Coordinate2D(newX, newY))
            | Q(c1, c) -> 
                let newX1 = (c1.X - anchorLeft) * x_ratio + left
                let newY1 = (c1.Y - anchorBottom) * y_ratio + bottom
                let newX = (c.X - anchorLeft) * x_ratio + left
                let newY = (c.Y - anchorBottom) * y_ratio + bottom
                Q(Coordinate2D(newX1, newY1), Coordinate2D(newX, newY))
            | C(c1, c2, c) ->
                let newX1 = (c1.X - anchorLeft) * x_ratio + left
                let newY1 = (c1.Y - anchorBottom) * y_ratio + bottom
                let newX2 = (c2.X - anchorLeft) * x_ratio + left
                let newY2 = (c2.Y - anchorBottom) * y_ratio + bottom
                let newX = (c.X - anchorLeft) * x_ratio + left
                let newY = (c.Y - anchorBottom) * y_ratio + bottom
                C(Coordinate2D(newX1, newY1), Coordinate2D(newX2, newY2), Coordinate2D(newX, newY))
            | Z -> Z

    type SVGPath(path: string, ?Left: float, ?Right: float, ?Top: float, ?Bottom: float) =
        let left, right, top, bottom = 
            defaultArg Right 1.0,
            defaultArg Left 0.0,
            defaultArg Top 1.0,
            defaultArg Bottom 0.0
    
        let splt = path.Split([| ' '; ',' |], System.StringSplitOptions.RemoveEmptyEntries)
        let segments = ResizeArray<SVGPathSegment>()

        let rec loop currentIndex =
            if currentIndex >= splt.Length then
                ()
            else
                let currentSegment = splt.[currentIndex]
                match currentSegment with
                | "M" -> 
                    let x = float splt.[currentIndex + 1]
                    let y = float splt.[currentIndex + 2]
                    segments.Add(M(Coordinate2D(x, y)))
                    loop (currentIndex + 3)
                | "L" ->
                    let x = float splt.[currentIndex + 1]
                    let y = float splt.[currentIndex + 2]
                    segments.Add(L(Coordinate2D(x, y)))
                    loop (currentIndex + 3)
                | "Q" ->
                    let x1 = float splt.[currentIndex + 1]
                    let y1 = float splt.[currentIndex + 2]
                    let x = float splt.[currentIndex + 3]
                    let y = float splt.[currentIndex + 4]
                    segments.Add(Q(Coordinate2D(x1, y1), Coordinate2D(x, y)))
                    loop (currentIndex + 5)
                | "C" ->
                    let x1 = float splt.[currentIndex + 1]
                    let y1 = float splt.[currentIndex + 2]
                    let x2 = float splt.[currentIndex + 3]
                    let y2 = float splt.[currentIndex + 4]
                    let x = float splt.[currentIndex + 5]
                    let y = float splt.[currentIndex + 6]
                    segments.Add(C(Coordinate2D(x1, y1), Coordinate2D(x2, y2), Coordinate2D(x, y)))
                    loop (currentIndex + 7)
                | "Z" ->
                    segments.Add(Z)
                    loop (currentIndex + 1)
                | _ ->
                    failwithf "Unknown path command: %s" currentSegment
                
        do loop 0 

        member this.Segments : SVGPathSegment array = segments |> Array.ofSeq
        member this.Left = left
        member this.Right = right
        member this.Top = top
        member this.Bottom = bottom

        static member toString (path: SVGPath) =
            path.Segments |> Array.map SVGPathSegment.toString |> String.concat " "

        static member normalize (path: SVGPath) =
            let mutable maxx = -System.Double.MaxValue
            let mutable maxy = -System.Double.MaxValue
            let mutable minx = System.Double.MaxValue
            let mutable miny = System.Double.MaxValue

            for segment in path.Segments do
                if segment <> Z then
                    let mx, my = SVGPathSegment.getMaxCoords segment
                    let mnx, mny = SVGPathSegment.getMinCoords segment
                    if mx > maxx then maxx <- mx
                    if my > maxy then maxy <- my
                    if mnx < minx then minx <- mnx
                    if mny < miny then miny <- mny

            let normalizedSegments = 
                path.Segments |> Array.map (SVGPathSegment.normalizeBy minx maxx miny maxy)
            
            SVGPath(
                normalizedSegments |> Array.map SVGPathSegment.toString |> String.concat " ",
                Left = path.Left,
                Right = path.Right,
                Top = path.Top,
                Bottom = path.Bottom
            )

        static member invert (direction: InvertDirection) (path: SVGPath) =
            let invertedSegments = 
                path.Segments |> Array.map (SVGPathSegment.invert direction)
            SVGPath(
                invertedSegments |> Array.map SVGPathSegment.toString |> String.concat " ",
                Left = path.Left,
                Right = path.Right,
                Top = path.Top,
                Bottom = path.Bottom
            )

        static member reposition (left:float) (right:float) (top:float) (bottom:float) (path: SVGPath) =
            let repositionedSegments = 
                path.Segments 
                |> Array.map (SVGPathSegment.reposition path.Left path.Right path.Top path.Bottom left right top bottom)
            SVGPath(
                repositionedSegments |> Array.map SVGPathSegment.toString |> String.concat " ",
                Left = left,
                Right = right,
                Top = top,
                Bottom = bottom
            )
