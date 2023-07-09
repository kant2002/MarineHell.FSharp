open System.Collections.Generic
open BWAPI.NET
open BWEM.NET
open Nito.Collections

type Strategy =
| WaitFor50
| AttackAtAllCost

let CalculateSides (wp : Deque<WalkPosition>) =
    let mutable p1 = wp.[0]
    let mutable p2 = wp.[1]
    let mutable d_max = -1.0
    for i = 0 to wp.Count - 1 do
        for j = 0 to wp.Count - 1 do
            let d = wp[i].GetDistance(wp[j])
            if (d > d_max) then
                    d_max <- d
                    p1 <- wp[i]
                    p2 <- wp[j]

    (p1.ToPosition(), p2.ToPosition())

let GetNearestChokepointCenter (position: Position) (chokePointsCenters: Position list) = 
    if chokePointsCenters.Length > 0 then
        chokePointsCenters |> Seq.minBy (fun x -> x.GetDistance(position))
    else
        position

let emptyTrainingQueue (unit: Unit) =
    unit.GetTrainingQueue().Count = 0

let GetShortestPath (map: Map) (start : TilePosition) (_end: TilePosition) =
    let shortestPath = new List<TilePosition>()
        
    let _length = ref 0
    let mutable curr: ChokePoint = null
    for next in map.GetPath(start.ToPosition(), _end.ToPosition(), _length) do
        if not (isNull curr) then
            let t0 = curr.Center.ToTilePosition()
            let t1 = next.Center.ToTilePosition()
            //trace a ray
            let mutable dx = abs(t1.x - t0.x)
            let mutable dy = abs(t1.y - t0.y)
            let mutable x = t0.x
            let mutable y = t0.y
            let n = 1 + dx + dy
            let x_inc = if (t1.x > t0.x) then 1 else -1
            let y_inc = if (t1.x > t0.x) then 1 else -1
            let mutable error = dx - dy

            dx <- dx * 2
            dy <- dy * 2

            for i = 0 to n-1 do
                shortestPath.Add(new TilePosition(x, y))
                if (error > 0) then
                    x <- x + x_inc
                    error <- error - dy
                else
                    y <- y + y_inc
                    error <- error - dx

        curr <- next

    shortestPath

let GetNearestBaseLocation (bases: Base seq) tilePosition =
    bases |> Seq.minBy (fun x -> x.Location.GetDistance(tilePosition))
    
let GetStartLocation (bases: Base seq) (player : Player) =
    let startLocation = player.GetStartLocation()
    GetNearestBaseLocation bases startLocation
    
    
let invalidateUnitIfDead (unit:Unit) =
    if unit <> null && not (unit.Exists()) then
        null
    else
        unit

type MarineHell() =
    inherit DefaultBWListener()
    [<DefaultValue>] val mutable private _bwClient : BWClient

    let mutable _frameskip = 0
    let mutable _cyclesForSearching = 0
    let mutable _maxCyclesForSearching = 0
    let mutable _searchingScv = 0
    let mutable _searchingTimeout = 0
    let mutable _timeout = 0
    [<DefaultValue>] val mutable private _bunkerBuilder : Unit
    [<DefaultValue>] val mutable private _searcher : Unit
    let mutable _debugText = ""
    let _enemyBuildingMemory = HashSet<Position>()
    let mutable _selectedStrategy = WaitFor50
    [<DefaultValue>] val mutable private _chokePointsCenters : Position list
    [<DefaultValue>] val mutable private map : Map

    member private this.GetBuildTile (game: Game) (builder:Unit) (buildingType:UnitType) (aroundTile:TilePosition) : TilePosition =
        let mutable ret = TilePosition.Invalid
        let mutable maxDist = 3
        let mutable stopDist = 40

        // Refinery, Assimilator, Extractor
        let candidate = match UnitTypeHelper.IsRefinery buildingType with
                        | true -> game.Neutral().GetUnits() |> Seq.tryPick (fun n ->
                                _cyclesForSearching <- _cyclesForSearching + 1
                                if ((n.GetUnitType() = UnitType.Resource_Vespene_Geyser) &&
                                    (abs(n.GetTilePosition().X - aroundTile.X) < stopDist) &&
                                    (abs(n.GetTilePosition().Y - aroundTile.Y) < stopDist)) then
                                    Some(n.GetTilePosition())
                                else
                                    None
                            )
                        | false -> None

        match candidate with
        | Some(candidate) -> candidate
        | None ->
            while ((maxDist < stopDist) && (ret = TilePosition.Invalid)) do
                for i = aroundTile.X - maxDist to aroundTile.X + maxDist do
                    for j = aroundTile.Y - maxDist to aroundTile.Y + maxDist do
                        if (game.CanBuildHere(TilePosition(i, j), buildingType, builder, false)) then
                            _cyclesForSearching <- _cyclesForSearching + game.GetAllUnits().Count
                            let unitsInWay = game.GetAllUnits() |> Seq.exists (fun u -> u.GetID() <> builder.GetID() && (abs(u.GetTilePosition().X - i) < 4) && (abs(u.GetTilePosition().Y - j) < 4))
                            if (not unitsInWay) then
                                _cyclesForSearching <- _cyclesForSearching + 1
                                ret <- TilePosition(i, j)

                maxDist <- maxDist + 2

            if (ret = TilePosition.Invalid) then
                game.Printf($"Unable to find suitable build position for {buildingType}")

            ret

    member this.Run() =
        this._bwClient <- new BWClient(this)
        this._bwClient.StartGame()

    override this.OnStart() =
        let game = this._bwClient.Game

        _frameskip <- 0
        _cyclesForSearching <- 0
        _maxCyclesForSearching <- 0
        _searchingScv <- 0
        _searchingTimeout <- 0
        _timeout <- 0
        this._bunkerBuilder <- null
        this._searcher <- null

        game.SetLocalSpeed(0)

        let map = new Map(game)
        map.Initialize()
        this.map <- map

        this._chokePointsCenters <- map.ChokePoints |> Seq.map (fun c -> 
            let (left, right) = CalculateSides(new Deque<WalkPosition>(c.Geometry))
            let center = (left + right) / 2
            center) |> Seq.toList

    override this.OnFrame() =
        let game = this._bwClient.Game
        let self = game.Self()
        let map = this.map
        let bases = map.Bases

        game.DrawTextScreen(10, 10, $"Playing as {self.GetName()} - {self.GetRace()}")
        game.DrawTextScreen(10, 20, $"Units: {self.GetUnits().Count}; Enemies: {_enemyBuildingMemory.Count}")
        game.DrawTextScreen(10, 30, $"Cycles for buildings: {_cyclesForSearching}; Max cycles: {_maxCyclesForSearching}")
        game.DrawTextScreen(10, 40, $"Elapsed time: {game.ElapsedTime()}; Strategy: {_selectedStrategy}")
        game.DrawTextScreen(10, 50, _debugText)
        game.DrawTextScreen(10, 60, $"supply: {self.SupplyTotal()} used: {self.SupplyUsed()}")

        (*
        * if (_game.elapsedTime() > 2001) { int x = (_game.elapsedTime() / 500) %
        * 2; if (x == 0) { selectedStrategy = Strategy.FindEnemy; } else {
        * selectedStrategy = Strategy.HugeAttack; } }
        *)
        let canBuild = _maxCyclesForSearching <= 300000

        game.SetLocalSpeed(0)

        if (_maxCyclesForSearching < _cyclesForSearching) then
            _maxCyclesForSearching <- _cyclesForSearching

        _cyclesForSearching <- 0

        let workers = List<Unit>()
        let barracks = List<Unit>()
        let marines = List<Unit>()
        let baseLocations = List<Base>()
        let allLocations = List<Base>()
        let mutable workerAttacked = Position.Invalid

        let mutable commandCenter : Unit option = None
        let mutable bunker : Unit option = None

        this._bunkerBuilder <- invalidateUnitIfDead this._bunkerBuilder
        this._searcher <- invalidateUnitIfDead this._searcher

        if not (isNull this._bunkerBuilder) then
            game.DrawTextMap(this._searcher.GetPosition(), "Mr. Searcher")

        // iterate through my units
        self.GetUnits() |> Seq.iter (fun myUnit ->
            if myUnit.GetUnitType().IsWorker() then
                workers.Add(myUnit)

            // if there's enough minerals, train an SCV
            if (myUnit.GetUnitType() = UnitType.Terran_Command_Center) then
                commandCenter <- Some myUnit

            if myUnit.GetUnitType() = UnitType.Terran_Barracks && not(myUnit.IsBeingConstructed()) then
                barracks.Add(myUnit)
            
            if (myUnit.GetUnitType() = UnitType.Terran_Marine) then
                marines.Add(myUnit)

            if (myUnit.GetUnitType() = UnitType.Terran_Bunker && not(myUnit.IsBeingConstructed())) then
                bunker <- Some myUnit

            if (myUnit.IsUnderAttack() && myUnit.CanAttack()) then
                game.SetLocalSpeed(1)
                myUnit.Attack(myUnit.GetPosition()) |> ignore
        )

        let idle_worker (myUnit : Unit) = 
            myUnit.GetUnitType().IsWorker() && myUnit.IsIdle()

        let mineralFieldUnits = game.Neutral().GetUnits() |> Seq.filter (fun neutralUnit -> neutralUnit.GetUnitType().IsMineralField())
        workers
            |> Seq.filter idle_worker // if it's a worker and it's idle, 
            |> Seq.iter (fun myUnit -> // send it to the closest mineral patch
                let planToBuildBarrack = match (bunker) with
                                            | None -> not (isNull this._bunkerBuilder) && myUnit.Equals(this._bunkerBuilder) && barracks.Count > 0
                                            | Some(_) -> false

                // find the closest mineral
                let find_closest_mineral (closestUnitOption: Unit option) (neutralUnit: Unit) =
                    match closestUnitOption with
                    | None -> Some(neutralUnit)
                    | Some(closestUnit) -> if myUnit.GetDistance(neutralUnit) < myUnit.GetDistance(closestUnit) then Some(neutralUnit) else closestUnitOption
                let closestMineral : Unit option = mineralFieldUnits |> Seq.fold find_closest_mineral None

                // if a mineral patch was found, send the worker to gather it
                if not planToBuildBarrack then
                    match (closestMineral) with
                    | Some(closestMineral) -> myUnit.Gather(closestMineral, false) |> ignore
                    | None -> ()

                if (myUnit.IsUnderAttack() && myUnit.CanAttack()) then
                    game.SetLocalSpeed(1)
                    myUnit.Attack(myUnit.GetPosition()) |> ignore

                if (myUnit.IsUnderAttack() && myUnit.IsGatheringMinerals()) then
                    workerAttacked <- myUnit.GetPosition()
        )

        if (this._bunkerBuilder = null && workers.Count > 10) then 
            this._bunkerBuilder <- workers[10]

        if (bunker.IsNone && barracks.Count >= 1 && workers.Count > 10 && canBuild) then
            game.SetLocalSpeed(20)
            if (_timeout < 200) then
                game.DrawTextMap(this._bunkerBuilder.GetPosition(), $"Moving to create bunker {_timeout}/400")
                let bunkerLocation = GetNearestChokepointCenter (this._bunkerBuilder.GetPosition()) this._chokePointsCenters
                this._bunkerBuilder.Move bunkerLocation |> ignore
                _timeout <- _timeout + 1
            else
                game.DrawTextMap(this._bunkerBuilder.GetPosition(), "Buiding bunker")
                let curretBunkerBuilderPosition = this._bunkerBuilder.GetTilePosition()
                let buildTile = this.GetBuildTile game this._bunkerBuilder UnitType.Terran_Barracks curretBunkerBuilderPosition
                if buildTile <> TilePosition.Invalid then
                    this._bunkerBuilder.Build(UnitType.Terran_Bunker, buildTile) |> ignore
        elif (workers.Count > 10) then
            game.SetLocalSpeed(10)
            game.DrawTextMap(workers[10].GetPosition(), "He will build bunker")

        match bunker with
        | Some(bunker) when not (isNull this._bunkerBuilder) && this._bunkerBuilder.IsRepairing() = false ->
            game.DrawTextMap(this._bunkerBuilder.GetPosition(), "Reparing bunker")
            this._bunkerBuilder.Repair(bunker) |> ignore
        | _ -> ()

        match commandCenter with
        | Some(commandCenter) when commandCenter.GetTrainingQueue().Count = 0 && workers.Count < 20 && self.Minerals() >= 50 ->
            commandCenter.Build(UnitType.Terran_SCV) |> ignore
        | _ -> ()

        _frameskip <- (_frameskip + 1) % 20
        if (_frameskip = 0) then
            _searchingTimeout <- _searchingTimeout + 1

            if canBuild then
                workers
                    |> Seq.filter (fun worker -> worker.IsGatheringMinerals())
                    |> Seq.iteri (fun i worker ->
                        let barrackBuildSatisfied =
                            self.Minerals() >= 150 * i 
                                && barracks.Count < 6
                        let supplyDepotSatisfied =
                            self.Minerals() >= i * 100 
                                && self.SupplyUsed() + (self.SupplyUsed() / 3) >= self.SupplyTotal()
                                && self.SupplyTotal() < 400

                        let targetBuilding = match (supplyDepotSatisfied, barrackBuildSatisfied) with
                                             | (true, true) -> UnitType.Terran_Supply_Depot
                                             | (true, false) -> UnitType.Terran_Supply_Depot
                                             | (false, true) -> UnitType.Terran_Barracks
                                             | (false, false) -> UnitType.None
                        if targetBuilding <> UnitType.None then
                            let buildTile = this.GetBuildTile game worker targetBuilding (self.GetStartLocation())
                            // and, if found, send the worker to build it (and leave others alone - break;)
                            if (buildTile <> TilePosition.Invalid) then
                                worker.Build(targetBuilding, buildTile) |> ignore
                )

            barracks
                |> Seq.filter emptyTrainingQueue
                |> Seq.iter (fun barrack ->
                    barrack.Build(UnitType.Terran_Marine) |> ignore
            )

            for b in bases do
                // If this is a possible start location,
                if (b.Starting) then baseLocations.Add(b)
                allLocations.Add(b)

            if (marines.Count > 50 || _selectedStrategy = Strategy.AttackAtAllCost) then
                _selectedStrategy <- if (marines.Count > 40) then AttackAtAllCost else WaitFor50
            
            marines |> Seq.iteri (fun k marine ->
                if not(marine.IsAttacking()) && not(marine.IsMoving()) then
                    if (_selectedStrategy = Strategy.AttackAtAllCost) then
                        if (_enemyBuildingMemory.Count = 0) then
                            if allLocations.Count > 0 then
                                marine.Attack(allLocations[k % allLocations.Count].Center) |> ignore
                        else
                            _enemyBuildingMemory |> Seq.iter (fun p -> marine.Attack(p) |> ignore)

                        if (marines.Count > 70) then
                            if (k < allLocations.Count) then
                                marine.Attack(allLocations[k].Center) |> ignore
                    else
                        let marinePosition = marine.GetPosition()
                        let findClosestChokepoint (bunker : Unit option) =
                            match bunker with
                            | Some(bunker) ->
                                match GetShortestPath map (bunker.GetTilePosition()) ((GetStartLocation bases  self).Location) with
                                | path when path.Count > 1 -> path[1].ToPosition()
                                | _ -> GetNearestChokepointCenter marinePosition this._chokePointsCenters
                            | None -> GetNearestChokepointCenter marinePosition this._chokePointsCenters

                        let newPos = findClosestChokepoint bunker
                        marine.Attack(newPos) |> ignore

                    match bunker with
                    | Some(bunker) when bunker.GetLoadedUnits().Count < 4 && k < 4 -> marine.Load(bunker) |> ignore
                    | _ -> ()

                    if (workerAttacked <> Position.Invalid) then
                        marine.Attack(workerAttacked) |> ignore
            )

            if (workers.Count > 7 && this._searcher = null) then
                this._searcher <- workers[7]

            if (not (isNull this._searcher) && this._searcher.IsGatheringMinerals() && _searchingScv < baseLocations.Count && _searchingTimeout % 10 = 0) then
                this._searcher.Move(baseLocations[_searchingScv].Center) |> ignore
                _searchingScv <- _searchingScv + 1

            if workers.Count <> 0 then
                let lastWorker = if workers.Count > 7 then workers[7] else workers[workers.Count - 1]
                _debugText <- $"Size: {workers.Count}; isGathering{lastWorker.IsGatheringMinerals()}; location: {baseLocations.Count}; num: {_searchingScv}"

            let enemyUnits = game.Enemy().GetUnits()
            let enemyBuildings = enemyUnits |> Seq.filter (fun u -> u.GetUnitType().IsBuilding())
            for u in enemyBuildings do
                // check if we have it's position in memory and add it if we don't
                _enemyBuildingMemory.Add(u.GetPosition()) |> ignore

            // loop over all the positions that we remember
            _enemyBuildingMemory |> Seq.exists (fun savedPosition ->
                // compute the TilePosition corresponding to our remembered Position p
                let savedTilePosition = new TilePosition(savedPosition.X / 32, savedPosition.Y / 32)
                // if that tile is currently visible to us...
                if (game.IsVisible(savedTilePosition)) then
                    // loop over all the visible enemy buildings and find out if at
                    // least one of them is still at that remembered position
                    let buildingStillThere = enemyBuildings |> Seq.exists (fun u -> u.GetPosition() = savedPosition)
                    // if there is no more any building, remove that position from our memory
                    if (not buildingStillThere) then
                        _enemyBuildingMemory.Remove(savedPosition) |> ignore
                        true
                    else
                        false
                else
                    false
            ) |> ignore
        
printfn "MarineHell in F#"
let bot = new MarineHell()
bot.Run()