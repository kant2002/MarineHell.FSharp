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
                    d_max <- d;
                    p1 <- wp[i];
                    p2 <- wp[j];

    (p1.ToPosition(), p2.ToPosition())

let GetNearestChokepointCenter (position: Position) (chokePointsCenters: Position list) = 
    if chokePointsCenters.Length > 0 then
        chokePointsCenters |> Seq.minBy (fun x -> x.GetDistance(position))
    else
        position

let emptyTrainingQueue (unit: Unit) =
    unit.GetTrainingQueue().Count = 0

type MarineHell() =
    inherit DefaultBWListener()
    [<DefaultValue>] val mutable private _bwClient : BWClient
    [<DefaultValue>] val mutable private _game : Game
    [<DefaultValue>] val mutable private _self : Player
    let mutable _frameskip = 0
    let mutable _cyclesForSearching = 0
    let mutable _maxCyclesForSearching = 0
    let mutable _searchingScv = 0
    let mutable _searchingTimeout = 0
    let mutable _timeout = 0
    [<DefaultValue>] val mutable private _bunkerBuilder : Unit
    [<DefaultValue>] val mutable private _searcher : Unit
    let mutable _debugText = ""
    let _enemyBuildingMemory = new HashSet<Position>()
    let mutable _selectedStrategy = WaitFor50
    [<DefaultValue>] val mutable private _chokePointsCenters : Position list
    [<DefaultValue>] static val mutable private map : Map


    member this.GetNearestChokepointCenter position = 
        GetNearestChokepointCenter position this._chokePointsCenters

    static member GetNearestBaseLocation tilePosition =
        MarineHell.map.Bases |> Seq.minBy (fun x -> x.Location.GetDistance(tilePosition))

    static member GetStartLocation (player : Player) =
        MarineHell.GetNearestBaseLocation(player.GetStartLocation())

    static member GetShortestPath (start : TilePosition, _end: TilePosition) =
        let shortestPath = new List<TilePosition>()
        
        let _length = ref 0
        let mutable curr: ChokePoint = null
        for next in MarineHell.map.GetPath(start.ToPosition(), _end.ToPosition(), _length) do
            if curr <> null then
                let t0 = curr.Center.ToTilePosition()
                let t1 = next.Center.ToTilePosition()
                //trace a ray
                let mutable dx = abs(t1.x - t0.x);
                let mutable dy = abs(t1.y - t0.y);
                let mutable x = t0.x;
                let mutable y = t0.y;
                let n = 1 + dx + dy;
                let x_inc = if (t1.x > t0.x) then 1 else -1
                let y_inc = if (t1.x > t0.x) then 1 else -1;
                let mutable error = dx - dy;

                dx <- dx * 2;
                dy <- dy * 2;

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

    member private this.GetBuildTile (builder:Unit, buildingType:UnitType, aroundTile:TilePosition) : TilePosition =
        let mutable ret = TilePosition.Invalid
        let mutable maxDist = 3
        let mutable stopDist = 40

        // Refinery, Assimilator, Extractor
        let candidate = match UnitTypeHelper.IsRefinery buildingType with
                        | true -> this._game.Neutral().GetUnits() |> Seq.tryPick (fun n ->
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
                        if (this._game.CanBuildHere(new TilePosition(i, j), buildingType, builder, false)) then
                            _cyclesForSearching <- _cyclesForSearching + this._game.GetAllUnits().Count
                            let unitsInWay = this._game.GetAllUnits() |> Seq.exists (fun u -> u.GetID() <> builder.GetID() && (abs(u.GetTilePosition().X - i) < 4) && (abs(u.GetTilePosition().Y - j) < 4))
                            if (not  unitsInWay) then
                                _cyclesForSearching <- _cyclesForSearching + 1
                                ret <- new TilePosition(i, j)

                maxDist <- maxDist + 2

            if (ret = TilePosition.Invalid) then
                this._game.Printf($"Unable to find suitable build position for {buildingType}");

            ret

    member this.Run() =
        this._bwClient <- new BWClient(this);
        this._bwClient.StartGame();

    override this.OnStart() =
        this._game <- this._bwClient.Game;
        _frameskip <- 0
        _cyclesForSearching <- 0
        _maxCyclesForSearching <- 0
        _searchingScv <- 0
        _searchingTimeout <- 0
        _timeout <- 0
        this._bunkerBuilder <- null
        this._searcher <- null

        this._self <- this._game.Self()
        this._game.SetLocalSpeed(0)

        let map = new Map(this._game)
        map.Initialize()
        MarineHell.map <- map

        this._chokePointsCenters <- map.ChokePoints |> Seq.map (fun c -> 
            let (left, right) = CalculateSides(new Deque<WalkPosition>(c.Geometry));
            let center = (left + right) / 2;
            center) |> Seq.toList

    override this.OnFrame() =
        this._game.DrawTextScreen(10, 10, $"Playing as {this._self.GetName()} - {this._self.GetRace()}")
        this._game.DrawTextScreen(10, 20, $"Units: {this._self.GetUnits().Count}; Enemies: {_enemyBuildingMemory.Count}");
        this._game.DrawTextScreen(10, 30, $"Cycles for buildings: {_cyclesForSearching}; Max cycles: {_maxCyclesForSearching}");
        this._game.DrawTextScreen(10, 40, $"Elapsed time: {this._game.ElapsedTime()}; Strategy: {_selectedStrategy}");
        this._game.DrawTextScreen(10, 50, _debugText);
        this._game.DrawTextScreen(10, 60, $"supply: {this._self.SupplyTotal()} used: {this._self.SupplyUsed()}");

        (*
        * if (_game.elapsedTime() > 2001) { int x = (_game.elapsedTime() / 500) %
        * 2; if (x == 0) { selectedStrategy = Strategy.FindEnemy; } else {
        * selectedStrategy = Strategy.HugeAttack; } }
        *)
        let canBuild = _maxCyclesForSearching <= 300000

        this._game.SetLocalSpeed(0)

        if (_maxCyclesForSearching < _cyclesForSearching) then
            _maxCyclesForSearching <- _cyclesForSearching

        _cyclesForSearching <- 0

        let workers = new List<Unit>()
        let barracks = new List<Unit>()
        let marines = new List<Unit>()
        let baseLocations = new List<Base>()
        let allLocations = new List<Base>()
        let mutable workerAttacked = Position.Invalid

        let mutable commandCenter : Unit option = None
        let mutable bunker : Unit option = None

        if this._bunkerBuilder <> null && not(this._bunkerBuilder.Exists()) then
            this._bunkerBuilder <- null

        if this._searcher <> null && not(this._searcher.Exists()) then
            this._searcher <- null

        if (this._searcher <> null) then
            this._game.DrawTextMap(this._searcher.GetPosition(), "Mr. Searcher")

        // iterate through my units
        this._self.GetUnits() |> Seq.iter (fun myUnit ->
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
                this._game.SetLocalSpeed(1)
                myUnit.Attack(myUnit.GetPosition()) |> ignore
        )

        let idle_worker (myUnit : Unit) = 
            myUnit.GetUnitType().IsWorker() && myUnit.IsIdle()

        let mineralFieldUnits = this._game.Neutral().GetUnits() |> Seq.filter (fun neutralUnit -> neutralUnit.GetUnitType().IsMineralField())
        workers
            |> Seq.filter idle_worker // if it's a worker and it's idle, 
            |> Seq.iter (fun myUnit -> // send it to the closest mineral patch
                let planToBuildBarrack = match (bunker) with
                                            | None -> this._bunkerBuilder <> null && myUnit.Equals(this._bunkerBuilder) && barracks.Count > 0
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
                    this._game.SetLocalSpeed(1)
                    myUnit.Attack(myUnit.GetPosition()) |> ignore

                if (myUnit.IsUnderAttack() && myUnit.IsGatheringMinerals()) then
                    workerAttacked <- myUnit.GetPosition()
        )

        if (this._bunkerBuilder = null && workers.Count > 10) then 
            this._bunkerBuilder <- workers[10]

        if (bunker.IsNone && barracks.Count >= 1 && workers.Count > 10 && canBuild) then
            this._game.SetLocalSpeed(20)
            if (_timeout < 200) then
                this._game.DrawTextMap(this._bunkerBuilder.GetPosition(), $"Moving to create bunker {_timeout}/400")
                this._bunkerBuilder.Move(this.GetNearestChokepointCenter(this._bunkerBuilder.GetPosition())) |> ignore
                _timeout <- _timeout + 1
            else
                this._game.DrawTextMap(this._bunkerBuilder.GetPosition(), "Buiding bunker")
                let buildTile = this.GetBuildTile(this._bunkerBuilder, UnitType.Terran_Barracks, this._bunkerBuilder.GetTilePosition())
                if buildTile <> TilePosition.Invalid then
                    this._bunkerBuilder.Build(UnitType.Terran_Bunker, buildTile) |> ignore
        elif (workers.Count > 10) then
            this._game.SetLocalSpeed(10)
            this._game.DrawTextMap(workers[10].GetPosition(), "He will build bunker")

        if (bunker.IsSome && this._bunkerBuilder <> null && this._bunkerBuilder.IsRepairing() = false) then
            this._game.DrawTextMap(this._bunkerBuilder.GetPosition(), "Reparing bunker")
            this._bunkerBuilder.Repair(bunker.Value) |> ignore

        if (commandCenter.IsSome && commandCenter.Value.GetTrainingQueue().Count = 0 && workers.Count < 20 && this._self.Minerals() >= 50) then
            commandCenter.Value.Build(UnitType.Terran_SCV) |> ignore

        _frameskip <- (_frameskip + 1) % 20
        if (_frameskip = 0) then
            _searchingTimeout <- _searchingTimeout + 1

            if canBuild then
                workers
                    |> Seq.filter (fun worker -> worker.IsGatheringMinerals())
                    |> Seq.iteri (fun i worker ->
                        let barrackBuildSatisfied =
                            this._self.Minerals() >= 150 * i 
                                && barracks.Count < 6
                        let supplyDepotSatisfied =
                            this._self.Minerals() >= i * 100 
                                && this._self.SupplyUsed() + (this._self.SupplyUsed() / 3) >= this._self.SupplyTotal()
                                && this._self.SupplyTotal() < 400

                        let targetBuilding = match (supplyDepotSatisfied, barrackBuildSatisfied) with
                                             | (true, true) -> UnitType.Terran_Supply_Depot
                                             | (true, false) -> UnitType.Terran_Supply_Depot
                                             | (false, true) -> UnitType.Terran_Barracks
                                             | (false, false) -> UnitType.None
                        if targetBuilding <> UnitType.None then
                            let buildTile = this.GetBuildTile(worker, targetBuilding, this._self.GetStartLocation())
                            // and, if found, send the worker to build it (and leave others alone - break;)
                            if (buildTile <> TilePosition.Invalid) then
                                worker.Build(targetBuilding, buildTile) |> ignore
                )

            barracks
                |> Seq.filter emptyTrainingQueue
                |> Seq.iter (fun barrack ->
                    barrack.Build(UnitType.Terran_Marine) |> ignore
            )

            for b in MarineHell.map.Bases do
                // If this is a possible start location,
                if (b.Starting) then baseLocations.Add(b)
                allLocations.Add(b)

            marines |> Seq.iteri (fun k marine ->
                if not(marine.IsAttacking()) && not(marine.IsMoving()) then
                    if (marines.Count > 50 || _selectedStrategy = Strategy.AttackAtAllCost) then
                        _selectedStrategy <- if (marines.Count > 40) then AttackAtAllCost else WaitFor50

                        if (_enemyBuildingMemory.Count = 0) then
                            if allLocations.Count > 0 then
                                marine.Attack(allLocations[k % allLocations.Count].Center) |> ignore
                        else
                            _enemyBuildingMemory |> Seq.iter (fun p -> marine.Attack(p) |> ignore)

                        if (marines.Count > 70) then
                            if (k < allLocations.Count) then
                                marine.Attack(allLocations[k].Center) |> ignore
                    else
                        let find_closest_chokepoint (bunker : Unit option) =
                            if (bunker.IsSome) then
                                let path = MarineHell.GetShortestPath(bunker.Value.GetTilePosition(), MarineHell.GetStartLocation(this._game.Self()).Location)
                                if (path.Count > 1) then
                                    path[1].ToPosition()
                                else
                                    this.GetNearestChokepointCenter(marine.GetPosition())
                            else
                                this.GetNearestChokepointCenter(marine.GetPosition())

                        let newPos = find_closest_chokepoint bunker
                        marine.Attack(newPos) |> ignore

                    if (bunker.IsSome && bunker.Value.GetLoadedUnits().Count < 4 && k < 4) then
                        marine.Load(bunker.Value) |> ignore

                    if (workerAttacked <> Position.Invalid) then
                        marine.Attack(workerAttacked) |> ignore

                if (workers.Count > 7 && this._searcher = null) then
                    this._searcher <- workers[7]

                if (this._searcher <> null && this._searcher.IsGatheringMinerals() && _searchingScv < baseLocations.Count && _searchingTimeout % 10 = 0) then
                    this._searcher.Move(baseLocations[_searchingScv].Center) |> ignore
                    _searchingScv <- _searchingScv + 1

                if workers.Count <> 0 then
                    let lastWorker = if workers.Count > 7 then workers[7] else workers[workers.Count - 1]
                    _debugText <- $"Size: {workers.Count}; isGathering{lastWorker.IsGatheringMinerals()}; location: {baseLocations.Count}; num: {_searchingScv}";

                let enemyUnits = this._game.Enemy().GetUnits()
                for u in enemyUnits do
                    // if this unit is in fact a building
                    if (u.GetUnitType().IsBuilding()) then
                        // check if we have it's position in memory and add it if we don't
                        _enemyBuildingMemory.Add(u.GetPosition()) |> ignore

                // loop over all the positions that we remember
                _enemyBuildingMemory |> Seq.exists (fun p ->
                    // compute the TilePosition corresponding to our remembered Position p
                    let tileCorrespondingToP = new TilePosition(p.X / 32, p.Y / 32)
                    // if that tile is currently visible to us...
                    if (this._game.IsVisible(tileCorrespondingToP)) then
                        // loop over all the visible enemy buildings and find out if at
                        // least one of them is still at that remembered position
                        //let buildingStillThere = false
                        let buildingStillThere = enemyUnits |> Seq.exists (fun u -> u.GetUnitType().IsBuilding() && (u.GetPosition() = p))
                        // if there is no more any building, remove that position from our memory
                        if (not buildingStillThere) then
                            _enemyBuildingMemory.Remove(p) |> ignore
                            true
                        else
                            false
                    else
                        false
                ) |> ignore
            )
        
printfn "MarineHell in F#"
let bot = new MarineHell();
bot.Run();