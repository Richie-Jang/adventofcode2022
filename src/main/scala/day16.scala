object day16 {

    type Id = String
    case class Room(id: Id, flow: Int, tunnels: List[Id])

    type Input = List[Room]
    def parse(xs: Iterator[String]): Input = {
        xs.map { case s"Valve $id has flow rate=$flow; $_ $_ to $_ $tunnelsStr" => 
            val tunnels = tunnelsStr.split(", ").toList
            Room(id, flow.toInt, tunnels)
        }.toList
    }

    case class RoomMap(
        rooms: Map[Id, Room],
        routes: Map[Id, Map[Id, Int]],
        valves: Set[Id]
    )

    def constructMap(input: Input): RoomMap = {
        val rooms = input.map(r => r.id -> r).toMap
        val valves = input.filter(r => r.flow > 0).map(r => r.id).toSet
        val tunnels = rooms.mapValues(_.tunnels).toMap
        val routeSearch = RouteSearch(tunnels)
        val routes = (valves + "AA").iterator.map { id => id -> routeSearch(id) }.toMap
        RoomMap(rooms, routes, valves)
    }

    def bestPath(map: RoomMap, start: Id, valves: Set[Id], timeAllowed: Int): (List[Id], Int) = {
        def recurse(path: List[Id], valvesLeft: Set[Id], timeLeft: Int, totalValue: Int): (List[Id], Int) = {
            valvesLeft
                .flatMap { id => 
                    val current = path.head
                    val distance = map.routes(current)(id)
                    val t = timeLeft - distance - 1
                    Option.when(t > 0) {
                        val value = map.rooms(id).flow * t
                        recurse(id :: path, valvesLeft - id, t, totalValue + value)
                    }
                }
                .maxByOption(_._2)
                .getOrElse{ (path.reverse, totalValue) }
        }
        recurse(start :: Nil, valves, timeAllowed, 0)
    }

    object RouteSearch {
        case class State(frontier: List[(Id, Int)], score: Map[Id, Int]) {
            private def setScore(id: Id, s: Int) = State((id, s+1) :: frontier, score + (id -> s))
            def dequeued(): (Id, State) = {
                var f = frontier.sortBy(_._2)
                (f.head._1, copy(frontier = f.tail))
            }
            def considerEdge(from: Id, to: Id): State = {
                val toScore = score(from) + 1
                if toScore >= score(to) then this else setScore(to, toScore)
            }
        }

        object State {
            def initial(start: Id) = State(List((start, 0)), Map(start -> 0).withDefault(_ => Int.MaxValue))
        }

        def apply(neighbors: Id => List[Id])(start: Id): Map[Id, Int] = {
            var state = State.initial(start)
            while state.frontier.nonEmpty do {
                val (curr, currState) = state.dequeued()
                state = neighbors(curr).foldLeft(currState) {(s,n) => s.considerEdge(curr, n)}
            }
            state.score
        }
    }

    @main def day16Part1(): Unit = {

    }

}