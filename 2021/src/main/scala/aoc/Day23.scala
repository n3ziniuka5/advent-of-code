package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day23:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day23.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    val roomLetters = Map(
      2 -> 'A',
      4 -> 'B',
      6 -> 'C',
      8 -> 'D'
    )
    val roomLettersReversed = roomLetters.map { case (k, v) => v -> k }

    val energyCost = Map(
      'A' -> 1,
      'B' -> 10,
      'C' -> 100,
      'D' -> 1000
    )

    def extendInputForPart2(lines: List[String]): List[String] =
        lines.take(3) ++
            List(
              "  #D#C#B#A#",
              "  #D#B#A#C#"
            ) ++
            lines.drop(3)

    def readInput(lines: List[String]): Map[Int, List[Char]] =
        lines
            .drop(2)
            .dropRight(1)
            .map { line =>
                line
                    .drop(1)
                    .dropRight(1)
                    .zipWithIndex
                    .filter { case (l, _) => l != '#' && l != ' ' }
                    .map(a => a._2 -> List(a._1))
                    .toMap
            }
            .foldLeft(Map.empty[Int, List[Char]]) { case (acc, map) =>
                map.map { case (room, list) =>
                    val newList = acc.getOrElse(room, List.empty) ++ list
                    room -> newList
                }
            }

    case class State(rooms: Map[Int, List[Char]], hallway: Map[Int, Char])

    def energyCost(dist: Int, char: Char): Int =
        dist * energyCost(char)

    def possibleStates(state: State, energySpent: Int, roomSize: Int): Iterable[(State, Int)] =
        val moveIntoRooms = state.hallway.flatMap { case (hallwayId, char) =>
            val roomId       = roomLettersReversed(char)
            val roomHasSpace = state.rooms(roomId).isEmpty || state.rooms(roomId).forall(_ == char)
            val pathNotBlocked =
                val step = if hallwayId < roomId then 1 else -1
                ((hallwayId + step) until roomId by step).forall(i => !state.hallway.contains(i))

            if roomHasSpace && pathNotBlocked then
                val currentRoom = state.rooms.getOrElse(roomId, List.empty)
                Some(
                  (
                    state.copy(
                      hallway = state.hallway - hallwayId,
                      rooms = state.rooms + (roomId -> (char +: currentRoom))
                    ),
                    energySpent + energyCost(Math.abs(hallwayId - roomId) + (roomSize - currentRoom.size), char)
                  )
                )
            else None
        }

        val moveOutOfRooms = state.rooms
            .filter { case (roomId, room) =>
                room.nonEmpty && !room.forall(_ == roomLetters(roomId))
            }
            .flatMap { case (roomId, room) =>
                val hallwayToLeft  = roomId to 0 by -1
                val hallwayToRight = roomId to 10

                def takeWhileUnblocked(hallwayPath: Range): IndexedSeq[(State, Int)] =
                    hallwayPath
                        .takeWhile(!state.hallway.contains(_))
                        .filterNot(roomLetters.contains)
                        .map { hallwayId =>
                            val currentRoom = state.rooms(roomId)
                            (
                              state.copy(
                                hallway = state.hallway + (hallwayId -> currentRoom.head),
                                rooms = state.rooms + (roomId        -> currentRoom.tail)
                              ),
                              energySpent + energyCost(
                                Math.abs(hallwayId - roomId) + (roomSize + 1 - currentRoom.size),
                                currentRoom.head
                              )
                            )
                        }

                takeWhileUnblocked(hallwayToLeft) ++ takeWhileUnblocked(hallwayToRight)
            }

        moveIntoRooms ++ moveOutOfRooms

    def lowestEnergy(map: Map[Int, List[Char]]): Int =
        val roomSize = map.head._2.size

        @tailrec
        def loop(
            states: mutable.PriorityQueue[(State, Int)],
            visited: Map[State, Int] = Map.empty
        ): Int =
            val (head, energy) = states.dequeue()

            val roomsCorrectlyFilled = head.hallway.isEmpty && head.rooms.forall { case (r, list) =>
                list.forall(_ == roomLetters(r))
            }

            if roomsCorrectlyFilled then energy
            else {
                val newStates = possibleStates(head, energy, roomSize)
                    .filter { (state, energy) =>
                        energy < visited.getOrElse(state, Integer.MAX_VALUE)
                    }

                loop(states.addAll(newStates), visited ++ newStates)
            }

        val initialState = (State(map, Map.empty), 0)

        loop(
          mutable.PriorityQueue.apply(initialState)(Ordering.by[(State, Int), Int](_._2).reverse),
          Map(initialState)
        )

    def part1(lines: List[String]): Int =
        lowestEnergy(readInput(lines))

    def part2(lines: List[String]): Int =
        lowestEnergy(readInput(extendInputForPart2(lines)))
