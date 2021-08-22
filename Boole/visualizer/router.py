import json
from collections import defaultdict
from math import sqrt, inf
from pprint import pprint
from random import random, shuffle, choice
from queue import PriorityQueue
from sys import argv
import os

from matplotlib import pyplot as plt, patches

import noise
from noise import snoise2

import matplotlib

# matplotlib.use("TkAgg")

ATTRACTION = 0.5


class Station:
    def __init__(self, id, inputs, to, type, name):
        self.id = id
        self.inputs = inputs
        self.to = to
        self.type = type
        self.x = random() * 2
        self.y = random() * 2
        self.force_x = 0
        self.force_y = 0
        self.port_locs = dict()
        self.name = name

    def sim(self, dt):
        self.x += self.force_x * dt
        self.y += self.force_y * dt

    def dist(self, other):
        if type(other) == Station:
            return sqrt((self.x - other.x) ** 2 + (self.y - other.y) ** 2)
        else:
            return sqrt((self.x - other[0]) ** 2 + (self.y - other[1]) ** 2)

    def __eq__(self, other):
        return self.id == other.id


class StationGroup:
    def __init__(self, stations):
        self.stations = stations

    @staticmethod
    def build(stations):
        # for itt in range(40000):
        for itt in range(10000):

            err = 0
            for movable in stations:
                movable.force_x += random() * 10000 / (itt + 50)
                movable.force_y += random() * 10000 / (itt + 50)
                for other in stations:
                    if movable == other:
                        continue
                    delta = (1 / (movable.x - other.x), 1 / (movable.y - other.y))
                    movable.force_x += delta[0]
                    movable.force_y += delta[1]
                for neighbour in movable.to:
                    other = stations[neighbour[0]]
                    if movable == other:
                        continue
                    delta = ((movable.x - other.x), (movable.y - other.y))
                    normalization = sqrt(delta[0] ** 2 + delta[1] ** 2) / ((len(movable.to) + other.inputs) ** 0.2)
                    err += sqrt(delta[0] ** 2 + delta[1] ** 2)
                    # print(normalization)
                    delta = (delta[0] / normalization, delta[1] / normalization)
                    movable.force_x -= delta[0] * ATTRACTION
                    movable.force_y -= delta[1] * ATTRACTION

                    other.force_x += delta[0] * ATTRACTION
                    other.force_y += delta[1] * ATTRACTION

            # print(err)
            for movable in stations:
                movable.sim(0.01)
                movable.force_x = 0
                movable.force_y = 0

        return StationGroup(stations)

    def scale(self, min_dist):
        dx = inf
        dy = inf
        avg_x = 0
        avg_y = 0
        for i in range(len(self.stations)):
            avg_x += self.stations[i].x
            avg_y += self.stations[i].y
            for j in range(i):
                dx = min(dx, abs(self.stations[i].x - self.stations[j].x) + abs(
                    self.stations[i].y - self.stations[j].y) / 3)
                dy = min(dy, abs(self.stations[i].y - self.stations[j].y) + abs(
                    self.stations[i].x - self.stations[j].x) / 3)

        avg_x, avg_y = avg_x / len(self.stations), avg_y / len(self.stations)

        for station in self.stations:
            station.x = round((station.x - avg_x) * min_dist / dx)
            station.y = round((station.y - avg_y) * min_dist / dy)

    def plot(self):
        fig, ax = plt.subplots()

        xs = []
        ys = []
        ids = []
        for station in self.stations:
            print(station.id, station.x, station.y)
            xs.append(station.x)
            ys.append(station.y)
            ids.append(station.id)

        plt.xlim(min(xs) - 3, max(xs) + 5)
        plt.ylim(min(ys) - 3, max(ys) + 5)
        ax.set_aspect('equal')

        for n, id in enumerate(ids):
            ax.add_patch(patches.Rectangle((xs[n], ys[n]), 2, 2))

        for i, txt in enumerate(ids):
            ax.annotate(txt, (xs[i], ys[i]))
        plt.show()


class World:
    def __init__(self, stations):
        self.stations = stations
        self.roads = []
        self.filled = dict()

        for station in stations:
            self.filled[(station.x, station.y)] = (0, station.id, 0)
            self.filled[(station.x + 1, station.y)] = (0, station.id, 0)
            self.filled[(station.x, station.y + 1)] = (0, station.id, 0)
            self.filled[(station.x + 1, station.y + 1)] = (0, station.id, 0)

    def build(self):
        # Direction 1 -> horizontal
        # Direction 2 -> vertical
        # Direction 3 -> corner

        for station in self.stations:
            for port, goal in enumerate(station.to):
                print(f"Scheduling: {station.id} {port} to {goal}")
                if station.id == goal[0]:
                    continue
                self.roads.append((station.id, port, goal[0], self.a_star(station, goal)))

    def a_star(self, station, goal):

        open_set = PriorityQueue()

        open_set.put((0,(station.x, station.y, 0)))
        open_set.put((0,(station.x + 1, station.y, 0)))
        open_set.put((0,(station.x, station.y + 1, 0)))
        open_set.put((0,(station.x + 1, station.y + 1, 0)))


        extra_set = set()
        extra_set.add((station.x, station.y, 0))
        extra_set.add((station.x + 1, station.y, 0))
        extra_set.add((station.x, station.y + 1, 0))
        extra_set.add((station.x + 1, station.y + 1, 0))

        goal, port = self.stations[goal[0]], goal[1]

        if port not in goal.port_locs:

            goal_set = set()

            goal_set.add((goal.x, goal.y))
            goal_set.add((goal.x + 1, goal.y))
            goal_set.add((goal.x, goal.y + 1))
            goal_set.add((goal.x + 1, goal.y + 1))

        else:
            goal_set = set()
            goal_set.add(goal.port_locs[port][:2])

        came_from = dict()
        g_score = defaultdict(lambda: inf)
        f_score = defaultdict(lambda: inf)
        for loc in extra_set:
            g_score[loc] = 0
            f_score[loc] = goal.dist(loc[:2])

        # TODO jumps moeten alle nodes weergeven en corners detecten

        while not open_set.empty():
            # current = min(open_set, key=lambda x: f_score[x])
            current = open_set.get()[1]
            if current[:2] in goal_set or (
                    current[:2] in self.filled and self.filled[current[:2]][1] == goal.id and self.filled[current[:2]][
                2] == port and self.filled[current[:2]][0] != 0):
                if port not in goal.port_locs:
                    goal.port_locs[port] = came_from[current]
                # else:
                #     print("YUS")
                return self.reconstruct(came_from, current, goal.id, port)

            # open_set.remove(current)

            if (current[0] + 1, current[1]) not in self.filled or self.filled[(current[0] + 1, current[1])][
                                                                  1:] == (goal.id, port) or (
            current[0] + 1, current[1]) in goal_set:
                neighbour = (current[0] + 1, current[1], 1)
                cost = [0, 1, 2, 3][current[2]]
                tentative_score = g_score[current] + cost
                if tentative_score < g_score[neighbour]:
                    came_from[neighbour] = current
                    g_score[neighbour] = tentative_score
                    # f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                    open_set.put((tentative_score + goal.dist(neighbour[:2]),neighbour))
            else:
                if self.filled[(current[0] + 1, current[1])][0] == 2 and (
                        current[0] + 2, current[1]) not in self.filled:
                    neighbour = (current[0] + 2, current[1], 1)
                    intermediate = (current[0] + 1, current[1], 1)
                    cost = [0, 2, 4, 6][current[2]]
                    tentative_score = g_score[current] + cost
                    if tentative_score < g_score[neighbour]:
                        came_from[neighbour] = intermediate
                        came_from[intermediate] = current
                        g_score[neighbour] = tentative_score
                        # f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                        open_set.put((tentative_score + goal.dist(neighbour[:2]), neighbour))

            if (current[0] - 1, current[1]) not in self.filled or self.filled[(current[0] - 1, current[1])][
                                                                  1:] == (goal.id, port) or (
            current[0] - 1, current[1]) in goal_set:
                neighbour = (current[0] - 1, current[1], 1)
                cost = [0, 1, 2, 3][current[2]]
                tentative_score = g_score[current] + cost
                if tentative_score < g_score[neighbour]:
                    came_from[neighbour] = current
                    g_score[neighbour] = tentative_score
                    # f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                    open_set.put((tentative_score + goal.dist(neighbour[:2]),neighbour))
            else:
                if self.filled[(current[0] - 1, current[1])][0] == 2 and (
                        current[0] - 2, current[1]) not in self.filled:
                    neighbour = (current[0] - 2, current[1], 1)
                    intermediate = (current[0] - 1, current[1], 1)
                    cost = [0, 2, 4, 6][current[2]]
                    tentative_score = g_score[current] + cost
                    if tentative_score < g_score[neighbour]:
                        came_from[neighbour] = intermediate
                        came_from[intermediate] = current
                        g_score[neighbour] = tentative_score
                        # f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                        open_set.put((tentative_score + goal.dist(neighbour[:2]), neighbour))

            if (current[0], current[1] + 1) not in self.filled or self.filled[(current[0], current[1] + 1)][
                                                                  1:] == (goal.id, port) or (
            current[0], current[1] + 1) in goal_set:
                neighbour = (current[0], current[1] + 1, 2)
                cost = [0, 2, 1, 3][current[2]]
                tentative_score = g_score[current] + cost
                if tentative_score < g_score[neighbour]:
                    came_from[neighbour] = current
                    g_score[neighbour] = tentative_score
                    # f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                    open_set.put((tentative_score + goal.dist(neighbour[:2]),neighbour))
            else:
                if self.filled[(current[0], current[1] + 1)][0] == 2 and (
                        current[0], current[1] + 2) not in self.filled:
                    neighbour = (current[0], current[1] + 2, 2)
                    intermediate = (current[0], current[1] +1, 2)
                    cost = [0, 4, 2, 6][current[2]]
                    tentative_score = g_score[current] + cost
                    if tentative_score < g_score[neighbour]:
                        came_from[neighbour] = intermediate
                        came_from[intermediate] = current
                        g_score[neighbour] = tentative_score
                        # f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                        open_set.put((tentative_score + goal.dist(neighbour[:2]), neighbour))

            if (current[0], current[1] - 1) not in self.filled or self.filled[(current[0], current[1] - 1)][
                                                                  1:] == (goal.id, port) or (
            current[0], current[1] - 1) in goal_set:
                neighbour = (current[0], current[1] - 1, 2)
                cost = [0, 2, 1, 3][current[2]]
                tentative_score = g_score[current] + cost
                if tentative_score < g_score[neighbour]:
                    came_from[neighbour] = current
                    g_score[neighbour] = tentative_score
                    # f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                    open_set.put((tentative_score + goal.dist(neighbour[:2]),neighbour))
            else:
                if self.filled[(current[0], current[1] - 1)][0] == 2 and (
                        current[0], current[1] - 2) not in self.filled:
                    neighbour = (current[0], current[1] - 2, 2)
                    intermediate = (current[0], current[1] - 1, 2)
                    cost = [0, 4, 2, 6][current[2]]
                    tentative_score = g_score[current] + cost
                    if tentative_score < g_score[neighbour]:
                        came_from[neighbour] = intermediate
                        came_from[intermediate] = current
                        g_score[neighbour] = tentative_score
                        # f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                        open_set.put((tentative_score + goal.dist(neighbour[:2]), neighbour))

    def reconstruct(self, came_from, current, goal, port):
        total_path = [current[:2]]
        self.filled[current[:2]] = (current[2], goal, port)
        while current in came_from:
            current = came_from[current]
            self.filled[current[:2]] = (current[2], goal, port)
            total_path.append(current[:2])
        return total_path[::-1]

    def plot(self):
        fig, ax = plt.subplots()

        xs = []
        ys = []
        ids = []
        for station in self.stations:
            print(station.id, station.x, station.y)
            xs.append(station.x)
            ys.append(station.y)
            ids.append(station.id)

        plt.xlim(min(xs) - 3, max(xs) + 5)
        plt.ylim(min(ys) - 3, max(ys) + 5)
        ax.set_aspect('equal')

        for n, id in enumerate(ids):
            ax.add_patch(patches.Rectangle((xs[n], ys[n]), 2, 2))

        for i, txt in enumerate(ids):
            ax.annotate(txt, (xs[i], ys[i]))

        for road in self.roads:
            xs, ys = zip(*road[3])
            xs = [x + 0.5 for x in xs]
            ys = [y + 0.5 for y in ys]
            ax.plot(xs, ys)

        plt.show()

    def to_json(self):

        lines = []
        for id, route in enumerate(self.roads):
            extended_line = route[3][:]
            for i in range(id):
                if route[3][-1] in self.roads[i][3]:
                    extended_line.extend(self.roads[i][3][self.roads[i][3].index(route[3][-1]) + 1:])
            lines.append(list(route[:3]) + [extended_line])

        station_data = []
        for _ in range(len(self.stations)):
            station_data.append([False] * 8)

        for route in self.roads:
            exit_loc = route[3][1]
            try:
                exit_ind = [
                    (self.stations[route[0]].x, self.stations[route[0]].y + 2),
                    (self.stations[route[0]].x + 1, self.stations[route[0]].y + 2),
                    (self.stations[route[0]].x + 2, self.stations[route[0]].y + 1),
                    (self.stations[route[0]].x + 2, self.stations[route[0]].y),
                    (self.stations[route[0]].x + 1, self.stations[route[0]].y - 1),
                    (self.stations[route[0]].x, self.stations[route[0]].y - 1),
                    (self.stations[route[0]].x - 1, self.stations[route[0]].y),
                    (self.stations[route[0]].x - 1, self.stations[route[0]].y + 1),
                ].index(exit_loc)

                station_data[route[0]][exit_ind] = True
            except:
                print("exit problem..")

            inp_locs = [
                (self.stations[route[2]].x, self.stations[route[2]].y + 2),
                (self.stations[route[2]].x + 1, self.stations[route[2]].y + 2),
                (self.stations[route[2]].x + 2, self.stations[route[2]].y + 1),
                (self.stations[route[2]].x + 2, self.stations[route[2]].y),
                (self.stations[route[2]].x + 1, self.stations[route[2]].y - 1),
                (self.stations[route[2]].x, self.stations[route[2]].y - 1),
                (self.stations[route[2]].x - 1, self.stations[route[2]].y),
                (self.stations[route[2]].x - 1, self.stations[route[2]].y + 1),
            ]

            station_locs = [
                (self.stations[route[2]].x, self.stations[route[2]].y),
                (self.stations[route[2]].x + 1, self.stations[route[2]].y),
                (self.stations[route[2]].x, self.stations[route[2]].y + 1),
                (self.stations[route[2]].x + 1, self.stations[route[2]].y + 1)
            ]

            if route[3][-2] in inp_locs and route[3][-1] in station_locs:
                station_data[route[2]][inp_locs.index(route[3][-2])] = True

            station_data[route[0]][exit_ind] = True
        # pprint(station_data)

        tiles = set()

        for station in self.stations:
            tiles.add(Tile(station.x,station.y, "STATION"))
            tiles.add(Tile(station.x+1,station.y, "STATION"))
            tiles.add(Tile(station.x,station.y+1, "STATION"))
            tiles.add(Tile(station.x+1,station.y+1, "STATION"))


        for id,road in enumerate(self.roads):
            path = road[3]

            for i in range(1, len(path) - 1):
                for j in range(id):
                    if path[i] in self.roads[j][3]:
                        if Tile(path[i][0], path[i][1], "X") in tiles:
                            tiles.remove(Tile(path[i][0], path[i][1], "X"))
                        tiles.add(Tile(path[i][0], path[i][1], "CROSSING"))
                        break
                else:
                    if abs(path[i - 1][0] - path[i + 1][0]) == 2:
                        tiles.add(Tile(path[i][0], path[i][1], "Horizontal"))
                    if abs(path[i - 1][1] - path[i + 1][1]) == 2:
                        tiles.add(Tile(path[i][0], path[i][1], "Vertical"))

                    if path[i - 1][0] == path[i][0] - 1 and path[i + 1][1] == path[i][1] - 1:
                        tiles.add(Tile(path[i][0], path[i][1], "WN"))
                    if path[i - 1][1] == path[i][1] - 1 and path[i + 1][0] == path[i][0] - 1:
                        tiles.add(Tile(path[i][0], path[i][1], "WN"))

                    if path[i - 1][0] == path[i][0] + 1 and path[i + 1][1] == path[i][1] - 1:
                        tiles.add(Tile(path[i][0], path[i][1], "NE"))
                    if path[i - 1][1] == path[i][1] - 1 and path[i + 1][0] == path[i][0] + 1:
                        tiles.add(Tile(path[i][0], path[i][1], "NE"))

                    if path[i - 1][0] == path[i][0] - 1 and path[i + 1][1] == path[i][1] + 1:
                        tiles.add(Tile(path[i][0], path[i][1], "SW"))
                    if path[i - 1][1] == path[i][1] + 1 and path[i + 1][0] == path[i][0] - 1:
                        tiles.add(Tile(path[i][0], path[i][1], "SW"))

                    if path[i - 1][0] == path[i][0] + 1 and path[i + 1][1] == path[i][1] + 1:
                        tiles.add(Tile(path[i][0], path[i][1], "ES"))
                    if path[i - 1][1] == path[i][1] + 1 and path[i + 1][0] == path[i][0] + 1:
                        tiles.add(Tile(path[i][0], path[i][1], "ES"))

            for j in range(id):
                if road[3][-1] in self.roads[j][3]:
                    if Tile(road[3][-1][0], road[3][-1][1], "X") in tiles:
                        tiles.remove(Tile(road[3][-1][0], road[3][-1][1], "X"))
                        if road[3][-2][0] == road[3][-1][0] - 1 :
                            tiles.add(Tile(road[3][-1][0], road[3][-1][1], "T_WEST"))
                        if road[3][-2][0] == road[3][-1][0] + 1 :
                            tiles.add(Tile(road[3][-1][0], road[3][-1][1], "T_EAST"))
                        if road[3][-2][1] == road[3][-1][1] - 1 :
                            tiles.add(Tile(road[3][-1][0], road[3][-1][1], "T_NORTH"))
                        if road[3][-2][1] == road[3][-1][1] + 1 :
                            tiles.add(Tile(road[3][-1][0], road[3][-1][1], "T_SOUTH"))



        min_x,min_y,max_x,max_y = min(tile.x for tile in tiles),min(tile.y for tile in tiles),max(tile.x for tile in tiles),max(tile.y for tile in tiles)

        for x in range(min_x,max_x+1):
            for y in range(min_y,max_y+1):
                if Tile(x, y, "X") not in tiles:
                    noise_a = snoise2(x / 35, y / 35, octaves=3)
                    if noise_a > 0.45:
                        tiles.add(Tile(x, y, choice(["water1", "water_2","water1", "water_2", "water_lily"])))
                    elif noise_a>0.4:
                        tiles.add(Tile(x, y, "water_sand"))

                    noise_a = snoise2(x/10,y/10,octaves=3)
                    if noise_a > 0.4:
                        tiles.add(Tile(x, y, choice(["Decoration1","Decoration2","Decoration3"])))
                    elif noise_a>0.3:
                        tiles.add(Tile(x, y, choice(["Decoration4", "Decoration4", "Decoration5"])))

                    noise_a = snoise2(x / 20, y / 20, octaves=3,base=999)
                    if noise_a > 0.5:
                        tiles.add(Tile(x, y, "Decoration5"))











        data = {
            "stations" : [{"x":station.x,"y":station.y,"stoppers":station_data[i],"type":station.type, "name": station.name} for (i,station) in enumerate(self.stations)],
            "lines" : [{"station_id":line[0], "station_track":line[1],"path":line[3]} for line in lines],
            "tiles" : [{"x":tile.x,"y":tile.y,"type":tile.type} for tile in tiles]
        }

        return json.dumps(data,indent=4)

class Tile:
    def __init__(self, x, y, type):
        self.x = x
        self.y = y
        self.type = type

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __hash__(self):
        return hash((self.x,self.y))


if __name__ == '__main__':
    if len(argv) == 2:
        file = argv[1].strip().strip("\"")
    else:
        file = "test_places.json"
        # file = "visualizer_setup/32.json"


    print(os.getcwd())

    with open(file) as f:
        data = json.load(f)
    pprint(data)

    stations = [Station(id, d["inputs"], d["to"], d["type"], d["name"]) for id, d in enumerate(data)]
    stations = StationGroup.build(stations)
    # stations.plot()
    stations.scale(4)
    # stations.plot()
    print("Stations placed")

    world = World(stations.stations)
    world.build()
    print(world.roads)
    # world.plot()
    outp = world.to_json()

    with open(f"{file}.result.json","w") as f:
        f.write(outp)

# PYTHON_EXECUTABLE=python3 CARGO_INCREMENTAL=1 cargo run -- shunting_yard/incrementer.train